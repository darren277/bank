#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>

// Function to convert packed decimal to double
double packed_to_double(unsigned char* packed, int length) {
    double result = 0.0;
    double scale = 1.0;
    int i;
    
    // Process all bytes except the last one
    for (i = 0; i < length - 1; i++) {
        result = result * 100 + ((packed[i] >> 4) & 0x0F) * 10 + (packed[i] & 0x0F);
    }
    
    // Process the last byte (contains the sign in the lower 4 bits)
    result = result * 10 + ((packed[length - 1] >> 4) & 0x0F);
    
    // Apply decimal places (2 in this case for money)
    result = result / 100.0;
    
    // Check sign (assuming 0xC for positive and 0xD for negative)
    if ((packed[length - 1] & 0x0F) == 0x0D) {
        result = -result;
    }
    
    return result;
}

void print_odbc_error(SQLHANDLE handle, SQLSMALLINT type) {
    SQLCHAR sqlstate[6];
    SQLCHAR message[SQL_MAX_MESSAGE_LENGTH];
    SQLSMALLINT i = 1;
    SQLINTEGER native_error;
    SQLSMALLINT msg_len;
    
    while (SQLGetDiagRec(type, handle, i, sqlstate, &native_error,
                        message, sizeof(message), &msg_len) == SQL_SUCCESS) {
        fprintf(stderr, "ODBC Error at position %d:\n", i);
        fprintf(stderr, "  SQLSTATE: %s\n", sqlstate);
        fprintf(stderr, "  Native Error: %ld\n", (long)native_error);
        fprintf(stderr, "  Message: %s\n", message);
        i++;
    }
}

// Function to insert a transaction
int insert_transaction(char* account_number, char* trans_type, unsigned char* packed_amount) {
    double amount = packed_to_double(packed_amount, 5);

    fprintf(stderr, "Debug: account_number='%s', trans_type='%s', amount=%.2f\n",
            account_number, trans_type, amount);
    
    if (!account_number || strlen(account_number) == 0) {
        fprintf(stderr, "Error: Invalid account number.\n");
        return -1;
    }

    if (!trans_type || strlen(trans_type) != 1 || (trans_type[0] != 'D' && trans_type[0] != 'W')) {
        fprintf(stderr, "Error: Invalid transaction type.\n");
        return -1;
    }

    if (amount <= 0) {
        fprintf(stderr, "Error: Invalid amount: %.2f\n", amount);
        return -1;
    }
    
    SQLHENV env = SQL_NULL_HANDLE;
    SQLHDBC dbc = SQL_NULL_HANDLE;
    SQLHSTMT stmt = SQL_NULL_HANDLE;
    SQLRETURN ret;
    char query[256];

    fprintf(stderr, "Debug: Initializing ODBC environment...\n");

    // Initialize ODBC environment
    ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error: Failed to allocate ODBC environment handle.\n");
        print_odbc_error(SQL_HANDLE_ENV, env);
        return -1;
    }

    fprintf(stderr, "Debug: Setting ODBC version...\n");
    
    ret = SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error: Failed to set ODBC version.\n");
        print_odbc_error(SQL_HANDLE_ENV, env);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -1;
    }

    fprintf(stderr, "Debug: Allocating connection handle...\n");
    
    // Allocate connection handle
    ret = SQLAllocHandle(SQL_HANDLE_DBC, env, &dbc);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error: Failed to allocate ODBC connection handle.\n");
        print_odbc_error(SQL_HANDLE_ENV, env);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -1;
    }

    fprintf(stderr, "Debug: Attempting database connection...\n");

    // Connect to data source
    ret = SQLConnect(dbc, 
                    (SQLCHAR*)"BankingDB", SQL_NTS,
                    (SQLCHAR*)"myusername", SQL_NTS,
                    (SQLCHAR*)"mypassword", SQL_NTS);
                    
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error: Failed to connect to the database.\n");
        print_odbc_error(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -1;
    }

    fprintf(stderr, "Debug: Connected to database. Allocating statement handle...\n");
    
    // Allocate statement handle
    ret = SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error: Failed to allocate statement handle.\n");
        print_odbc_error(SQL_HANDLE_DBC, dbc);
        SQLDisconnect(dbc);
        SQLFreeHandle(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -1;
    }

    fprintf(stderr, "Debug: Preparing SQL query...\n");
    
    snprintf(query, sizeof(query),
             "INSERT INTO transactions (account_number, transaction_type, amount) "
             "VALUES ('%s', '%s', %lf);",
             account_number, trans_type, amount);

    fprintf(stderr, "Debug: Executing SQL query: %s\n", query);
    
    // Execute SQL statement
    ret = SQLExecDirect(stmt, (SQLCHAR*)query, SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error: Failed to execute SQL query.\n");
        print_odbc_error(SQL_HANDLE_STMT, stmt);
        SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        SQLDisconnect(dbc);
        SQLFreeHandle(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -1;
    }

    fprintf(stderr, "Debug: Query executed successfully. Cleaning up...\n");

    // Clean up
    SQLFreeHandle(SQL_HANDLE_STMT, stmt);
    SQLDisconnect(dbc);
    SQLFreeHandle(SQL_HANDLE_DBC, dbc);
    SQLFreeHandle(SQL_HANDLE_ENV, env);

    return 0;
}

// Function to retrieve transactions
int get_transactions(const char* account_number, char* output, int max_len) {
    SQLHENV env;
    SQLHDBC dbc;
    SQLHSTMT stmt;
    SQLRETURN ret;
    char query[256];
    char buffer[256];
    int len = 0;

    fprintf(stderr, "Debug: account_number='%s', max_len=%d\n", account_number ? account_number : "(null)", max_len);
    fprintf(stderr, "Debug: Initial output buffer='%s'\n", output);

    fprintf(stderr, "Debug: Received account_number='%s'\n", account_number ? account_number : "(null)");
    fprintf(stderr, "Debug: Max length for output=%d\n", max_len);
    fprintf(stderr, "Debug: Initial output buffer='%s'\n", output);

    fprintf(stderr, "Debug: account_number pointer=%p\n", account_number);
    if (account_number) {
        fprintf(stderr, "Debug: Received account_number='%s', length=%zu\n", account_number, strlen(account_number));
    } else {
        fprintf(stderr, "Error: account_number is NULL\n");
    }

    // Check account_number validity
    if (!account_number || strlen(account_number) == 0) {
        fprintf(stderr, "Error: Invalid account_number. Exiting.\n");
        return -1;
    }

    fprintf(stderr, "Debug: Raw account_number memory: ");
    for (int i = 0; i < 11; i++) {  // Adjust length as needed
        fprintf(stderr, "%02x ", ((unsigned char*)account_number)[i]);
    }
    fprintf(stderr, "\n");

    if (!account_number || strlen(account_number) > 10) {  // Max length check
        fprintf(stderr, "Error: Invalid account_number length\n");
        return -1;
    }

    // Initialize ODBC environment
    SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
    SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);

    // Allocate connection handle
    SQLAllocHandle(SQL_HANDLE_DBC, env, &dbc);

    fprintf(stderr, "Debug: SQL query='%s'\n", query);

    // Connect to data source
    ret = SQLConnect(dbc, (SQLCHAR*)"BankingDB", SQL_NTS,
                     (SQLCHAR*)"myusername", SQL_NTS,
                     (SQLCHAR*)"mypassword", SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error connecting to database.\n");

        // Handle connection error
        SQLFreeHandle(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -1;
    }

    // Allocate statement handle
    SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);

    // Prepare SQL statement
    snprintf(query, sizeof(query),
             "SELECT transaction_id, transaction_type, amount, timestamp FROM transactions WHERE account_number = '%s';",
             account_number);

    // Execute SQL statement
    fprintf(stderr, "Debug: Executing SQL query: %s\n", query);

    ret = SQLExecDirect(stmt, (SQLCHAR*)query, SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        fprintf(stderr, "Error executing query. SQLRETURN=%d\n", ret);

        // Handle execution error
        SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        SQLDisconnect(dbc);
        SQLFreeHandle(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -2;
    }

    // Initialize output buffer with empty string
    output[0] = '\0';
    len = 0;

    // Fetch and concatenate results
    fprintf(stderr, "Debug: Fetching results...\n");

    while ((ret = SQLFetch(stmt)) != SQL_NO_DATA) {
        if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO) {
            fprintf(stderr, "Debug: Fetching row...\n");

            SQLINTEGER transaction_id;
            char transaction_type[2];
            double amount;
            char timestamp[30];
            SQLLEN indicator;
            
            // Get data with proper length indicators
            SQLGetData(stmt, 1, SQL_C_SLONG, &transaction_id, 0, &indicator);
            SQLGetData(stmt, 2, SQL_C_CHAR, transaction_type, sizeof(transaction_type), &indicator);
            SQLGetData(stmt, 3, SQL_C_DOUBLE, &amount, 0, &indicator);
            SQLGetData(stmt, 4, SQL_C_CHAR, timestamp, sizeof(timestamp), &indicator);

            // Format the output
            int written = snprintf(buffer, sizeof(buffer), 
                "ID: %d, Type: %s, Amount: %.2f, Time: %s\n",
                transaction_id, transaction_type, amount, timestamp);

            // Check if we have enough space left
            if (len + written >= max_len - 1) {
                fprintf(stderr, "Warning: Output buffer full\n");
                break;
            }

            // Copy to output buffer
            memcpy(output + len, buffer, written);
            len += written;

            // Ensure null termination
            output[len] = '\0';

            fprintf(stderr, "Debug: Current buffer length: %d\n", len);
            fprintf(stderr, "Fetched Row: ID=%d, Type='%s', Amount=%.2f, Time='%s'\n",
                    transaction_id, transaction_type, amount, timestamp);
        }
    }

    // Clean up
    SQLFreeHandle(SQL_HANDLE_STMT, stmt);
    SQLDisconnect(dbc);
    SQLFreeHandle(SQL_HANDLE_DBC, dbc);
    SQLFreeHandle(SQL_HANDLE_ENV, env);

    return 0;
}
