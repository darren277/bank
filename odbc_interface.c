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

    // Initialize ODBC environment
    SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
    SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (void*)SQL_OV_ODBC3, 0);

    // Allocate connection handle
    SQLAllocHandle(SQL_HANDLE_DBC, env, &dbc);

    // Connect to data source
    ret = SQLConnect(dbc, (SQLCHAR*)"BankingDB", SQL_NTS,
                     (SQLCHAR*)"myusername", SQL_NTS,
                     (SQLCHAR*)"mypassword", SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
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
    ret = SQLExecDirect(stmt, (SQLCHAR*)query, SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) {
        // Handle execution error
        SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        SQLDisconnect(dbc);
        SQLFreeHandle(SQL_HANDLE_DBC, dbc);
        SQLFreeHandle(SQL_HANDLE_ENV, env);
        return -2;
    }

    // Fetch and concatenate results
    while ((ret = SQLFetch(stmt)) != SQL_NO_DATA) {
        if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO) {
            SQLINTEGER transaction_id;
            char transaction_type[2];
            double amount;
            char timestamp[30];

            SQLGetData(stmt, 1, SQL_C_SLONG, &transaction_id, 0, NULL);
            SQLGetData(stmt, 2, SQL_C_CHAR, transaction_type, sizeof(transaction_type), NULL);
            SQLGetData(stmt, 3, SQL_C_DOUBLE, &amount, 0, NULL);
            SQLGetData(stmt, 4, SQL_C_CHAR, timestamp, sizeof(timestamp), NULL);

            // Format the output
            snprintf(buffer, sizeof(buffer), "ID: %d, Type: %s, Amount: %.2lf, Time: %s\n",
                     transaction_id, transaction_type, amount, timestamp);

            // Append to output
            strncat(output, buffer, max_len - len - 1);
            len += strlen(buffer);

            if (len >= max_len - 1) {
                break;
            }
        }
    }

    // Clean up
    SQLFreeHandle(SQL_HANDLE_STMT, stmt);
    SQLDisconnect(dbc);
    SQLFreeHandle(SQL_HANDLE_DBC, dbc);
    SQLFreeHandle(SQL_HANDLE_ENV, env);

    return 0;
}
