include .env

LODBC_INTERFACE=/usr/lib/arm-linux-gnueabihf
#CGI_PATH=/usr/lib/cgi-bin
CGI_PATH="."

MORE_FLAGS=-std=default -debug -W -Wall -fimplicit-init -fstatic-call -lstdc++

build:
	cobc -x bank.cob
	cobc -x -free -o insert_transaction insert_transaction.cob
	cobc -x -free -o get_transactions get_transactions.cob

build-odbc:
	gcc -shared -o libodbc_interface.so odbc_interface.c -lodbc -fPIC
	cobc -x -free -o insert_transaction_odbc insert_transaction_odbc.cob ./libodbc_interface.so -L$(LODBC_INTERFACE) -lodbc
	cobc -x -free -o get_transactions_odbc get_transactions_odbc.cob ./libodbc_interface.so -L$(LODBC_INTERFACE) -lodbc

build-rest-api:
	cobc -x -free -o $(CGI_PATH)/interest_api.cgi interest_api.cob
	cobc -x -free -o $(CGI_PATH)/get_transactions_api.cgi get_transactions_api.cob
	cobc -x -free -o $(CGI_PATH)/insert_transaction_api.cgi insert_transaction_api.cob

build-odbc-rest-api:
	gcc -shared -o libodbc_interface.so odbc_interface.c -lodbc -fPIC
	DEBUG_MODE=$(DEBUG_MODE) cobc -x $(MORE_FLAGS) -free -o $(CGI_PATH)/insert_transaction_odbc_api.cgi insert_transaction_odbc_api.cob ./libodbc_interface.so -L$(LODBC_INTERFACE) -lodbc

clean:
	rm -f bank insert_transaction get_transactions insert_transaction_odbc get_transactions_odbc *.so *.o output.txt *.cgi psql_result.tmp

run:
	./bank
	./insert_transaction
	./get_transactions

run-odbc:
	COB_PRE_LOAD=./libodbc_interface.so ./insert_transaction_odbc
	COB_PRE_LOAD=./libodbc_interface.so ./get_transactions_odbc

run-rest-api:
	touch psql_result.tmp
	REQUEST_METHOD="GET" QUERY_STRING="principal=1000&rate=0.05&time=2&account=1234567890" $(CGI_PATH)/interest_api.cgi
	REQUEST_METHOD="GET" QUERY_STRING="account=1234567890" $(CGI_PATH)/get_transactions_api.cgi
	REQUEST_METHOD="GET" QUERY_STRING="amount=1000&transaction_type=D&account=1234567890" $(CGI_PATH)/insert_transaction_api.cgi

run-test-insert-transaction-api:
	touch psql_result.tmp
	REQUEST_METHOD="GET" QUERY_STRING="amount=1000&transaction_type=D&account=1234567890" $(CGI_PATH)/insert_transaction_api.cgi

run-test-insert-transaction-odbc-api:
	touch psql_result.tmp
	DEBUG_MODE=$(DEBUG_MODE) COB_PRE_LOAD=./libodbc_interface.so REQUEST_METHOD="GET" QUERY_STRING="amount=1000&transaction_type=D&account=1234567890" $(CGI_PATH)/insert_transaction_odbc_api.cgi


# run with `sudo`
db-config:
	cp ./config/odbc.ini /etc/odbc.ini
	cp ./config/odbc.ini ~/.odbc.ini
	cp ./config/odbcinst.ini /etc/odbcinst.ini

test-odbc:
	odbcinst -q -s
	isql -v BankingDB $(PG_USER) $(PG_PASS)

# REST API CRUD TESTS

# Recording a Transaction via the API
# This request calculates interest and records it as a deposit ('D') in the transactions table.

# curl "http://localhost/cgi-bin/interest_api.cgi?principal=1000&rate=0.05&time=2&account=1234567890"

# Retrieving Transaction History via the API

# curl "http://localhost/cgi-bin/get_transactions_api.cgi?account=1234567890"
