# bank

## Docker Usage

Compile: `make build-odbc-rest-api`.

Build: `docker build -t cobol-cgi-app .`.
Run: `docker run -d -p 8080:80 cobol-cgi-app`.

### Test

Insert transaction: `curl "http://localhost:8080/api/insert_transaction?amount=1000&transaction_type=D&account=1234567890"`
Get balance: `curl "http://localhost:8080/api/get_balance?account=1234567890"`

## Database Initialization

If necessary, enter the postgres container first: `docker exec -it postgresql psql -U myusername`.
Then: `CREATE ROLE postgres WITH SUPERUSER LOGIN PASSWORD 'mypassword';`
Then: Run each of the following (just the query strings).

```shell
$ sudo -u postgres psql -c "ALTER USER myusername CREATEDB;"
==> ALTER ROLE

$ PGPASSWORD=mypassword psql -U myusername -d postgres -c "CREATE DATABASE bank;"
==> CREATE DATABASE

$ PGPASSWORD=mypassword psql -U myusername -d bank -c "CREATE TABLE IF NOT EXISTS accounts (account_number VARCHAR(10) PRIMARY KEY, balance NUMERIC(15,2) NOT NULL);"
==> CREATE TABLE

$ PGPASSWORD=mypassword psql -U myusername -d bank -c "CREATE TABLE IF NOT EXISTS transactions (transaction_id SERIAL PRIMARY KEY, account_number VARCHAR(10) REFERENCES accounts(account_number), transaction_type CHAR(1), amount NUMERIC(15,2), timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP);"
==> CREATE TABLE

-- Transaction type: 'D' for deposit, 'W' for withdrawal.

$ PGPASSWORD=mypassword psql -U myusername -d bank -c "INSERT INTO accounts (account_number, balance) VALUES ('1234567890', 1000.00);"
$ PGPASSWORD=mypassword psql -U myusername -d bank -c "INSERT INTO accounts (account_number, balance) VALUES ('0987654321', 500.00);"

$ PGPASSWORD=mypassword psql -U myusername -d bank -c "INSERT INTO transactions (account_number, transaction_type, amount) VALUES ('1234567890', 'D', 80.00);"

$ PGPASSWORD=mypassword psql -U myusername -d bank -c "SELECT transaction_id, transaction_type, amount, timestamp FROM transactions WHERE account_number = '1234567890';" -t -A

```

export ODBCINI=/etc/odbc.ini
export ODBCSYSINI=/etc

sudo apt install unixodbc unixodbc-dev
sudo apt-get install odbc-postgresql

sudo find /usr/lib -name "psqlodbc.so" 2>/dev/null
sudo find /usr/lib -name "libodbcpsqlS.so" 2>/dev/null

dpkg -L odbc-postgresql
dpkg -L unixodbc-dev

ls -l /usr/lib/arm-linux-gnueabihf/libodbc.so

sudo find /usr -name "libodbc_interface.so" 2>/dev/null

## Issues

### SQL Injection

...
