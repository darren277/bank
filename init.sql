-- init.sql

-- 1. Possibly create a superuser role (only if you need it).
-- NOTE: This requires connecting as a superuser (e.g., `-U postgres`).
CREATE ROLE postgres WITH SUPERUSER LOGIN PASSWORD 'mypassword';

-- 2. Alter an existing user to have CREATEDB (optional).
ALTER USER myusername CREATEDB;

-- 3. Create the `bank` database if it doesn't already exist.
CREATE DATABASE bank;

-- Switch context to the `bank` database so subsequent commands run there.
\c bank

-- 4. Create tables
CREATE TABLE IF NOT EXISTS accounts (
    account_number VARCHAR(10) PRIMARY KEY,
    balance NUMERIC(15,2) NOT NULL
);

CREATE TABLE IF NOT EXISTS transactions (
    transaction_id SERIAL PRIMARY KEY,
    account_number VARCHAR(10) REFERENCES accounts(account_number),
    transaction_type CHAR(1),
    amount NUMERIC(15,2),
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 5. Populate initial data
INSERT INTO accounts (account_number, balance) VALUES ('1234567890', 1000.00) 
    ON CONFLICT DO NOTHING;
INSERT INTO accounts (account_number, balance) VALUES ('0987654321', 500.00)
    ON CONFLICT DO NOTHING;

INSERT INTO transactions (account_number, transaction_type, amount)
    VALUES ('1234567890', 'D', 80.00);

-- 6. Test the data
SELECT transaction_id, transaction_type, amount, timestamp
  FROM transactions
 WHERE account_number = '1234567890';
