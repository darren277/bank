#!/bin/bash

echo "DEBUG: CGI_PORT=$CGI_PORT, DEBUG_MODE=$DEBUG_MODE"

envsubst '$CGI_PORT $DEBUG_MODE' < /etc/nginx/sites-available/cobol_app.template.conf > /etc/nginx/sites-available/cobol_app.conf

cat <<EOF > /etc/odbc.ini
[BankingDB]
Driver=PostgreSQL
Description=PostgreSQL ODBC driver
Servername=${PG_HOST}
Port=${INTERNAL_PG_PORT}
Database=${PG_DB}
Username=${PG_USER}
Password=${PG_PASS}
EOF

cat <<EOF > /etc/odbcinst.ini
[PostgreSQL]
Description=PostgreSQL ODBC driver
Driver=/usr/lib/${GNU_LIB}/odbc/psqlodbca.so
EOF

# Optionally show what we wrote:
echo "===== /etc/odbc.ini ====="
cat /etc/odbc.ini

echo "===== /etc/odbcinst.ini ====="
cat /etc/odbcinst.ini


# Start fcgiwrap
service fcgiwrap start

# Start Nginx
nginx -g "daemon off;"
