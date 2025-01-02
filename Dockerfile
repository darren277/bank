# Base image
FROM debian:bullseye

ARG CGI_PORT
ARG PG_HOST
ARG PG_PORT
ARG PG_USER
ARG PG_PASS
ARG PG_DB

ENV CGI_PORT=$CGI_PORT
ENV PG_HOST=$PG_HOST
ENV PG_PORT=$PG_PORT
ENV PG_USER=$PG_USER
ENV PG_PASS=$PG_PASS
ENV PG_DB=$PG_DB

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV CGI_PATH=/usr/local/cgi-bin
ENV LD_LIBRARY_PATH=/usr/local/cgi-bin
ENV GNU_COBOL_VERSION=3.1.2

# Install necessary packages
RUN apt-get update && apt-get install -y \
    nginx \
    fcgiwrap \
    gcc \
    unixodbc \
    unixodbc-dev \
    odbc-postgresql \
    && apt-get clean

# Install dependencies for building GNU COBOL
RUN apt-get update && apt-get install -y \
    wget \
    build-essential \
    libdb-dev \
    libgmp-dev \
    libncurses-dev \
    && apt-get clean

# Download and compile GNU COBOL
RUN wget https://ftp.gnu.org/gnu/gnucobol/gnucobol-$GNU_COBOL_VERSION.tar.xz \
    && tar -xvf gnucobol-$GNU_COBOL_VERSION.tar.xz \
    && cd gnucobol-$GNU_COBOL_VERSION \
    && ./configure && make && make install \
    && ldconfig \
    && cd .. && rm -rf gnucobol-$GNU_COBOL_VERSION gnucobol-$GNU_COBOL_VERSION.tar.xz

# Configure Nginx
RUN mkdir -p /etc/nginx/sites-available /etc/nginx/sites-enabled
COPY cobol_app.conf /etc/nginx/sites-available/
RUN ln -s /etc/nginx/sites-available/cobol_app.conf /etc/nginx/sites-enabled/

# sed replace ${CGI_PORT} with the actual port number
RUN sed -i "s/\${CGI_PORT}/${CGI_PORT}/g" /etc/nginx/sites-available/cobol_app.conf

# Add the CGI scripts
RUN mkdir -p $CGI_PATH
COPY insert_transaction_odbc_api.cgi $CGI_PATH/
# TODO: COPY get_transactions_odbc_api.cgi $CGI_PATH/
RUN chmod +x $CGI_PATH/*.cgi

# Add the shared library
COPY libodbc_interface.so $CGI_PATH/
ENV LD_LIBRARY_PATH=$CGI_PATH:$LD_LIBRARY_PATH

# Expose port for CGI
EXPOSE $CGI_PORT

# Set up entrypoint script
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Start services
CMD ["/entrypoint.sh"]
