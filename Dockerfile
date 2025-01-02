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

# envsubst: command not found
RUN apt-get update && apt-get install -y gettext-base

# Configure Nginx
RUN mkdir -p /etc/nginx/sites-available /etc/nginx/sites-enabled
COPY cobol_app.conf /etc/nginx/sites-available/cobol_app.template.conf
RUN touch /etc/nginx/sites-available/cobol_app.conf
RUN ln -s /etc/nginx/sites-available/cobol_app.conf /etc/nginx/sites-enabled/

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

ARG GNU_LIB
ENV GNU_LIB=$GNU_LIB

ARG INTERNAL_PG_PORT
ENV INTERNAL_PG_PORT=$INTERNAL_PG_PORT

ARG DEBUG_MODE
ENV DEBUG_MODE=$DEBUG_MODE

# Set up entrypoint script
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Start services
CMD ["/entrypoint.sh"]
#CMD tail -f /dev/null
