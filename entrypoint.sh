#!/bin/bash

# Start fcgiwrap
service fcgiwrap start

# Start Nginx
nginx -g "daemon off;"
