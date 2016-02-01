#!/bin/sh

until nc -z authservice 80; do
    echo "Waiting for auth-service..."
    sleep 1
done

echo "Authentication web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
