#!/bin/sh

until nc -z auth 3000; do
    echo "Waiting for auth-service..."
    sleep 1
done

echo "Authentication web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
