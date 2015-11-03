#!/bin/sh

until nc -z database 5432; do
    echo "Waiting for PostgreSQL..."
    sleep 1
done

echo "setting up test user"

export AUTH_SERVICE_DB_DATABASE=postgres
export AUTH_SERVICE_DB_HOST=database
export AUTH_SERVICE_DB_USER=postgres

auth-service adduser usr1 pwd no@spam.please

echo "Lambdatrade is running!"

exec auth-service run 1>&2
