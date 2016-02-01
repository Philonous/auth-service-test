#!/bin/sh

# This is supposed to be run inside the docker container
# See docker-compose.yaml (should volume it in)
#
# supposed to be docker-exec'ed by test.sh before testing the service

set -x

export DB_HOST=database
export DB_USER=lambdatrade
export DB_DATABASE=lambdatrade

USER=no@spam.please
PASSWORD=pwd123
NAME="John Doe"
INSTANCE="de305d54-75b4-431b-adb2-eb6b9e546014"

db () {
    psql -h $DB_HOST -U $DB_USER $DB_DATABASE
}

start () {
    cabal run -- run
}

setup () {
    set -x
    db <<EOF
DROP SCHEMA public CASCADE;
CREATE SCHEMA public
  AUTHORIZATION postgres;

GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;
COMMENT ON SCHEMA public
  IS 'standard public schema';
EOF
    auth-service adduser "$USER" "$PASSWORD" "$NAME"

    db <<EOF
INSERT INTO "instance" VALUES ('${INSTANCE}', 'instance1');
INSERT INTO "user_instance" VALUES ( (SELECT "uuid" FROM "user" WHERE "email" = '${USER}')
                                   , (SELECT "uuid" FROM "instance" WHERE "name"='instance1'));
EOF

}

setup
