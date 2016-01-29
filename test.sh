#!/bin/sh

# You need to have jq in path

export DB_HOST=
export DB_USER=
export DB_DATABASE=auth-service

HOST=localhost:3000
USER=no@spam.please
PASSWORD=pwd123
NAME="John Doe"
INSTANCE="de305d54-75b4-431b-adb2-eb6b9e546014"

start () {
    cabal run -- run
}

setup () {
    set -x
    psql $DB_DATABASE <<EOF
DROP SCHEMA public CASCADE;
CREATE SCHEMA public
  AUTHORIZATION postgres;

GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;
COMMENT ON SCHEMA public
  IS 'standard public schema';
EOF

    cabal run -- adduser "$USER" "$PASSWORD" "$NAME"

    psql $DB_DATABASE <<EOF
INSERT INTO "instance" VALUES ('${INSTANCE}', 'instance1');
INSERT INTO "user_instance" VALUES ( (SELECT "uuid" FROM "user" WHERE "email" = '${USER}')
                                   , (SELECT "uuid" FROM "instance" WHERE "name"='instance1'));
EOF

}

login() {
curl -H "Content-Type: application/json" -X POST -d "{ \"user\": \"$USER\", \"password\": \"$PASSWORD\" }" \
               http://$HOST/login
}

test () {
    set -e
    RES="$(login) "
    TOKEN=$(echo "$RES" | jq -r '.token.token')
    if [[ -z "$TOKEN" ]]; then
       echo "Could not login"
       exit 1
    fi
    RES=$(curl http://$HOST/check-token/$INSTANCE/$TOKEN)
    echo $RES

}

case $1 in
  run)
    start
    ;;
  setup)
    setup
    ;;
  test)
    test
    ;;
  login)
    login
    ;;
  *)
    echo "usage: test.sh (run|setup|test)"
    exit 1
    ;;
esac
