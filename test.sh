#!/bin/sh

set -e

# You need to have jq in path

export DB_HOST=database
export DB_USER=lambdatrade
export DB_DATABASE=lambdatrade

HOST=localhost:3000
DOCKER_HOST=localhost:8000

# Make sure that this matches with setuptest.sh when running against docker
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


# setup for _local_ testing
local_setup () {
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
    cabal run -- adduser "$USER" "$PASSWORD" "$NAME"

    db <<EOF
INSERT INTO "instance" VALUES ('${INSTANCE}', 'instance1');
INSERT INTO "user_instance" VALUES ( (SELECT "uuid" FROM "user" WHERE "email" = '${USER}')
                                   , (SELECT "uuid" FROM "instance" WHERE "name"='instance1'));
EOF

}

nginx_logs () {
    docker exec authservice_nginx_1 cat /tmp/nginx.log
}

nginx_conf() {
    m4 -DAUTH_SERVICE=authservice:3000 \
       -DUPSTREAM=localhost:4000 \
       -INSTANCE=myinstance \
       -DPORT=8000 \
       -DACCESS_LOG=off \
       -DFOREGROUND \
       -DINSTANCE=de305d54-75b4-431b-adb2-eb6b9e546014\
       nginx.conf.m4 \
      | tee nginx.conf

    docker restart authservice_nginx_1
    docker exec authservice_nginx_1 cat /tmp/nginx.log

}

start_nginx() {
    nginx_conf
    sudo docker-compose up
    }

login() {
curl -H "Content-Type: application/json" -X POST -d "{ \"user\": \"$USER\", \"password\": \"$PASSWORD\" }" \
               http://$1/login
}

# run _local_ test
runtest() {
    RES="$(login "$HOST")"
    echo $RES
    TOKEN=$(echo "$RES" | jq -r '.token.token')
    if [[ -z "$TOKEN" ]]; then
       echo "Could not login"
       exit 1
    fi
    RES=$(curl http://$HOST/check-token/$INSTANCE/$TOKEN)
    echo $RES
}


setup () {
    docker exec -it authservice_authservice_1 /testsetup.sh
}

docker_test() {
    setup
    nginx_conf
    RES="$(login "$DOCKER_HOST")"
    echo $RES
    TOKEN=$(echo "$RES" | jq -r '.token.token')
    if [[ -z "$TOKEN" ]]; then
       echo "Could not login"
       exit 1
    fi
    # RES=$(curl -v\
    #            -H "X-Token: $TOKEN" \
    #            -H "X-Instance: $INSTANCE" \
    #            http://$DOCKER_HOST/)
    # echo $RES
    RES=$(curl -v \
               -H "X-Instance: $INSTANCE" \
               -H "X-Token: $TOKEN" \
               http://$DOCKER_HOST/)
    echo $RES
    # RES=$(curl -v \
    #            --cookie "TOKEN=$TOKEN" http://$DOCKER_HOST/)
    # echo $RES
    # nginx_logs

}

case $1 in
    run)
        start
        ;;
    localsetup)
        local_setup
        ;;
    localtest)
        runtest
        ;;
    dockertest)
        docker_test
        ;;
    login)
        login $DOCKER_HOST
        ;;
    nginx)
        nginx_conf
        ;;
    *)
        echo "usage: test.sh (run|localsetup|localtest|dockertest|login|nginx)"
        exit 1
        ;;
esac
