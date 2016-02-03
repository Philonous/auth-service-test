#!/bin/sh

set -e

if [[ -n $DEBUG ]]; then
  set -x
  CURL="curl -v"
else
  CURL="curl --silent"
fi


# You need to have jq in path

export DB_HOST=database
export DB_USER=lambdatrade
export DB_DATABASE=lambdatrade

HOST=localhost:3000
DOCKER_HOST=localhost:8000
API=$DOCKER_HOST/api

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

nginx_build_conf () {
    m4 -DAUTH_SERVICE=authservice:3000 \
       -DUPSTREAM=localhost:4000 \
       -INSTANCE=myinstance \
       -DPORT=8000 \
       -DACCESS_LOG=off \
       -DERROR_LOG=/tmp/nginx.log \
       -DFOREGROUND \
       -DUPSTREAM_PORT=4000 \
       -DACCES_LOG=off \
       -DFoo=bar \
       nginx.conf.m4 \
       > nginx.conf
}

nginx_reload_conf() {
    nginx_build_conf
    docker restart authservice_nginx_1
    nginx_logs

}

start_nginx() {
    nginx_conf
    sudo docker-compose up
    }

enter_nginx() {
    docker exec -it authservice_nginx_1 /bin/bash
}


login() {
    $CURL -H "Content-Type: application/json" \
         -d "{ \"user\": \"$USER\", \"password\": \"$PASSWORD\" }" \
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
    RES=$($CURL http://$HOST/check-token/$INSTANCE/$TOKEN)
    echo $RES
}


docker_setup () {
    docker exec -it authservice_auth-service_1 /testsetup.sh > /dev/null
}

docker_test() {
    echo "Running setup"
    docker_setup
    echo "Trying to log in"
    RES="$(login "$API")"
    TOKEN=$(echo "$RES" | jq -r '.token.token')
    echo "Got token: $TOKEN"
    if [[ -z "$TOKEN" ]]; then
       echo "Could not login"
       exit 1
    fi
    echo "Checking token"
    RES=$($CURL --write-out %{http_code}\
                -H "X-Token: $TOKEN" \
                http://$API/check-token)
    if [[ $RES != "204" ]]; then
      echo "Could not verify tokenm got $RES"
      exit 1
    fi
    echo "Trying to reach upstream resource"
    RES=$($CURL -H "X-Instance: $INSTANCE" \
                -H "X-Token: $TOKEN" \
                http://$API/index.htm)
    if [[ $RES != "it works!" ]]; then
      echo "Could not retrieve index.html, got $RES insted"
      exit 1
    fi
    echo "Upstream says:" $RES

    echo "Trying to reach upstream resource without token"
    RES=$($CURL -o /dev/null \
                -w %{http_code} \
                -H "X-Instance: $INSTANCE" \
                http://$API/index.htm)
    if [[ $RES != "403" ]]; then
      echo "Did not get authorization denied, instead got $RES"
      exit 1
    fi

    echo "Trying to reach upstream resource with bogus token"
    RES=$($CURL -o /dev/null \
                -w %{http_code} \
                -H "X-Instance: $INSTANCE" \
                -H "X-Token: bogus$TOKEN" \
                http://$API/index.htm)
    if [[ $RES != "403" ]]; then
      echo "Did not get authorization denied, instead got $RES"
      exit 1
    fi


    echo "Trying /auth.html"

    RES=$($CURL http://$DOCKER_HOST/auth.html)
    if [[ $RES != "Authenticate!" ]]; then
      echo "Did not get auth.html, instead got $RES"
      exit 1
    else
      echo "Got /auth.html"
    fi

    echo "Trying /auth.html with token"
    RES=$($CURL -o /dev/null \
                -w %{http_code} \
                --cookie "token=$TOKEN" \
                http://$DOCKER_HOST/auth.html)
    if [[ $RES != "303" ]]; then
      echo "Did not get redirect, instead got $RES"
      exit 1
    else
      echo "Got redirect"
    fi



    # RES=$($CURL -v \
    #            --cookie "TOKEN=$TOKEN" http://$DOCKER_HOST/)
    # echo $RES
    # nginx_logs
}



docker_rebuild() {
    set -e
    nginx_build_conf
    docker-compose stop
    docker-compose rm -f
    docker-compose build
    docker-compose up
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
    docker)
        docker_rebuild
        ;;
    login)
        login $DOCKER_HOST
        ;;
    nginx_rebuild_conf)
        nginx_build_conf
        ;;
    nginx_logs)
        nginx_logs
        ;;
    nginx_enter)
        enter_nginx
        ;;
    *)
        echo "usage: test.sh (run|localsetup|localtest|dockertest|login|nginx)"
        exit 1
        ;;
esac
