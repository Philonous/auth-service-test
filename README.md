auth-service
============

An authentication micro-service written in Haskell.

Get started
-----------

The instructions below assumes that you are using Docker Compose, that the API
you want to protect is called "server", that you are running a PostgreSQL
instance called "database", and that your PostgreSQL database and user name is
"lambdatrade".

Add this repository as a submodule, and put the following in your
docker-compose.yml configuration:

    auth:
      build: auth-service
      environment:
        DB_PASSWORD: ...
      links:
      - database
    auth_web:
      build: auth-service/web
      links:
      - auth
      - server

At this point you can proxy your API requests to http://auth_web:3000/, like so:

    location /api/ {
        proxy_pass http://auth_web:3000/;
    }

Please see auth-service.config for the configuration options.

Managing users
--------------

Users can be added like so:

    docker exec -it app_auth_1 auth-service adduser "My Name" my_password my_email@example.com

TODO: Document change password.

Other changes can be done with SQL directly.

API
---

Authentication is done like this:

    POST /api/login
    { "user": "my_email@example.com", "password": "my_password" }

The above requests sets a cookie, "token". It also sets a header, "X-Token", in
case you would rather get the token like that. As a third option, it includes
the token in the JSON body like so:

    {"token":"the_token"}

You will need to set this token in future requests, either as a header or as a
cookie.

Logging out is done like this:

    POST /api/logout