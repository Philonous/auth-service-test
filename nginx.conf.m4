# use m4 to configure AUTH_SERVICE and UPSTREAM and PORT
#
# Example:
# m4 -DAUTH_SERVICE=localhost:3000 \
#    -DUPSTREAM=localhost:4000 \
#    -INSTANCE=myinstance \
#    -ACCESS_LOG=/var/log/nginx/access.log
#    -DPORT=80 \
#    nginx.conf \
#    > nginx.conf.out
#
#    add -DFOREGROUND to run nginx in foreground



worker_processes 1;

error_log /tmp/nginx.log debug;

events {
    worker_connections 1024;
}

http {
    include       /etc/nginx/mime.types;
    default_type  application/octet-stream;
    sendfile        on;
    keepalive_timeout  65;
    ifdef(`ACCESS_LOG', `access_log ACCESS_LOG;')
    server {
        listen PORT;
        server_name auth-service;
        rewrite_log on;
        location / {
            auth_request /auth;
            auth_request_set $user $upstream_http_x_user;
            # The variable $user now contains the username when the check was
            # successful


            # Redirect to internal path depending on X-instance
            if ($http_x_instance = "de305d54-75b4-431b-adb2-eb6b9e546014") {
                rewrite ^/(.*)$ /testinstance/$1 last;
            }
            return 404;
        }
        location = /auth {
                internal;
                set $token $cookie_token;
                if ($http_x_token) {
                  set $token $http_x_token;
                }
                proxy_pass http://AUTH_SERVICE/check-token/$http_x_instance/$token/;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
        }

        # Every instance needs a location block. Use it to proxy_pass or
        # whatever :)
        location /testinstance/ {
                internal;
                alias /www/;
        }


        # This part is only necessary if the client doesn't contact the central
        # auth service (e.g. for SSO)
        location = /login {
                proxy_pass http://AUTH_SERVICE/login/;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token";
        }

        location = /logout {
                proxy_pass http://AUTH_SERVICE/logout/$cookie_token;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token";
        }


    }
 }
