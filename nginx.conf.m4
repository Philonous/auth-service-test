# use m4 to configure
#
# Example:
# m4 -DAUTH_SERVICE=localhost:3000 \
#    -DUPSTREAM_PORT=4000 \
#    -DACCESS_LOG=/var/log/nginx/access.log
#    -DERROR_LOG=/var/log/nginx/error.log
#    -DPORT=80 \
#    nginx.conf.m4 \
#    > nginx.conf

worker_processes 1;

ifdef(`ERROR_LOG', `error_log ERROR_LOG;')

events {
    worker_connections 1024;
}

http {
    include /etc/nginx/mime.types;
    default_type application/octet-stream;
    sendfile on;
    keepalive_timeout 65;
    ifdef(`ACCESS_LOG', `access_log ACCESS_LOG;')

    server {
        listen PORT;
        server_name auth-service;
        rewrite_log on;
        resolver 127.0.0.1;
        location / {
            auth_request /auth;
            auth_request_set $user $upstream_http_x_user;
            # The variable $user now contains the username when the check was
            # successful
            ifdef(`UPSTREAM_PORT', `proxy_pass http://$http_x_instance:UPSTREAM_PORT/;', `proxy_pass http://$http_x_instance/;')
            proxy_set_header X-User $user;
            proxy_set_header X-Original-URI $request_uri;
        }

        location = /auth {
                internal;
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                if ($token = '') {
                  return 403;
                }
                if ($http_x_instance = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/check-token/$http_x_token/$http_x_instance;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
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
        location = /check-token {
                set $token $cookie_token;
                if ($token = '') {
                  set $token $http_x_token;
                }
                if ($token = '') {
                  return 403;
                }
                proxy_pass http://AUTH_SERVICE/check-token/$http_x_token/;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
        }
    }
}
