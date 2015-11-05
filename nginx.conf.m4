# use m4 to configure AUTH_SERVICE and UPSTREAM and PORT
#
# Example:
# m4 -DAUTH_SERVICE=localhost:3000 \
#    -DUPSTREAM=localhost:4000 \
#    -DPORT=80
#    nginx.conf \
#    > nginx.conf.out


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
    server {
        listen PORT;
        server_name auth-service;
        rewrite_log on;
        location / {
            auth_request /auth;
            auth_request_set $user $upstream_http_x_user;
            # The variable $user now contains the username when the check was
            # successful

            proxy_pass http://UPSTREAM/;
            proxy_pass_request_body off;
            proxy_set_header Content-Length "";
            proxy_set_header X-User $user;
            proxy_set_header X-Original-URI $request_uri;
        }
        location /auth {
                internal;
                proxy_pass http://AUTH_SERVICE/checkToken/$cookie_token/;
                proxy_pass_request_body off;
                proxy_set_header Content-Length "";
                proxy_set_header X-Original-URI $request_uri;
        }

        # This part is only necessary if the client doesn't contact the central
        # auth service (e.g. for SSO)
        location /login {
                proxy_pass http://AUTH_SERVICE/login/;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token";
        }

        location /logout {
                proxy_pass http://AUTH_SERVICE/logout/$cookie_token;
                proxy_set_header X-Original-URI $request_uri;
                add_header Set-Cookie "token=$upstream_http_x_token";
        }


    }
 }