FROM nginx

EXPOSE 3000

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get -y install netcat

COPY nginx.conf /etc/nginx/nginx.conf
COPY run.sh /app/run.sh
WORKDIR /app
CMD ["sh", "run.sh"]
