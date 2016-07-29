FROM debian:jessie
MAINTAINER LambdatradeAB

EXPOSE 3000

RUN echo "dependencies v1" && \
    export DEBIAN_FRONTEND=noninteractive && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/debian jessie main' > /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get -y install \
      g++ \
      libicu-dev \
      libpq-dev \
      libstdc++-4.8-dev \
      netcat \
      postgresql-client \
      stack \
        && \
    ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/libstdc++.so && \
    rm -rf /var/lib/apt/lists/*

RUN stack setup --resolver=lts-6.9

COPY auth-service.cabal /opt/auth-service-cabal/auth-service.cabal
COPY stack.yaml /opt/auth-service-cabal/stack.yaml
COPY nejla-common /opt/auth-service-cabal/nejla-common
RUN cd /opt/auth-service-cabal && \
    stack setup && \
    stack install --dependencies-only && \
    rm -R /opt/auth-service-cabal

COPY . /opt/auth-service
RUN cd /opt/auth-service && \
    stack install && \
    rm -R /opt/auth-service

ENV PATH /root/.local/bin:$PATH

COPY run.sh /run.sh

CMD ["sh", "/run.sh"]
