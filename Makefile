build-env-file=build.env
ifdef env-file
build-env-file=$(env-file)
endif
include $(build-env-file)

WEB_IMAGE=$(REGISTRY)/$(WEB_IMAGE_NAME)

.PHONY: all
all: auth-web.image
	$(MAKE) -C service all

.PHONY: service/image
service/image:
	$(MAKE) -C service image

.PHONY: test
test: unittests systemtests

.PHONY: unittests
unittests:
	$(MAKE) -C service test

systemtests: export NORATELIMIT=true
systemtests: up
	tests/test dockertest
	$(MAKE) down

auth-web-deps := $(shell find web)

auth-web.image: $(auth-web-deps)
	docker build -t $(WEB_IMAGE):$(TAG) web
	echo -n "$(TAG)" > auth-web.image

.PHONY: run
run: service/image auth-web.image
	docker-compose up

dev/ed25519.priv.der:
	mkdir -p dev
	openssl genpkey -algorithm Ed25519 -outform der \
	  | base64 > dev/ed25519.priv.der

dev/ed25519.priv.env: dev/ed25519.priv.der
	bash -c 'echo "SIGNED_HEADERS_PRIVATE_KEY=$$(cat dev/ed25519.priv.der)" > dev/ed25519.priv.env'

dev/ed25519.pub.der: dev/ed25519.priv.der
	base64 -d dev/ed25519.priv.der \
	  | openssl pkey -inform der -pubout -outform der \
	  | base64 > dev/ed25519.pub.der

.PHONY: up
up: service/image auth-web.image dev/ed25519.priv.env
	docker-compose up -d

.PHONY: down
down:
	docker-compose down --remove-orphans -v

.PHONY: push
push:
	$(MAKE) -C service push
	docker push $(WEB_IMAGE):$(TAG)
	docker push $(WEB_IMAGE):latest

.PHONY: clean
clean:
	$(MAKE) -C service clean
	rm -r dev
