build-env-file=build.env
ifdef env-file
build-env-file=$(env-file)
endif
include $(build-env-file)

TAG=$(shell git rev-parse HEAD)
SERVICE_IMAGE=$(REGISTRY)/$(APP_IMAGE_NAME)
WEB_IMAGE=$(REGISTRY)/$(WEB_IMAGE_NAME)

all: service-container auth-web-container

build:
	cd service &&\
	stack build --install-ghc

service-container: build stack-deployimage
	cd service && \
	stack image container
	docker tag $(APP_IMAGE_NAME):latest $(SERVICE_IMAGE):latest
	docker tag $(APP_IMAGE_NAME):latest $(SERVICE_IMAGE):$(TAG)

auth-web-container:
	docker build -t $(WEB_IMAGE) web
	docker tag $(WEB_IMAGE):latest $(WEB_IMAGE):$(TAG)


run: all
	docker-compose up

up: all
	docker-compose up -d

down:
	docker-compose down --remove-orphans -v

stack-deployimage:
	scripts/docker-build docker/stack-deployimage

push:
	docker push $(WEB_IMAGE):$(TAG)
	docker push $(SERVICE_IMAGE):$(TAG)

.PHONY: all build run up down push stack-deployimage service-container auth-web-container