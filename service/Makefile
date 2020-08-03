ifeq ($(origin stack_args),undefined)
export stack_args := --docker --docker-stack-exe download --no-nix --colour=auto
endif

ifeq (${TERM},dumb)
export stack_build_args := ${stack_build_args} --colour=never
endif

TAG=$(shell git rev-parse HEAD)
SERVICE_IMAGE=$(REGISTRY)/$(APP_IMAGE_NAME)

tests = dist/tests/api-tests dist/tests/unit-tests

tests-baseimage = registry.nejla.com/nejla-ab/auth-service/baseimage
srcfiles=$(shell find src -type f)
test-srcfiles=$(shell find test -type f)


.PHONY: all
all: image baseimage $(tests)

image: dist/auth-service Dockerfile
	docker build -t "registry.nejla.com/nejla-ab/auth-service/auth-service:${TAG}" .
	docker tag "registry.nejla.com/nejla-ab/auth-service/auth-service:${TAG}" \
	           "registry.nejla.com/nejla-ab/auth-service/auth-service:latest"
	touch image


baseimage: Dockerfile
	docker build . --target baseimage \
	  -t $(tests-baseimage):$(shell git rev-parse HEAD)
	touch baseimage

dist/auth-service: $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc --test --no-run-tests ${stack_build_args} \
	      --copy-bins --local-bin-path ./dist


# Tests
#######

$(tests): dist/tests/% : dist/auth-service $(test-srcfiles) package.yaml stack.yaml
	mkdir -p dist/tests
	cp "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/

# Bring up required services before running test suite
.PHONY: test-up
test-up: $(tests) baseimage
	docker-compose -f docker-compose.test.yaml -p auth-service-test up -d
	$(MAKE) test

.PHONY: test-down
test-down:
	docker-compose -f docker-compose.test.yaml -p auth-service-test down -v

.PHONY: test
test: $(tests) baseimage
	docker run --rm -it -v "$(PWD)/dist/tests:/tests:ro" \
           --network auth-service-test_auth-service-test \
           -e DB_HOST=database \
           $(tests-baseimage):$(shell git rev-parse HEAD) \
           sh -c 'for t in /tests/*; do $$t; done'

# Clean
#######

.PHONY: clean
clean:
	stack clean
	rm -f stack.yaml.lock
	rm -rf dist
	rm -f *.cabal
	rm -f image
	rm -f baseimage

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
