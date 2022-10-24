PROJECT_NAME := kivra_api_errors

build:
	@docker build -t $(PROJECT_NAME) .

lint: build
	@docker run --rm $(PROJECT_NAME)

test-erl:
	rebar3 dialyzer;
	rebar3 eunit

test-go:
	go test .

test-python:
	python3 -m unittest kivra_api_errors_tests.py
