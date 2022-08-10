PROJECT_NAME := kivra_api_errors

build:
	@docker build -t $(PROJECT_NAME) .

lint: build
	@docker run --rm $(PROJECT_NAME)

erl:
	rebar3 dialyzer;
	rebar3 eunit

go:
	go test .
