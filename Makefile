PROJECT_NAME := kivra_api_errors

build:
	@docker build -t $(PROJECT_NAME) .

lint: build
	@docker run --rm $(PROJECT_NAME)
