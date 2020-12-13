.PHONY: build
build:
	@elm-app build

.PHONY: run
run:
	@elm-app start

.PHONY: lint
lint:
	@elm-format src/ --yes
	@elm-analyse
