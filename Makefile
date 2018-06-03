.PHONY: build
build:
	stack build
.PHONY: install
install:
	stack install
.PHONY: test
test: install
	socks spec/loader.sox
