.PHONY: all hooks test build clean lint stylish

all: lint stylish test

hooks:
	cp hooks/* .git/hooks/

test:
	AWS_SECRET_ACCESS_KEY=foo AWS_ACCESS_KEY_ID=bar stack build skylark-core --test

build:
	stack build skylark-core

clean:
	stack clean

lint:
	hlint src
	hlint test/Test

stylish:
	find src -name "*.hs" -type f -execdir stylish-haskell -i "{}" \;
	find test/Test -name "*.hs" -type f -execdir stylish-haskell -i "{}" \;

