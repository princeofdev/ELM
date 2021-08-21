export PATH := ../../node_modules/.bin:$(PATH)
export SHELL := /usr/bin/env bash

all: build elm index
build:
mkdir -p dist \
mkdir -p build
elm:
elm make src/Main.elm –output build/main.js
bundle:
cat build/main.js build/bootstrap.js > dist/bundle.js
index:
cat build/index.html > dist/index.html
