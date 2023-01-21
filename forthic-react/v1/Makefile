# Makefile for react-app
APP_NAME = react-app
STATIC_DIR=../../apps/examples/static/react/$(APP_NAME)/v1

.PHONY: build test run

run:
	HTTPS=true BROWSER=none npm run start

test:
	npm run test

build:
	npm run build
	rm -rf $(STATIC_DIR); mkdir -p $(STATIC_DIR)
	cp build/static/js/main*.js* $(STATIC_DIR)
	cp build/static/css/main*.css $(STATIC_DIR)
	cp build/manifest.json $(STATIC_DIR)
	python3 update_template.py
