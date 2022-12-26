# Makefile for react-app
APP_NAME = react-app

# TODO: Figure out what this should be
# TEMPLATE_DIR=../apps/examples/templates/react/$(APP_NAME)
TEMPLATE_FILE=../apps/examples/templates/react/$(APP_NAME).html
STATIC_DIR=../apps/examples/static/react/$(APP_NAME)

.PHONY: build test run

run:
	HTTPS=true BROWSER=none npm run start

# TODO: Figure out how to substitute the compiled versions into the react template
build:
	npm run build
	rm -rf $(STATIC_DIR); mkdir $(STATIC_DIR)
	cp build/static/js/main*.js $(STATIC_DIR)
	cp build/static/css/main*.css $(STATIC_DIR)
	cp build/manifest.json $(STATIC_DIR)
	python3 update_template.py
