# Makefile for react-app
STATIC_DIR=../../server/static/react/react-app/v1

.PHONY: build test run

run:
	HTTPS=true BROWSER=none npm run start

test:
	@echo
	@echo "Forthic React tests"
	@echo "============"
	CI=1 npm run test

run-test:
	npm run test

build:
	npm run build
	rm -rf $(STATIC_DIR); mkdir -p $(STATIC_DIR)
	cp build/static/js/main*.js* $(STATIC_DIR)
	cp build/static/css/main*.css $(STATIC_DIR)
	cp build/manifest.json $(STATIC_DIR)
	python3 update_template.py
