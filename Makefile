SHELL := /bin/bash

.PHONY: install-forthic test test-js credentials-server examples docs

example-server: install-forthic
	source myenv/bin/activate && cd server && FLASK_APP=run.py FLASK_DEBUG=true flask run --port=8000

myenv:
	python3 -m venv myenv

install-forthic: myenv
	source myenv/bin/activate && python -m pip install -U pip && pip install .

build-forthic-react:
	cd forthic-react/v1 && make build

docs: myenv
	source myenv/bin/activate && pip install tox && tox -edocs

test: myenv
	source myenv/bin/activate && pip install tox && tox

qa: myenv
	source myenv/bin/activate && pip install tox && tox -eqa

test-py:
	source myenv/bin/activate && python -m pytest tests/tests_py

delete-secrets:
	rm server/.key
	rm server/.secrets

# NOTE: The Forthic JS code has been deprecated. Please use Forthic React for client side work
test-js:
	@echo
	@echo "Forthic JS tests"
	@echo "============"
	node --experimental-modules ./tests/tests_js/test_all.mjs

test-react:
	@echo
	@echo "Forthic React tests"
	@echo "============"
	cd forthic-react/v1 && npm install && CI=1 npm run test

test-rs:
	@echo
	@echo "Forthic Rust tests"
	@echo "============"
	cargo test --manifest-path tests/tests_rs/Cargo.toml


test-all: test-py test-react test-js

credentials-server:
	FLASK_APP=apps/setup/run.py flask run --port=8000
