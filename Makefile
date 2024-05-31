SHELL := /bin/bash
TOX_INI = -c ./forthic-py/tox.ini

.PHONY: install-forthic test test-js credentials-server examples docs

# ----- Deprecated targets -----------------------------------------------------
## NOTE: These are deprecated targets that are only useful for LinkedIn internal apps and
##       the older Coding Forthic videos.
example-server: install-forthic
	source myenv/bin/activate && cd server && FLASK_APP=run.py FLASK_DEBUG=true flask run --port=8000

myenv:
	python3 -m venv myenv

install-forthic: myenv
	source myenv/bin/activate && python -m pip install -U pip && cd ./forthic-py && pip install . && pip install Flask

test-py:
	source myenv/bin/activate && python -m pytest tests/tests_py

delete-secrets:
	rm server/.key
	rm server/.secrets

build-forthic-react:
	cd forthic-react/v1 && make build

test-all: test-py test-react test-js

credentials-server:
	FLASK_APP=apps/setup/run.py flask run --port=8000


# ----- Python package targets ------------------------------------------------
## These targets are used to test and build the Forthic Python package
test: myenv
	source myenv/bin/activate && pip install tox && tox $(TOX_INI)

qa: myenv
	source myenv/bin/activate && pip install tox && tox $(TOX_INI) -eqa


# ----- Other test targets -----------------------------------------------------
# NOTE: The Forthic JS code has been deprecated. Please use Forthic React for client side work
test-js:
	@echo
	@echo "Forthic JS tests"
	@echo "============"
	node --experimental-modules ./forthic-js/tests/test_all.mjs

test-react:
	@echo
	@echo "Forthic React tests"
	@echo "============"
	cd forthic-react/v1 && npm install && CI=1 npm run test

test-rs:
	@echo
	@echo "Forthic Rust tests"
	@echo "============"
	cargo test --manifest-path forthic-rs/tests/Cargo.toml
