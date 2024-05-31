SHELL := /bin/bash
TOX_INI = -c ./forthic-py/tox.ini

.PHONY: install-forthic test credentials-server

# ----- Example server --------------------------------------------------------
example-server: install-forthic
	source myenv/bin/activate && cd server && FLASK_APP=run.py FLASK_DEBUG=true flask run --port=8000

myenv:
	python3 -m venv myenv

install-forthic: myenv
	source myenv/bin/activate && python -m pip install -U pip && cd ./forthic-py && pip install . && pip install Flask

delete-secrets:
	rm server/.key
	rm server/.secrets

credentials-server:
	FLASK_APP=apps/setup/run.py flask run --port=8000

build-forthic-react:
	cd forthic-react/v1 && make build


# ----- Tests ------------------------------------------------------------------

test-py:
	cd forthic-py && make test

test-js:
	cd forthic-js && make test

test-react:
	cd forthic-react/v1 && make test

test-all: test-py test-js test-react


test-rs:
	cd experimental/forthic-rs && make test

test-zig:
	cd experimental/forthic-zig && make test

test-experimental: test-rs test-zig
