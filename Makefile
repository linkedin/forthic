.PHONY: install-forthic test test-js credentials-server examples docs

example-server: install-forthic
	pushd apps/examples/static/forthic && ln -sf ../../../../forthic-js . && popd
	source myenv/bin/activate && cd apps/examples && FLASK_APP=run.py FLASK_ENV=development flask run --port=8000

myenv:
	python3 -m venv myenv

install-forthic: myenv
	source myenv/bin/activate && python -m pip install -U pip && pip install .

docs: myenv
	source myenv/bin/activate && pip install tox && tox -edocs

test: myenv
	source myenv/bin/activate && pip install tox && tox

qa: myenv
	source myenv/bin/activate && pip install tox && tox -eqa

test-js:
	@echo
	@echo "JS tests"
	@echo "============"
	node --experimental-modules ./tests/tests_js/test_all.mjs

test-all: test test-js

credentials-server:
	FLASK_APP=apps/setup/run.py flask run --port=8000
