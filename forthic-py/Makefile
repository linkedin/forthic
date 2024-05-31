test:
	tox

flit-env:
	python3 -m venv flit-env


build: flit-env
	source flit-env/bin/activate && python -m pip install -U pip && pip install flit && flit build
	rm -rf flit-env

