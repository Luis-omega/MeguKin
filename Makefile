
.PHONY: requirements run

sourceEnv=source .env/bin/activate

src="MeguKin"

pythonFiles=$(find MeguKin/ tests/ -name "*.py")

pythonSrc=$(find MeguKin/ -name "*.py")

test:
	#@${sourceEnv};export PYTHONPATH=":";pytest
	@${sourceEnv};pytest

install: $(pythonSrc)
	@${sourceEnv};pip install -e .

uninstall:
	@${sourceEnv};pip uninstall ${src}

run:
	@${sourceEnv};megukin

gen-stub:
	@${sourceEnv};stubgen ${src}

format:
	@${sourceEnv};black ${src}/ tests/

requirements: 
	@${sourceEnv};pip freeze > requirements.txt

watch:
	@${sourceEnv};while sleep 0.5; do ls MeguKin/**/*.py tests/*.py | entr -d make test; done

clean_cache:
	rm -rf build MeguKin.egg-info .hypothesis .mypy_cache .pytest_cache .ropeproject
