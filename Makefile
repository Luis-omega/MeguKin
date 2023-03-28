
.PHONY: requirements

sourceEnv=source .env/bin/activate

src="MeguKin"

pythonFiles=$(find MeguKin/ tests/ -name "*.py")

pythonSrc=$(find MeguKin/ -name "*.py")

test: install
	@${sourceEnv};pytest

install: $(pythonSrc)
	@${sourceEnv};pip install .

uninstall:
	@${sourceEnv};pip uninstall ${src}

gen-stub:
	@${sourceEnv};stubgen ${src}

format:
	@${sourceEnv};black ${src}/ tests/

requirements: 
	@${sourceEnv};pip freeze > requirements.txt

