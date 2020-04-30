.PHONY: commit-check check clean lint style test test-watch coverage circleci circleci-prof-test check-dburi prompt-clean prompt-long-process

commit-check: lint style

check: lint style test

clean: prompt-clean
	stack clean --full

# For running these you'll need to install hlint and stylish-haskell first. Run:
# stack install hlint stylish-haskell
lint:
	git ls-files | grep '\.l\?hs$$' | xargs stack exec -- hlint -X QuasiQuotes -X NoPatternSynonyms "$$@"

style:
	git ls-files | grep '\.l\?hs$$' | xargs stack exec -- stylish-haskell -i && git diff-index --exit-code HEAD -- '*.hs' '*.lhs'

test: check-dburi
	stack test

test-watch: check-dburi
	stack build --file-watch --test --test-arguments '--rerun --failure-report=.TESTREPORT --rerun-all-on-success'

coverage: check-dburi clean
	stack build --coverage
	stack test --coverage

circleci: prompt-long-process
	circleci local execute --job build-test-9.4

circleci-prof-test: prompt-long-process
	circleci local execute --job build-prof-test

check-dburi:
	test -n "$(POSTGREST_TEST_CONNECTION)" # Requires POSTGREST_TEST_CONNECTION environmental variable

prompt-clean:
	@echo -n 'Are you sure? You will have to rebuild. [y/N] ' && read ans && [ $${ans:-N} = y ]

prompt-long-process:
	@echo -n 'Are you sure? This might take a while. [y/N] ' && read ans && [ $${ans:-N} = y ]
