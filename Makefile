.PHONY: commit-check check clean lint style test test-watch coverage circleci circleci-memory prompt-clean prompt-long-process cachix-push-all

commit-check: lint style

check: lint style test

clean: prompt-clean
	stack clean --full

# For running these you'll need to install hlint and stylish-haskell first. Run:
# stack install hlint stylish-haskell
lint:
	git ls-files | grep '\.l\?hs$$' | \
	  xargs stack exec -- hlint -X QuasiQuotes -X NoPatternSynonyms "$$@"

style:
	git ls-files | grep '\.l\?hs$$' | xargs stack exec -- stylish-haskell -i

test:
	test/with_tmp_db stack test

test-watch:
	test/with_tmp_db stack build --file-watch --test \
	  --test-arguments '--rerun --failure-report=.TESTREPORT --rerun-all-on-success'

coverage: clean
	stack build --coverage
	test/with_tmp_db stack test --coverage

circleci: prompt-long-process
	circleci local execute --job stack-test

circleci-memory: prompt-long-process
	circleci local execute --job stack-test-memory

prompt-clean:
	@echo -n 'Are you sure? You will have to rebuild. [y/N] ' && read ans && [ $${ans:-N} = y ]

prompt-long-process:
	@echo -n 'Are you sure? This might take a while. [y/N] ' && read ans && [ $${ans:-N} = y ]

cachix-push-all:
	nix-store -qR --include-outputs $$(nix-instantiate) | cachix push postgrest
