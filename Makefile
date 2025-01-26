docs: clean
	cabal run site build

watch: docs
	cabal run site watch

clean:
	rm -rf _cache docs
