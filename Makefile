.PHONY : build
build :
	stack build

.PHONY : run
run : build
	stack exec -- ebnf-grammar-parser-exe | more

.PHONY : docs
docs :
	stack haddock && open `stack path --local-doc-root`/index.html

.PHONY : lint
lint :
	hlint app src

.PHONY : tidy
tidy :
	-find . -name '*~' -delete
	-find . -name '#*' -delete

.PHONY : clean
clean : tidy
	stack clean
