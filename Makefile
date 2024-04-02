RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

build:
	$(RUN_LISP) --eval '(asdf:make :riacl)'

lint:
	~/.roswell/bin/sblint | reviewdog -efm="%f:%l:%c: %m" -diff="git diff main"

.PHONY: build lint
