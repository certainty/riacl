RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

build: build-server

build-server:
	$(RUN_LISP) --eval '(asdf:make :riacl/server)'

test-server:
	$(RUN_LISP) --eval '(asdf:test-system :riacl/server)'

test-client:
	$(RUN_LISP) --eval '(asdf:test-system :riacl/client)'

lint:
	~/.roswell/bin/sblint | reviewdog -efm="%f:%l:%c: %m" -diff="git diff main"

.PHONY: build lint
