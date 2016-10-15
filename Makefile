UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
	EMACS := /Users/emacs/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs
else
	EMACS := emacs
endif

test:
	/bin/bash -l -c "$(EMACS) -Q -batch -l kungfu-tests.el -f ert-run-tests-batch-and-exit"
termux:	
	/data/data/com.termux/files/usr/bin/bash -l -c "$(EMACS) -Q -batch -l kungfu-tests.el -f ert-run-tests-batch-and-exit"
