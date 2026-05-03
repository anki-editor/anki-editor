EL := anki-editor.el anki-editor-ui.el anki-editor-tests.el
ELC := $(EL:.el=.elc)

.PHONY: build test clean

build: $(ELC)

%.elc: %.el
	emacs -Q -batch -L . -f batch-byte-compile $<

test:
	emacs -Q -batch -l ert -l anki-editor.el -l anki-editor-tests.el \
	  --eval "(let ((ert-batch-print-level 10) (ert-batch-print-length 120)) \
                    (add-hook 'kill-emacs-hook #'anki-editor-test-stop-python-server) \
                    (ert-run-tests-batch-and-exit))"

clean:
	rm $(ELC)
