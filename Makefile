WEBFILES=README.html BUGS.html TODO.html

web: $(WEBFILES)

webup: web
	scp $(WEBFILES) common-lisp.net:public_html/swankr/

README.html: README
	emacs --batch --visit README  --eval '(org-export-as-html nil)'

%.html: %.org
	emacs --batch --visit $^ --eval '(org-export-as-html nil)'

clean:
	-rm $(WEBFILES)

.PHONY: web clean
