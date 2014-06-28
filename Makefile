RACKET=racket
SHELL=/bin/bash
TESTS=${wildcard tests/*.scm}


.PHONY: anf matrices test racket clean

default: matrices

anf: $(TESTS:.scm=.anf) 

matrices: $(TESTS:.scm=.mat)

racket: $(TESTS:.scm=.rkt)



tests/%.anf: tests/%.scm
	$(RACKET) desugar.rkt < $< > $@

tests/%.mat: tests/%.anf
	$(RACKET) write-matrices.rkt < $< > $@

tests/%.rkt: tests/%.anf
	cp anf-header.rkt $@
	cat $< >> $@


clean:
	rm -f tests/*.{anf,mat,rkt}

