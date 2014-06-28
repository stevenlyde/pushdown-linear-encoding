RACKET=racket
SHELL=/bin/bash
TESTS=${wildcard tests/*.scm}


.PHONY: cps matrices test racket clean

default: matrices

cps: $(TESTS:.scm=.cps) 

matrices: $(TESTS:.scm=.mat)

racket: $(TESTS:.scm=.rkt)



tests/%.cps: tests/%.scm
	$(RACKET) desugar.rkt < $< > $@

tests/%.mat: tests/%.cps
	$(RACKET) write-matrices.rkt < $< > $@

tests/%.rkt: tests/%.cps
	cp cps-header.rkt $@
	cat $< >> $@


clean:
	rm -f tests/*.{cps,mat,rkt}

