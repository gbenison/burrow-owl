
## EXTRA_DIST=demonstration

## .PHONY: doc

all-local: doxygen-docs

doxygen-docs:
	$(DOXYGEN) Doxyfile

clean-local:
	test -d api && rm -r api
