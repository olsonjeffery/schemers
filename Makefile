CFG_PYTHON				?= python
E = echo
Q = @
RUSTCRATES          =   schemers

schemers_TYPE        =    bin
schemers_RUSTCFLAGS  =    -D warnings

all: tidy
#schemers_RUSTCFLAGS  +=	  schemers

include             rust-mk/rust.mk

tidy:
	@echo check: formatting
	$(Q)find $(S)src -name '*.r[sc]' \
	| grep '^$(S)src/rust-sdl2' -v \
	| xargs -n 10 $(CFG_PYTHON) $(S)src/etc/tidy.py

###############
