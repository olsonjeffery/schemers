CFG_PYTHON				?= python
Q = @

tidy:
	@echo check: formatting
	$(Q)find $(S)src -name '*.r[sc]' \
	| xargs -n 10 $(CFG_PYTHON) $(S)etc/tidy.py
