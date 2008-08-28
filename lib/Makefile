LIBS := selenium_remote

all test clean source_backup: 
	@for dir in $(LIBS); do \
	    (cd $$dir; $(MAKE) $@) \
	done

