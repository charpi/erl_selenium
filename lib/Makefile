LIBS := selenium_remote

all test clean binary_backup: 
	@for dir in $(LIBS); do \
	    (cd $$dir; $(MAKE) $@) \
	done

