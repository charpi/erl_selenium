LIBS=lib

all test clean binary_backup: 
	@for dir in $(LIBS); do \
	    (cd $$dir; $(MAKE) $@) \
	done

source_backup:
	tar --exclude="*~" --exclude="selenium/selenium_src.tgz" -C .. -czvf selenium_src.tgz selenium


