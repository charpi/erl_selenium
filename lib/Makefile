LIBS := selenium_remote


%: 
	@for dir in $(LIBS); do \
	    (cd $$dir; $(MAKE) $@) \
	done

