LIBS := selenium_remote


%: 
	@for dir in $(LIBS); do \
	 if ! test  -f $$dir/SKIP ; then \
	    $(MAKE) -C $$dir $@ || exit 1; \
	 fi \
	done;
