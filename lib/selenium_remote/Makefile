SUB_DIRECTORIES = src priv doc

edoc: 
	 $(MAKE) -C doc $@ || exit 1
%: 
	@for dir in $(SUB_DIRECTORIES); do \
	 if ! test  -f $$dir/SKIP ; then \
	    $(MAKE) -C $$dir $@ || exit 1 ; \
	 fi \
	done;
