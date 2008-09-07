SUB_DIRECTORIES = src priv doc

%: 
	@for dir in $(SUB_DIRECTORIES); do \
	  if ! test  -f $$dir/SKIP ; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done
