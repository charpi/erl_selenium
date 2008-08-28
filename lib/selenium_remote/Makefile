SUB_DIRECTORIES = src priv

all release_tar test clean source_backup: 
	@for dir in $(SUB_DIRECTORIES); do \
	  if ! test  -f $$dir/SKIP ; then \
	    (cd $$dir; $(MAKE) $@) \
	  fi \
	done
