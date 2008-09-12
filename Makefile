include vsn.mk 
LIBS=lib

all %: 
	@for dir in $(LIBS); do \
	 if ! test  -f $$dir/SKIP ; then \
	    $(MAKE) -C $$dir $@ || exit 1; \
	 fi \
	done;


source_backup:
	today=`date +%Y%m%d` ;\
	tar --exclude="*/.svn*" --exclude="*~" --exclude="selenium/selenium_src-*.tgz" -C .. -czvf selenium_src-$$today.tgz selenium



