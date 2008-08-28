include vsn.mk 
LIBS=lib

all test clean: 
	@for dir in $(LIBS); do \
	    (cd $$dir; $(MAKE) $@) \
	done

source_backup:
	today=`date +%Y%M%d` ;\
	tar --exclude="*/.svn*" --exclude="*~" --exclude="selenium/selenium_src-*.tgz" -C .. -czvf selenium_src-$$today.tgz selenium



