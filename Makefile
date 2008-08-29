include vsn.mk 
LIBS=lib

all test clean: 
	@for dir in $(LIBS); do \
	    (cd $$dir; $(MAKE) $@) \
	done

source_backup:
	today=`date +%Y%m%d` ;\
	tar --exclude="*/.svn*" --exclude="*~" --exclude="selenium/selenium_src-*.tgz" -C .. -czvf selenium_src-$$today.tgz selenium



