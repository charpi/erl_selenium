include vsn.mk 
include include.mk
LIBS=lib

all clean %: 
	@for dir in $(LIBS); do \
	 if ! test  -f $$dir/SKIP ; then \
	    $(MAKE) -C $$dir $@ || exit 1; \
	 fi \
	done;


start_forge:
	$(ERLDIR)/bin/escript mak/forge_server.esh

source_backup:
	today=`date +%Y%m%d` ;\
	tar --exclude="*/.svn*" --exclude="*~" --exclude="selenium/selenium_src-*.tgz" -C .. -czvf selenium_src-$$today.tgz selenium



