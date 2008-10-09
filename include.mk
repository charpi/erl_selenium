###-*-makefile-*-   ; force emacs to enter makefile-mode

ERLDIR=/home/opt/lib/erlang

EMULATOR=beam
APP=app

ERLBINDIR =$(ERLDIR)/bin
ERL=$(ERLBINDIR)/erl
ERLC=$(ERLBINDIR)/erlc

EBIN_FILES= $(patsubst %.erl,../ebin/%.$(EMULATOR),$(wildcard *.erl))
MODULES=$(basename $(wildcard *.erl))
APP_FILES= $(patsubst %.app.src,../ebin/%.$(APP),$(wildcard *.app.src))
RELEASE_FILE=$(RELEASES:%=../ebin/%.rel)
BACKUP_FILES=*~

all: $(EBIN_FILES) $(APP_FILES)

clean:
	rm -f $(EBIN_FILES) $(APP_FILES) erl_crash.dump

binary_backup: all
	tar -C .. -czvf ../../../test-$(VSN).tgz .

APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

# Targets
../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.appup: %.appup 
	cp $< $@

../ebin/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

%.$(EMULATOR): 	%.erl
	$(ERLC) $(ERLC_FLAGS) $<
