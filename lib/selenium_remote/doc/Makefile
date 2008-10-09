include ../vsn.mk
include ../../../include.mk

all: selenium_api.html edoc

selenium_api.html: ../priv/iedoc.xml ../priv/make_api.esh
	$(ERLDIR)/bin/escript ../priv/make_api.esh doc . ../priv/iedoc.xml

edoc:
	$(ERLDIR)/bin/escript ../../../mak/make_doc selenium_remote ../../../lib
