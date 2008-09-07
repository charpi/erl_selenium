include ../vsn.mk
include ../../../include.mk

all: selenium_api.html 

selenium_api.html: ../priv/iedoc.xml
	$(ERLDIR)/bin/escript ../priv/make_api.esh doc . ../priv/iedoc.xml
