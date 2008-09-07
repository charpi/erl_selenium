include ../vsn.mk
include ../../../include.mk

# For make clean
EBIN_FILES+= ../ebin/selenium_api.beam ../ebin/selenium_session.beam 

all: ../ebin/selenium_api.beam ../ebin/selenium_session.beam 

../ebin/selenium_api.beam: ../priv/iedoc.xml
	$(ERLDIR)/bin/escript ../priv/make_api.esh api ../ebin ../priv/iedoc.xml

../ebin/selenium_session.beam: ../priv/iedoc.xml
	$(ERLDIR)/bin/escript ../priv/make_api.esh session ../ebin ../priv/iedoc.xml

test: all
	erl -pa ../ebin -s selenium_ut tests -s selenium_acceptance tests -s selenium_api_acceptance tests -s erlang halt


start_server:
	java -jar ../priv/selenium-server-1.0-beta-1.jar -log /tmp/selenium_server.log -browserSessionReuse > /dev/null 2>& 1 &\
	echo $$! > /tmp/selenium_server.pid

stop_server:
	kill `cat /tmp/selenium_server.pid`
