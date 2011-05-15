ERL_TOP="/usr/local/lib/erlang"
USE_EMAKE = true
EMAKE_COMPILE_OPTIONS = ["{d, 'HOST', \"localhost\"}",
                         "{d, 'PORT', 4444}",
                         "{d, 'COMMAND', \"*firefox\"}",
			 "{d, 'WEBDRIVER_BROWSER', [htmlunit, firefox]}"
			 ]
ERLC_FLAGS = "+warn_unused_vars +warn_unused_import"
ERL_FLAGS = "-sname test"
CHECK_APP= false
