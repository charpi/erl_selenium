ERL_TOP="/usr/local/lib/erlang-R13B-4"
USE_EMAKE = true
EMAKE_COMPILE_OPTIONS = ["{d, 'HOST', \"localhost\"}",
                         "{d, 'PORT', 4444}",
                         "{d, 'COMMAND', \"*firefox /usr/lib/firefox-3.5.9/firefox \"}"]
ERLC_FLAGS = "+warn_unused_vars +warn_unused_import"
ERL_FLAGS = "-sname test"
CHECK_APP= true
