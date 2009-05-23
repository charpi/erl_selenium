ERL_TOP="/usr/local/lib/erlang_R13-B0"
USE_EMAKE = true
EMAKE_COMPILE_OPTIONS = ["{d, 'HOST', \"localhost\"}",
                         "{d, 'PORT', 4444}",
                         "{d, 'COMMAND', \"*safari\"}"]
ERLC_FLAGS = "+warn_unused_vars +warn_unused_import"
ERL_FLAGS = "-sname test"

