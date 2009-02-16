ERL_TOP="/opt/lib/otp_R12B-5"
USE_EMAKE = true
EMAKE_COMPILE_OPTIONS = ["{d, 'HOST', \"localhost\"}",
                         "{d, 'PORT', 4444}",
                         "{d, 'COMMAND', \"*firefox\ /usr/lib/firefox-3.0.6/firefox\"}"]
ERLC_FLAGS = "+warn_unused_vars +warn_unused_import"
ERL_FLAGS = "-sname test"

