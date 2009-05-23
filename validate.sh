#! /bin/sh 


status() {
    case "$1" in
	"0") echo "Ok"
	    ;;
	*) echo "Fail"
	    ;;
    esac
}

rake start_server 1> /dev/null 2>&1 
echo -n "Validate with OTP R12B-5 : "
sed -i -e "s,ERL_TOP.*,ERL_TOP=\"/usr/local/lib/erlang_R12-B5\"," erlang_config.rb
rake clean 2> /dev/null
rake tests 1> /tmp/selenium_validation_R12.log 2>&1 
status $?
echo -n "Validate with OTP R13B-0 : "
sed -i -e "s,ERL_TOP.*,ERL_TOP=\"/usr/local/lib/erlang_R13-B0\"," erlang_config.rb
rake clean 2> /dev/null
rake tests  1> /tmp/selenium_validation_R13.log 2>&1 
status $?
rake stop_server 1> /dev/null 2>&1
