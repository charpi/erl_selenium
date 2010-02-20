#! /bin/sh 

status() {
    case "$1" in
	"0") echo "Ok"
	    ;;
	*) echo "Fail"
	    ;;
    esac
}

run_validation() {
    echo "Validate with $1 : "
    sed -i -e "s,ERL_TOP.*,ERL_TOP=\"$1\"," erlang_config.rb
    rake clean 2> /dev/null
    rake tests 1> /tmp/selenium_validation_R12.log 2>&1 
    status $?
}

rake start_server 1> /dev/null 2>&1 

run_validation /usr/local/lib/erlang_R12-B5
run_validation /usr/local/lib/erlang_R13-B2

rake stop_server 1> /dev/null 2>&1
