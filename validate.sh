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
    echo "---- Validate with $1 ----" >> /tmp/selenium_validation.log
    rake tests 1>> /tmp/selenium_validation.log 2>&1 
    status $?
}

rake start_test_server 1> /dev/null 2>&1 
sleep 1
rm /tmp/selenium_validation.log

run_validation /usr/local/lib/erlang

rake stop_test_server 1> /dev/null 2>&1
