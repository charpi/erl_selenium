#! /bin/sh 


status() {
    case "$1" in
	"0") echo "Ok"
	    ;;
	*) echo "Fail"
	    ;;
    esac
}

make start_server 1> /dev/null 2>&1 
echo -n "Validate with OTP R12B-3 : "
make ERLDIR=/opt/lib/otp_R12B-3 clean all test 1> /tmp/selenium_validation.log 2>&1 
status $?
echo -n "Validate with OTP R11B-5 : "
make ERLDIR=/opt/lib/otp_R11B-5 clean all test  1> /tmp/selenium_validation.log 2>&1 
status $?
make stop_server 1> /dev/null 2>&1
