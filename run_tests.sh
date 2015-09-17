
SHOULD_PASS_TESTS="tests/should-pass"
SHOULD_FAIL_TESTS="tests/should-fail"

for dir in $(ls -1 $SHOULD_PASS_TESTS)
do
    for file in $(ls -1 $SHOULD_PASS_TESTS/$dir/*.fk)
    do
	if [ "$1" = "v" ]; then
	    echo "Testing ${file}..."
	fi
	./frankInterpreter.byte ${file} > /dev/null 2>&1
	if [ "$?" -ne 0 ]; then
	    echo "${file} FAILED"
	fi
    done
done

for dir in $(ls -1 $SHOULD_FAIL_TESTS)
do
    for file in $(ls -1 $SHOULD_FAIL_TESTS/$dir/*.fk)
    do
	if [ "$1" = "v" ]; then
	    echo "Testing ${file}..."
	fi
	./frankInterpreter.byte ${file} > /dev/null 2>&1
	if [ "$?" -eq 0 ]; then
	    echo "${file} FAILED"
	fi
    done
done
