PASS_COLOR='\033[1;92m'
FAILED_COLOR='\033[1;91m'
RESET_COLOR='\033[0m'

executable=testexecutable
lsresult=(`ls ./test/samples/test*.mc`)
results=()
sources=()
for i in "${!lsresult[@]}"; do
    result_file="${lsresult[i]%.*}.out"
    if [ -f "$result_file" ]; then
        results+=($result_file)
        sources+=("${lsresult[i]}")
    fi
done

total_test_sources=0
total_pass=0
total_fail=0

for i in "${!sources[@]}";
do
    ((total_test_sources=total_test_sources+1))
    { error=$(opam exec -- dune exec bin/microcc.exe -- "${sources[i]}" -o "$executable.out" 2>&1 1>/dev/null); }

    if [ -f "$executable" ] 
    then
        exeres=$(./$executable)
        correctres="$(<${results[i]})"
        
        if [[ $exeres == $correctres ]]; then
            printf "$PASS_COLOR[ PASS ]$RESET_COLOR %s, %s\n" "${sources[i]}" "${results[i]}"
            ((total_pass=total_pass+1))
        else
            printf "$FAILED_COLOR[ FAIL ]$RESET_COLOR %s, %s\nCorrect result: %s\nGiven result: %s\n\n" "${sources[i]}" "${results[i]}" "$correctres" "$exeres"
            ((total_fail=total_fail+1))
        fi

        rm $executable
    else
        printf "$FAILED_COLOR[ FAIL ]$RESET_COLOR %s, %s\n%s\n\n" "${sources[i]}" "${results[i]}" "$error"
        ((total_fail=total_fail+1))
    fi
done

printf "\n\n--------------------------\n"
printf "Total tested sources: %d\n" $total_test_sources
printf "Pass: %d\tFail: %d\n" $total_pass $total_fail
