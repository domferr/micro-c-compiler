#!/bin/bash

PASS_COLOR='\033[1;92m'
FAILED_COLOR='\033[1;91m'
RESET_COLOR='\033[0m'

total_test_sources=0
total_pass=0
total_fail=0
for var in "$@"
do
  ((total_test_sources=total_test_sources+1))
  { error=$(opam exec -- dune exec test/parser_test.exe -- "$var" 2>&1 1>/dev/null); }

  if [ -z "$error" ] 
  then
    printf "$PASS_COLOR[ PASS ]$RESET_COLOR %s\n" $var
    ((total_pass=total_pass+1))
  else
    printf "$FAILED_COLOR[ FAIL ]$RESET_COLOR %s\n%s\n\n" "$var" "$error"
    ((total_fail=total_fail+1))
  fi
done
printf "\n\n--------------------------\n"
printf "Total tested sources: %d\n" $total_test_sources
printf "Pass: %d\tFail: %d\n" $total_pass $total_fail