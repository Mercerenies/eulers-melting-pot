#!/bin/zsh

max=
maxlen=0
for var in `seq 1 999`; do
    denoms=
    into=1
    while [[ ! "$denoms" =~ "b"$into"e" ]]; do
        denoms=$denoms"b"$into"e"
        (( into=$into % $var ))
        (( into=$into * 10 ))
    done
    answer=`echo $denoms | grep -o "b"$into"e.*$"`
    answer=`echo $answer | tr e "\n" | wc -l`
    if (( $answer > $maxlen )); then
        max=$var
        maxlen=$answer
    fi
done
echo $max
