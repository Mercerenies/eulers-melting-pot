#!/bin/zsh

max=
maxlen=0
for var in `seq 1 999`; do
    denoms=
    into=1
    while [[ ! "$denoms" =~ "b"$into"e" ]]; do
        denoms=$denoms"b"$into"e"
        (( into=$into * 10 ))
        (( into=$into % $var ))
    done
    answer=`echo $denoms | grep -o "b"$into"e.*$"`
    answer=`echo $answer | tr e "\n" | wc -l`
    (( answer=$answer - 1 ))
    if (( $answer > $maxlen )); then
        max=$var
        maxlen=$answer
    fi
done
echo $max
