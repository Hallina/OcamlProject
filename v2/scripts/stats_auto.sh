#!/bin/bash

# ./abr tests/donnee100.txt dumps/dump_abr.csv
# ./abr tests/donnee150.txt dumps/dump_abr.csv
# ./abr tests/donnee500.txt dumps/dump_abr.csv
# ./abr tests/donnee750.txt dumps/dump_abr.csv
# ./abr tests/donnee1000.txt dumps/dump_abr.csv
# ./abr tests/donnee10000.txt dumps/dump_abr.csv
# ./abr tests/donnee50000.txt dumps/dump_abr.csv

# echo "abr donnees done at "; date +%H:%M:%S

# for taille in 100 150 250 500 750 1000 2500 5000 7500 10000 25000 50000
# do
#   for ((i = 1; i < 10; i++))
#   do 
#     ./abr "tests/size${taille}/random${taille}_${i}.txt" "dumps/dump_abr.csv"

#     echo "abr random${taille}_${i} done at "; date +%H:%M:%S

#   done
# done

# echo "abr random done at "; date +%H:%M:%S

# ./acme tests/donnee100.txt dumps/dump_acme.csv 100
# ./acme tests/donnee150.txt dumps/dump_acme.csv 35
# ./acme tests/donnee500.txt dumps/dump_acme.csv 90
# ./acme tests/donnee750.txt dumps/dump_acme.csv 143
# ./acme tests/donnee1000.txt dumps/dump_acme.csv 1000
# ./acme tests/donnee10000.txt dumps/dump_acme.csv 1960
# ./acme tests/donnee50000.txt dumps/dump_acme.csv 7572


d=$(date +%H:%M:%S)
echo "ach donnees done at ${d}"

for taille in 100 150 250 500 750 1000 2500 5000 7500 10000 25000 50000
do
  for ((i = 1; i < 10; i++))
  do 
    ./ach "tests/size${taille}/random${taille}_${i}.txt" "dumps/dump_ach.csv"

    d=$(date +%H:%M:%S)
    echo "ach random${taille}_${i} done at ${d}"

  done
done

d=$(date +%H:%M:%S)
echo "ach random done at ${d}"


d=$(date +%H:%M:%S)
echo "acm donnees done at ${d}"

for taille in 100 150 250 500 750 1000 2500 5000 7500 10000 25000 50000
do
  for ((i = 1; i < 10; i++))
  do 
    ./acm "tests/size${taille}/random${taille}_${i}.txt" "dumps/dump_acm.csv"

    d=$(date +%H:%M:%S)
    echo "acm random${taille}_${i} done at ${d}"

  done
done

d=$(date +%H:%M:%S)
echo "acm random done at ${d}"


for taille in 100 150 250 500 750 1000 2500 5000 7500 10000 25000 50000
do
  for ((i = 1; i < 10; i++))
  do 
    ./acme "tests/size${taille}/random${taille}_${i}.txt" "dumps/dump_acme.csv"

    d=$(date +%H:%M:%S)
    echo "acme random${taille}_${i} done at ${d}"

  done
done

d=$(date +%H:%M:%S)
echo "acme random done at ${d}"