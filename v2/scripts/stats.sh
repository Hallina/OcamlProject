#!/bin/bash

for ((i = 0; i < $5; i++))
do
    ./$1 $2 $3 $4
done