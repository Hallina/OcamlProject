#!/bin/bash

for ((i = 0; i < $4; i++))
do
    ./$1 $2 $3
done