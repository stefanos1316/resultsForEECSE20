#!/bin/sh

filename=$1

while read data; do
  task=$(echo "$data" | awk '{print $1}')
  stock=$(echo "$data" | awk '{print $2}')
  all=$(echo "$data" | awk '{print $3}')
  difference=$(percentageDifference "$stock" "$all")
  
  echo "$task $difference"
done < $1