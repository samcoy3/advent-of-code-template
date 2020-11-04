#!/usr/bin/zsh

for x in "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25"; do cat src/Days/Day01.hs | sed "s/01/$x/" > src/Days/Day"$x".hs; done
