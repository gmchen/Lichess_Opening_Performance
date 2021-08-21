#!/bin/bash

echo "Start time: $(date)" >> data/2_get_analyis_urls.log

printf "name.unique	san	url\n" > data/computer_analysis_urls.tsv

while IFS=$'\t' read -r -a myArray
do
	line="${myArray[0]}	${myArray[1]}"
	name_unique=${myArray[0]}
	san=${myArray[1]}
	echo $name_unique
	echo $san
	if [[ "$name_unique" == "name.unique" ]]; then
		continue
	fi
	
	current_url=""
	while true
	do
		return_val=$(curl -X POST \
			-F "pgn=$san" \
			https://lichess.org/api/import)
		echo $return_val
		if [[ "$return_val" =~ .*"Too many requests".* ]]
		then
 			echo "Received too many requests message - waiting 1 minute"
			sleep 1m
		else
			break
		fi
	done
	printf "$line\t$(echo $return_val | jq -r '[.url] | @tsv')\n" >> data/computer_analysis_urls.tsv
	
	# Sleep 40s to limit to <100 requests / hour
	sleep 40s
	
	done < input_data/input_pgn.tsv

echo "End time: $(date)" >> data/2_get_analyis_urls.log
