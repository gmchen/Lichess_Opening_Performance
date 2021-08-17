#!/bin/bash

echo "Start time: $(date)" >> data/query.log

printf "eco	name	time_control	rating_group	white_wins	draw	black_wins\n" > data/game_results.tsv
printf "eco	name	computer_analysis_cp\n" > data/computer_eval.tsv

for filename in ../eco_openings/a.tsv ../eco_openings/b.tsv ../eco_openings/c.tsv ../eco_openings/d.tsv ../eco_openings/e.tsv
do
	while IFS=$'\t' read -r -a myArray
	do
		line="${myArray[0]}	${myArray[1]}"
		eco_code=${myArray[0]}
		fen=${myArray[2]}
		echo $eco_code
		echo $fen
		if [[ "$eco_code" == "eco" ]]; then
			continue
		fi
		
		master_games=""
		while true
		do
			master_games=$(curl -G \
				--data-urlencode "fen=$fen" \
				--data-urlencode "topGames=0" \
				--data-urlencode "moves=0" \
				https://explorer.lichess.ovh/master)
			if [[ "$master_games" =~ .*"429 Too Many Requests".* ]]
			then
	 			echo "Received 429 status - waiting 1 minute"
				sleep 1m
			else
				break
			fi
		done
		printf "$line\tany\tmasters\t$(echo $master_games | jq -r '[.white, .draws, .black] | @tsv')\n" >> data/game_results.tsv
		
		lichess_alluser_games=""
		while true
		do
			lichess_alluser_games=$(curl -G \
				--data-urlencode "fen=$fen" \
				--data-urlencode "variant=standard" \
				--data-urlencode "speeds[]=bullet" \
				--data-urlencode "speeds[]=blitz" \
				--data-urlencode "speeds[]=rapid" \
				--data-urlencode "speeds[]=classical" \
				--data-urlencode "ratings[]=1600" \
				--data-urlencode "ratings[]=1800" \
				--data-urlencode "ratings[]=2000" \
				--data-urlencode "ratings[]=2200" \
				--data-urlencode "ratings[]=2500" \
				--data-urlencode "recentGames=0" \
				--data-urlencode "moves=0" \
				https://explorer.lichess.ovh/lichess)
			if [[ "$lichess_alluser_games" =~ .*"429 Too Many Requests".* ]]
			then
	 			echo "Received 429 status - waiting 1 minute"
				sleep 1m
			else
				break
			fi
		done
		printf "$line\tany\tall_lichess_users\t$(echo $lichess_alluser_games | jq -r '[.white, .draws, .black] | @tsv')\n" >> data/game_results.tsv

		for time_control in "bullet" "blitz" "rapid" "classical"
		do
			for rating_group in 1600 1800 2000 2200 2500
			do
				lichess_current_games=""
				while true
				do
					lichess_current_games=$(curl -G \
						--data-urlencode "fen=$fen" \
						--data-urlencode "variant=standard" \
						--data-urlencode "speeds[]=${time_control}" \
						--data-urlencode "ratings[]=${rating_group}" \
						--data-urlencode "recentGames=0" \
						--data-urlencode "moves=0" \
						https://explorer.lichess.ovh/lichess)
					if [[ "$lichess_current_games" =~ .*"429 Too Many Requests".* ]]
					then
			 			echo "Received 429 status - waiting 1 minute"
						sleep 1m
					else
						break
					fi
				done
				printf "$line\t${time_control}\t${rating_group}\t$(echo $lichess_current_games | jq -r '[.white, .draws, .black] | @tsv')\n" >> data/game_results.tsv
			done
		done
		while true
		do
			computer_analysis=""
			computer_analysis=$(curl -G \
				--data-urlencode "fen=$fen" \
				https://lichess.org/api/cloud-eval)
			if [[ "$computer_analysis" =~ .*"429 Too Many Requests".* ]]
			then
		 		echo "Received 429 status - waiting 1 minute"
				sleep 1m
			else
				break
			fi
		done
		printf "$line\t$(echo $computer_analysis | jq -r '.pvs[0].cp')\n" >> data/computer_eval.tsv
		
	done < $filename
done

echo "End time: $(date)" >> data/query.log
