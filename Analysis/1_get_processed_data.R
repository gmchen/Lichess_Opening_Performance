library(tidyverse)
library(bigchess)

setwd("~/Box/repos/Lichess_Opening_Performance/Analysis/")

game_results <- read.delim("../lichess-api-queries/data/game_results.tsv", sep="\t", header=TRUE)
computer_eval <- read.delim("../lichess-api-queries/data/computer_eval.tsv", sep="\t", header=TRUE)
computer_analysis_urls <- read.delim("../lichess-api-queries/data/computer_analysis_urls.tsv", sep="\t", header=TRUE)
computer_eval$computer_analysis_cp[computer_eval$computer_analysis_cp == "null"] <- NA
computer_eval$computer_analysis_cp <- as.numeric(computer_eval$computer_analysis_cp)

eco_openings_a <- read.delim("../eco_openings/a.tsv", sep="\t", header=TRUE)
eco_openings_b <- read.delim("../eco_openings/b.tsv", sep="\t", header=TRUE)
eco_openings_c <- read.delim("../eco_openings/c.tsv", sep="\t", header=TRUE)
eco_openings_d <- read.delim("../eco_openings/d.tsv", sep="\t", header=TRUE)
eco_openings_e <- read.delim("../eco_openings/e.tsv", sep="\t", header=TRUE)

eco_openings <- rbind(eco_openings_a, eco_openings_b, eco_openings_c, eco_openings_d, eco_openings_e)


rm(eco_openings_a, eco_openings_b, eco_openings_c, eco_openings_d, eco_openings_e)

hist(computer_eval$computer_analysis_cp, breaks = 100)
boxplot(computer_eval$computer_analysis_cp, breaks = 100)

computer_eval[order(computer_eval$computer_analysis_cp),]

# Add a number to duplicated opening names
make.unique.2 = function(x, sep='.'){
  ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
}
eco_openings$name.unique <- make.unique.2(eco_openings$name, sep=" ")

if(!all(computer_eval$name == eco_openings$name)) {
  stop("ECO opening names do not match computer_eval.tsv")
}
computer_eval$name.unique <- eco_openings$name.unique

computer_eval <- computer_eval[,c("eco", "name.unique", "computer_analysis_cp")]

if(!all(game_results$name == rep(eco_openings$name, each=22))) {
  stop("ECO opening names do not match game_results.tsv")
}
game_results$name.unique <- rep(eco_openings$name.unique, each=22)

game_results <- game_results[,c("eco", "name.unique", "time_control", "rating_group", "white_wins", "draw", "black_wins")]

game_results$total_games <- game_results$white_wins + game_results$draw + game_results$black_wins

game_results$white_win_proportion <- game_results$white_wins / game_results$total_games
game_results$draw_proportion <- game_results$draw / game_results$total_games
game_results$black_win_proportion <- game_results$black_wins / game_results$total_games

game_results <- game_results %>% left_join(computer_eval)

# Add the position number for computer_analysi_urls
computer_analysis_urls$last_move_number <- sapply(computer_analysis_urls$san, function(x) {
  moves <- extract_moves(x, N = 100, last.move = FALSE)
  if(!any(is.na(moves))) {
    stop("Need to set higher N for bigchess::extract_moves")
  }
  return(sum(!is.na(moves)) - 1)
  })

write.table(game_results, file="../data/game_results_with_computer_analysis.tsv", row.names = FALSE, quote=FALSE, sep="\t")
write.table(computer_eval, file="../data/computer_eval_unique_names.tsv", sep="\t", row.names = FALSE, quote=FALSE)
write.table(eco_openings, file="../data/eco_openings_unique_names.tsv", sep="\t", row.names = FALSE, quote=FALSE)
write.table(computer_analysis_urls, file="../data/computer_analysis_urls_with_last_move.tsv", sep="\t", row.names = FALSE, quote=FALSE)
