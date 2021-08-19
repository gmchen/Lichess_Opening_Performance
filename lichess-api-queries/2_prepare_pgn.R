library("bigchess")
# This is the concatenated list of ECO openings with unique names
eco_openings <- read.delim("../data/eco_openings.tsv", sep="\t", header=TRUE)
eco_openings$san <- sapply(eco_openings$moves, bigchess::lan2san)

input_pgn <- eco_openings[,c("name.unique", "san")]

write.table(input_pgn, file="input_data/input_pgn.tsv", row.names = FALSE, quote=FALSE, sep="\t")
