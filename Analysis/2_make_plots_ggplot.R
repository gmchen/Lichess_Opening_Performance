library(reshape2)
library(tidyverse)

setwd("~/Box/repos/Lichess_Opening_Performance/Analysis/")

game_results <- read.delim("../data/game_results_with_computer_analysis.tsv", sep="\t", header=TRUE)

game_results %>% filter(rating_group=="all_lichess_users") %>% 
  filter(total_games >= 100) %>% 
  filter(!is.na(computer_analysis_cp)) %>% 
  ggplot(aes(x=computer_analysis_cp, y=white_win_proportion, label=name.unique)) + 
  geom_point() +
  geom_text() +
  theme_bw()

game_results %>% filter(rating_group=="all_lichess_users") %>% left_join(computer_eval) %>% ggplot(aes(x=computer_analysis_cp, y=white_win_proportion)) + geom_point()


game_results %>% filter(rating_group=="masters") %>% 
  filter(total_games >= 100) %>% 
  filter(!is.na(computer_analysis_cp)) %>% 
  ggplot(aes(x=computer_analysis_cp, y=white_win_proportion, label=name.unique)) + 
  geom_point() +
  #geom_text() +
  theme_bw()


scatterplots.by.rating <- lapply(c("1600", "1800", "2000", "2200", "2500", "masters"), function(current.rating) {
  current_game_results <- game_results %>% 
    group_by(name.unique, rating_group) %>% 
    #summarise(white_wins = sum(white_wins), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    filter(rating_group==current.rating & time_control %in% c("blitz", "any")) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(white_win_odds = white_wins / black_wins) %>%
    ungroup() %>%
    mutate(residuals = lm(formula=white_win_proportion ~ computer_analysis_cp) %>% residuals())
  
  ggplot(current_game_results, aes(x=computer_analysis_cp, y=white_win_proportion, size = total_games, colour = rank(residuals), label=name.unique)) + 
    geom_point(shape=1) +
    #geom_point() +
    scale_size_continuous(range = c(1,5), trans="log10") +
    #geom_text() +
    geom_smooth(method='lm', formula= y~x, show.legend = FALSE, se=FALSE, color="black", size=0.5, fullrange=TRUE) +
    theme_classic() +
    scale_color_distiller(palette = "RdYlBu") +
    geom_vline(xintercept=c(0), linetype="dotted") +
    geom_hline(yintercept=c(0.5), linetype="dotted") +
    scale_x_continuous(limits = c(-600,600), breaks = c(seq(-500,500,100))) +
    scale_y_continuous(limit = c(-0.1, 1.1), breaks = c(seq(0,1,0.25)))
  #theme(panel.grid.minor.x = element_blank(),
  #      panel.grid.minor.y = element_blank())
})

cowplot::plot_grid(plotlist=scatterplots.by.rating, ncol = 3)

scatterplots.by.time.control <- lapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {
  current_game_results <- game_results %>% 
    group_by(name.unique, time_control) %>% 
    summarise(white_wins = sum(white_wins), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    filter(time_control==time.control) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(white_win_odds = white_wins / black_wins) %>%
    ungroup() %>%
    mutate(residuals = lm(formula=white_win_proportion ~ computer_analysis_cp) %>% residuals())
  
  # Label the top 10 and bottom 20 residuals
  #idx <- c(head(order(current_game_results$residuals), n=10), tail(order(current_game_results$residuals), n=10))
  idx <- chull(current_game_results$computer_analysis_cp, current_game_results$white_win_proportion)
  current_game_results$labels <- NA
  current_game_results$labels[idx] <- current_game_results$name.unique[idx]
  
  ggplot(current_game_results, aes(x=computer_analysis_cp, y=white_win_proportion, colour = rank(residuals), label=labels)) + 
    geom_point(aes(size = total_games), shape=1) +
    #geom_point() +
    scale_size_continuous(range = c(1,5), trans="log10", labels=scales::comma) +
    #geom_text_repel(size=2, max.overlaps = Inf, min.segment.length = unit(0, 'lines'), label.padding=0, box.padding=2, force_pull = 0.01) +
    geom_smooth(method='lm', formula= y~x, show.legend = FALSE, se=FALSE, color="black", size=0.5, fullrange=TRUE) +
    theme_classic() +
    scale_color_distiller(palette = "RdYlBu") +
    geom_vline(xintercept=c(0), linetype="dotted") +
    geom_hline(yintercept=c(0.5), linetype="dotted") +
    scale_x_continuous(name = "Computer Evaluation: Centipawns", limits = c(-600,600), breaks = c(seq(-500,500,100))) +
    scale_y_continuous(name = "Percent of Games Won for White", limit = c(-0.1, 1.1), breaks = c(seq(0,1,0.25)), labels = scales::percent) +
    guides(color = "none", size=guide_legend("Number of games"))
  #theme(panel.grid.minor.x = element_blank(),
  #      panel.grid.minor.y = element_blank())
})

scatterplots.by.time.control.and.rating <- lapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {
  lapply(c("1600", "1800", "2000", "2200", "2500"), function(current.rating) {
    current_game_results <- game_results %>% 
      filter(time_control==time.control & rating_group == current.rating) %>% 
      left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
      filter(total_games >= 100) %>% 
      filter(!is.na(computer_analysis_cp)) %>%
      mutate(white_win_proportion = white_wins / total_games) %>% 
      mutate(white_win_odds = white_wins / black_wins) %>%
      ungroup() %>%
      mutate(residuals = lm(formula=white_win_proportion ~ computer_analysis_cp) %>% residuals())
    
    ggplot(current_game_results, aes(x=computer_analysis_cp, y=white_win_proportion, size = total_games, colour = rank(residuals), label=name.unique)) + 
      geom_point(shape=1) +
      #geom_point() +
      scale_size_continuous(range = c(1,5), trans="log10") +
      #geom_text() +
      geom_smooth(method='lm', formula= y~x, show.legend = FALSE, se=FALSE, color="black", size=0.5, fullrange=TRUE) +
      theme_classic() +
      scale_color_distiller(palette = "RdYlBu") +
      geom_vline(xintercept=c(0), linetype="dotted") +
      geom_hline(yintercept=c(0.5), linetype="dotted") +
      scale_x_continuous(limits = c(-600,600), breaks = c(seq(-500,500,100))) +
      scale_y_continuous(limit = c(-0.1, 1.1), breaks = c(seq(0,1,0.25))) +
      ggtitle(paste0(current.rating, " ", time.control))
    #theme(panel.grid.minor.x = element_blank(),
    #      panel.grid.minor.y = element_blank())
  })
})

p <- cowplot::plot_grid(plotlist=unlist(scatterplots.by.time.control.and.rating, recursive = F), ncol = 5)

ggsave("scatterplots_by_timecontrol_rating.pdf", p, width=30, height=20)

scatterplots.between_rating_groups <- lapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {
  current_game_results <- game_results %>% 
    group_by(name.unique, time_control) %>% 
    summarise(white_wins = sum(white_wins), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    filter(time_control==time.control) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(white_win_odds = white_wins / black_wins) %>%
    ungroup() %>%
    mutate(residuals = lm(formula=white_win_proportion ~ computer_analysis_cp) %>% residuals())
  
  ggplot(current_game_results, aes(x=computer_analysis_cp, y=white_win_proportion, size = total_games, colour = rank(residuals), label=name.unique)) + 
    geom_point(shape=1) +
    #geom_point() +
    scale_size_continuous(range = c(1,5), trans="log10") +
    #geom_text() +
    geom_smooth(method='lm', formula= y~x, show.legend = FALSE, se=FALSE, color="black", size=0.5, fullrange=TRUE) +
    theme_classic() +
    scale_color_distiller(palette = "RdYlBu") +
    geom_vline(xintercept=c(0), linetype="dotted") +
    geom_hline(yintercept=c(0.5), linetype="dotted") +
    scale_x_continuous(limits = c(-600,600), breaks = c(seq(-500,500,100))) +
    scale_y_continuous(limit = c(-0.1, 1.1), breaks = c(seq(0,1,0.25)))
  #theme(panel.grid.minor.x = element_blank(),
  #      panel.grid.minor.y = element_blank())
})

regression.slope.stats.by.time.control <- sapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {
  lm.obj <- game_results %>% 
    group_by(name.unique, time_control) %>% 
    summarise(white_wins = sum(white_wins), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    filter(time_control==time.control) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(white_win_odds = white_wins / black_wins) %>%
    ungroup() %>%
    lm(formula=white_win_proportion ~ computer_analysis_cp)
  
  lm.obj.summary <- summary(lm.obj)
  coef <- lm.obj.summary$coefficients["computer_analysis_cp","Estimate"]
  coef.se <- lm.obj.summary$coefficients["computer_analysis_cp","Std. Error"]
  confint.95 <- confint(lm.obj, "computer_analysis_cp")
  
  return(c(coef=coef, coef.se=coef.se, lcb.95=confint.95[1], ucb.95=confint.95[2]))
})

regression.slope.stats.by.time.control <- as.data.frame(t(regression.slope.stats.by.time.control))

regression.slope.stats.by.time.control$time_control <- rownames(regression.slope.stats.by.time.control)
regression.slope.stats.by.time.control$time_control <- factor(regression.slope.stats$time_control, levels=c("bullet", "blitz", "rapid", "classical"))

ggplot(regression.slope.stats.by.time.control, aes(x=time_control, y=coef * 1e4)) +
  geom_errorbar(aes(ymin=lcb.95*1e4, ymax=ucb.95*1e4), width=0.5) +
  geom_bar(stat="identity", fill="grey", color="black") +
  theme_classic() +
  scale_y_continuous(limit = c(0, 14), breaks = c(seq(0,15,2)), expand = c(0,0)) +
  ylab("Increase in win percentage per 100 centipawn")

regression.slope.stats.by.rating <- sapply(c("1600", "1800", "2000", "2200", "2500", "masters"), function(current.rating) {
  lm.obj <- game_results %>% 
    group_by(name.unique, rating_group) %>% 
    summarise(white_wins = sum(white_wins), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    filter(rating_group==current.rating) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(white_win_odds = white_wins / black_wins) %>%
    ungroup() %>%
    lm(formula=white_win_proportion ~ computer_analysis_cp)
  
  lm.obj.summary <- summary(lm.obj)
  coef <- lm.obj.summary$coefficients["computer_analysis_cp","Estimate"]
  coef.se <- lm.obj.summary$coefficients["computer_analysis_cp","Std. Error"]
  confint.95 <- confint(lm.obj, "computer_analysis_cp")
  
  return(c(coef=coef, coef.se=coef.se, lcb.95=confint.95[1], ucb.95=confint.95[2]))
})

regression.slope.stats.by.rating <- as.data.frame(t(regression.slope.stats.by.rating))

regression.slope.stats.by.rating$rating_group <- rownames(regression.slope.stats.by.rating)
regression.slope.stats.by.rating$rating_group <- factor(regression.slope.stats.by.rating$rating_group, levels=c("1600", "1800", "2000", "2200", "2500", "masters"))

###
regression.slope.stats.by.time.control.and.rating <- lapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {
  out.list <- lapply(c("1600", "1800", "2000", "2200", "2500"), function(current.rating) {
    lm.obj <- game_results %>% 
      group_by(name.unique, rating_group) %>% 
      filter(time_control==time.control & rating_group == current.rating) %>% 
      left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
      filter(total_games >= 100) %>% 
      filter(!is.na(computer_analysis_cp)) %>%
      mutate(white_win_proportion = white_wins / total_games) %>% 
      mutate(white_win_odds = white_wins / black_wins) %>%
      ungroup() %>%
      lm(formula=white_win_proportion ~ computer_analysis_cp)
    
    lm.obj.summary <- summary(lm.obj)
    coef <- lm.obj.summary$coefficients["computer_analysis_cp","Estimate"]
    coef.se <- lm.obj.summary$coefficients["computer_analysis_cp","Std. Error"]
    confint.95 <- confint(lm.obj, "computer_analysis_cp")
    pval <- lm.obj.summary$coefficients["computer_analysis_cp","Pr(>|t|)"]
    
    return(list(coef=coef, coef.se=coef.se, lcb.95=confint.95[1], ucb.95=confint.95[2], pval=pval))
  })
  names(out.list) <- paste0("rating.", c("1600", "1800", "2000", "2200", "2500"))
  return(out.list)
})
names(regression.slope.stats.by.time.control.and.rating) <- c("bullet", "blitz","rapid", "classical")

regression.slope.stats.by.time.control.and.rating <- melt(regression.slope.stats.by.time.control.and.rating)
regression.slope.stats.by.time.control.and.rating <- dcast(regression.slope.stats.by.time.control.and.rating, L1 + L2 ~ L3)
colnames(regression.slope.stats.by.time.control.and.rating)[1:2] <- c("time_control", "rating_group")
regression.slope.stats.by.time.control.and.rating$rating_group <- sub("rating.", "", regression.slope.stats.by.time.control.and.rating$rating_group)

regression.slope.stats.by.time.control.and.rating$rating_group <- factor(regression.slope.stats.by.time.control.and.rating$rating_group, levels=c("1600", "1800", "2000", "2200", "2500"))
regression.slope.stats.by.time.control.and.rating$time_control<- factor(regression.slope.stats.by.time.control.and.rating$time_control, levels=c("bullet", "blitz", "rapid", "classical"))

# For visualization purposes, only show top or bottom error bar
regression.slope.stats.by.time.control.and.rating$lcb.95[regression.slope.stats.by.time.control.and.rating$coef > 0] <- regression.slope.stats.by.time.control.and.rating$coef[regression.slope.stats.by.time.control.and.rating$coef > 0]
regression.slope.stats.by.time.control.and.rating$ucb.95[regression.slope.stats.by.time.control.and.rating$coef < 0] <- regression.slope.stats.by.time.control.and.rating$coef[regression.slope.stats.by.time.control.and.rating$coef < 0]

# Remove 2500 classical, since the upper 95% confidence interval is something like 79%
regression.slope.stats.by.time.control.and.rating <- regression.slope.stats.by.time.control.and.rating[-15,]

ggplot(regression.slope.stats.by.time.control.and.rating, aes(x=time_control, y=coef * 1e4, fill=rating_group)) +
  geom_errorbar(aes(ymin=lcb.95*1e4, ymax=ucb.95*1e4), position=position_dodge(width=0.9), width=0.2) +
  geom_bar(position=position_dodge(width=0.9), stat="identity", color="black") +
  theme_classic() +
  scale_y_continuous(limit = c(-1, 14), breaks = c(seq(0,15,2)), expand = c(0,0)) +
  ylab("Increase in win percentage per 100 centipawn")

# Is regression slope significantly influenced by rating and/or time control
lm.out <- game_results %>%
  filter(time_control != "any" & rating_group %in% c("1600", "1800", "2000", "2200", "2500")) %>%
  lm(formula=white_win_proportion ~ name.unique + computer_analysis_cp * time_control * rating_group)

game_results %>% 
  group_by(name.unique, time_control) %>% 
  summarise(white_wins = sum(white_wins), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
  mutate(white_win_proportion = white_wins / total_games)
#filter(total_games >= 100) %>% 
#filter(!is.na(computer_analysis_cp)) %>%

game_results %>%
  filter(rating_group=="all_lichess_users") %>%
  filter(total_games >= 100) %>% 
  filter(!is.na(computer_analysis_cp)) %>%
  ggplot(aes(x = computer_analysis_cp, y=draw_proportion)) +
  geom_point() +
  theme_bw()

head(sort(openings), n=20)
head(rev(sort(openings)), n=20)
