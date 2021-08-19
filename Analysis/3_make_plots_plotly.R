library(reshape2)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(htmltools)

setwd("~/Box/repos/Lichess_Opening_Performance/Analysis/")

game_results <- read.delim("../data/game_results_with_computer_analysis.tsv", sep="\t", header=TRUE)
eco_openings <- read.delim("../data/eco_openings.tsv", sep="\t", header=TRUE)
computer_eval <- read.delim("../data/computer_eval_unique_names.tsv", sep="\t", header=TRUE)

#scatterplots.by.time.control <- lapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {
time.control <- "rapid"
  current_game_results <- game_results %>% 
    group_by(name.unique, time_control) %>% 
    summarise(white_wins = sum(white_wins), draw = sum(draw), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    filter(time_control==time.control) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    left_join(eco_openings %>% select(name.unique, fen)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(draw_proportion = draw / total_games) %>% 
    mutate(black_win_proportion = black_wins / total_games) %>% 
    ungroup() %>%
    mutate(residuals = lm(formula=white_win_proportion ~ computer_analysis_cp) %>% residuals())
  
  current_game_results$lichess_url <- sapply(paste0("https://lichess.org/analysis/", current_game_results$fen), URLencode)
  
  current.lm <- lm(white_win_proportion ~ computer_analysis_cp, current_game_results)
  
  # Functions for horizontal and vertical dotted lines
  vline <- function(x = 0, color = "black") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color, width=1, dash="dot")
    )
  }
  
  hline <- function(y = 0, color = "black") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y, 
      line = list(color = color, width=1, dash="dot")
    )
  }
  
  # Buttons for zooming into quadrants
  # updatemenus component
  updatemenus <- list(
    list(
      active = 0,
      type = 'buttons',
      buttons = list(
        
        list(
          label = "All quadrants",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        list(
          label = "Upper-left quadrant",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 30), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0.475, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        list(
          label = "Upper-right quadrant",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-30, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0.475, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        
        list(
          label = "Lower-left quadrant",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 30), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 0.525), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        
        list(
          label = "Lower-right quadrant",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-30, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 0.525), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            )))
      )
    )
  )
  
  fig <- plot_ly(current_game_results) %>%
    layout(shapes = list(vline(0), hline(0.5)))
  
  fig <- fig %>% add_trace(
    x = ~computer_analysis_cp,
    y = ~white_win_proportion,
    type = 'scatter',
    mode = 'markers',
    color = ~rank(white_win_proportion), 
    #size = ~log10(total_games),
    #sizes = c(5, 100),
    #sizemode="diameter",
    marker = list(size = log10(current_game_results$total_games) * 3, opacity = 0.5, sizemode = 'diameter'),
    colors = "RdYlBu",
    text = ~paste(
          name.unique, 
          '<br>Total games:', total_games, 
          '<br>White win:', scales::percent(white_win_proportion, accuracy=1), 
          '<br>Draw:', scales::percent(draw_proportion, accuracy = 1), 
          '<br>Black win:', scales::percent(black_win_proportion, accuracy = 1), 
          '<br>Computer evaluation (centipawns):', computer_analysis_cp),
    hoverinfo = 'text',
    custom_url = current_game_results$lichess_url
    ) %>% 
    #config(scrollZoom = TRUE) %>%
    layout(
      xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
      yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25, scaleanchor = "x", scaleratio = 1/300),
      #autosize = T, width = 800, height = 500,
      updatemenus = updatemenus
      )
  
  fig <- fig %>% add_trace(
    x = ~computer_analysis_cp, 
    y = ~predict(current.lm), type = 'scatter', mode = 'lines',
    line=list(color = "black")
    ) %>% 
    hide_colorbar() %>%
    layout(showlegend = FALSE)
  
  # Click to open lichess url
  js <- "
        function(el, x) {
          el.on('plotly_click', function(d) {
            var point = d.points[0];
            var url = point.data.custom_url[point.pointIndex];
            window.open(url);
          });
        }"
  
  fig <- fig %>% onRender(js)
  
  fig
  
  saveWidget(fig, "fig.html", selfcontained = F, libdir = "lib")
#})
