library(reshape2)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(htmltools)

setwd("~/Box/repos/Lichess_Opening_Performance/Analysis/")

game_results <- read.delim("../data/game_results_with_computer_analysis.tsv", sep="\t", header=TRUE)
eco_openings <- read.delim("../data/eco_openings.tsv", sep="\t", header=TRUE)
computer_eval <- read.delim("../data/computer_eval_unique_names.tsv", sep="\t", header=TRUE)
computer_analysis_urls <- read.delim("../data/computer_analysis_urls_with_last_move.tsv", sep="\t", header=TRUE)


#scatterplots.by.time.control <- lapply(c("bullet", "blitz","rapid", "classical"), function(time.control) {

# Use time control as the dropdown variable containing Bullet, Blitz, Rapid, Classical, Masters
#game_results$time_control[game_results$time_control == "bullet"] <- "Lichess Games: Bullet"
#game_results$time_control[game_results$time_control == "blitz"] <- "Lichess Games: Blitz"
#game_results$time_control[game_results$time_control == "rapid"] <- "Lichess Games: Rapid"
#game_results$time_control[game_results$time_control == "classical"] <- "Lichess Games: Classical"
#game_results$time_control[game_results$time_control == "any"] <- "Lichess Games: All"
game_results$time_control[game_results$rating_group == "masters"] <- "masters"

game_results <- game_results %>% 
    group_by(name.unique, time_control) %>% 
    summarise(white_wins = sum(white_wins), draw = sum(draw), black_wins = sum(black_wins), total_games = sum(total_games)) %>% 
    left_join(computer_eval %>% select(name.unique, computer_analysis_cp)) %>%
    left_join(eco_openings %>% select(name.unique, fen)) %>%
    filter(total_games >= 100) %>% 
    filter(!is.na(computer_analysis_cp)) %>%
    mutate(white_win_proportion = white_wins / total_games) %>% 
    mutate(draw_proportion = draw / total_games) %>% 
    mutate(black_win_proportion = black_wins / total_games) %>% 
    ungroup()

# Add analysis board urls
game_results <- game_results %>% 
  left_join(computer_analysis_urls) %>%
  mutate(url.use = url %>% paste0("/", ifelse(white_win_proportion / black_win_proportion > 1, "white", "black"), "#", last_move_number))

for(current.time.control in c("bullet", "blitz", "rapid", "classical", "any", "masters")) {
  print(current.time.control)
  plot.title <- switch(current.time.control, 
                          bullet="Lichess Games: Bullet", 
                          blitz="Lichess Games: Blitz",
                          rapid="Lichess Games: Rapid",
                          classical="Lichess Games: Classical",
                          any="Lichess Games: All",
                          masters="Master Games")
  
  current_game_results <- game_results %>% filter(time_control == current.time.control)
  
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
      direction = "right",
      xanchor = 'center',
      yanchor = "bottom",
      pad = list('r'= 0, 't'= 5, 'b' = 10),
      x = 0.5,
      y = 1.01,
      buttons = list(
        
        list(
          label = "Default zoom",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        list(
          label = "Upper-left",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 30), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0.475, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        list(
          label = "Upper-right",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-30, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0.475, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        
        list(
          label = "Lower-left",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 30), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 0.525), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            ))),
        
        
        list(
          label = "Lower-right",
          method = "relayout",
          args = list(list(
              xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-30, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tick0 = -600, dtick = 100),
              yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 0.525), zeroline = FALSE, showline=TRUE, showgrid = TRUE, automargin = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25)
            )))
      )
    )
  )
  
  fig <- plot_ly(current_game_results) %>%
    layout(shapes = list(vline(0), hline(0.5)))
  
  fig <- fig %>% add_trace(
#  fig <- plot_ly(current_game_results,
    x = ~computer_analysis_cp,
    y = ~white_win_proportion,
    type = 'scatter',
    mode = 'markers',
    color = ~-rank(white_win_proportion), 
    #size = ~log10(total_games),
    #sizes = c(5, 100),
    #sizemode="diameter",
    marker = list(size = (log10(current_game_results$total_games)-1) * 3+1, opacity = 0.60, sizemode = 'diameter'),
    colors = "RdYlBu",
    text = ~paste(
          name.unique, 
          '<br>Total games:', total_games, 
          '<br>White win:', scales::percent(white_win_proportion, accuracy=1), 
          '<br>Draw:', scales::percent(draw_proportion, accuracy = 1), 
          '<br>Black win:', scales::percent(black_win_proportion, accuracy = 1), 
          '<br>Computer evaluation (centipawns):', computer_analysis_cp),
    hoverinfo = 'text',
    custom_url = current_game_results$url.use
    ) %>% 
    #config(scrollZoom = TRUE) %>%
    layout(
      xaxis = list(title = "Computer Evaluation: Centipawns", fixedrange = TRUE, range = c(-600, 600), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = -600, dtick = 100),
      #yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tickformat = "%", tick0 = 0, dtick = 0.25, scaleanchor = "x", scaleratio = 1/300),
      yaxis = list(title = "Percent of Games Won for White", fixedrange = TRUE, range = c(0, 1), zeroline = FALSE, showline=TRUE, showgrid = TRUE, tick0 = 0, dtick = 0.25, scaleanchor = "x", scaleratio = 1/300),
      #autosize = T, width = 800, height = 500,
      #margin =  list(l = 80, r = 80, b = 80, t = 100, pad = 0),
      updatemenus = updatemenus
      )
  
  fig <- fig %>% add_trace(
    x = ~computer_analysis_cp, 
    y = ~predict(current.lm), type = 'scatter', mode = 'lines',
    line=list(color = "black")
    ) %>% 
    hide_colorbar() %>%
    layout(showlegend = FALSE, x = 60) %>%
    config(displayModeBar = FALSE)
  
  # Click to open lichess url
  js <- "
        function(el, x) {
          el.on('plotly_click', function(d) {
            var point = d.points[0];
            var url = point.data.custom_url[point.pointIndex];
            window.open(url);
          });
        }"
  
  # Search for an opening
  fig <- htmlwidgets::appendContent(fig, htmltools::tags$label("for"='inputText', 'Search for an opening:'))
  fig <- htmlwidgets::appendContent(fig, htmltools::tags$input(id='inputText', value='', ''), htmltools::tags$button(id='buttonSearch', 'Search'), htmltools::tags$button(id='buttonReset', 'Reset'))
  fig <- htmlwidgets::appendContent(fig, htmltools::tags$script(HTML(
    '
    document.getElementById("buttonSearch").addEventListener("click", function() {        
    queryString = document.getElementById("inputText").value.toLowerCase();
    var i = 0;
    var j = 0;
    var found = [];
    var newx = [];
    var newy = [];
    var myDiv = document.getElementsByClassName("js-plotly-plot")[0];
    var data = JSON.parse(document.querySelectorAll("script[type=\'application/json\']")[0].innerHTML);
    for (j = 0; j < data.x.data[i].text.length; j += 1) {
      currentString = data.x.data[i].text[j].toLowerCase();
      if (currentString.indexOf(queryString) !== -1) {
        found.push({curveNumber: i, pointNumber: j});
        newx[j] = data.x.data[i].x[j];
        newy[j] = data.x.data[i].y[j];
      } else {
        newx[j] = -1000000;
        newy[j] = -1000000
      }
    }
    var update = {"x":[newx], "y":[newy]};
    Plotly.restyle(myDiv, update, [0]);
    //Plotly.Fx.hover(myDiv, found);
    });
    
    document.getElementById("buttonReset").addEventListener("click", function() {
      var i = 0;
      var j = 0;
      var myDiv = document.getElementsByClassName("js-plotly-plot")[0];
      var data = JSON.parse(document.querySelectorAll("script[type=\'application/json\']")[0].innerHTML);
      var newx = [];
      var newy = [];
      for (j = 0; j < data.x.data[i].text.length; j += 1) {
        newx[j] = data.x.data[i].x[j];
        newy[j] = data.x.data[i].y[j];
      }
      var update = {"x":[newx], "y":[newy]};
      Plotly.restyle(myDiv, update, [0]);
    });
    document.getElementById("inputText").addEventListener("keyup", function(event) {
      // Number 13 is the "Enter" key on the keyboard
      if (event.keyCode === 13) {
      // Cancel the default action, if needed
      event.preventDefault();
      // Trigger the button element with a click
      document.getElementById("buttonSearch").click();
      }
    });
  ')))                                                    
  
  fig$sizingPolicy$browser$fill <- TRUE
  fig$sizingPolicy$viewer$fill <- TRUE
  fig$sizingPolicy$browser$padding <- 30
  fig$sizingPolicy$viewer$padding <- 30
  fig$sizingPolicy$defaultWidth <- 800
  fig$sizingPolicy$defaultHeight <- 700
  
  fig <- fig %>% onRender(js)
  
  fig
  
  #saveWidget(fig, "fig.html", selfcontained = F, libdir = "lib")
  saveWidget(fig, paste0("output/plotly/white_win_proportion_vs_computer_eval_", current.time.control, ".html"), selfcontained = FALSE)
  #})
}

