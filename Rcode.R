library(readr)
library(rbokeh)
library(tidyverse)
library(tidyr)
library(dbplyr)
library(rmarkdown)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(htmlwidgets)
library(htmltools)
library(shiny)
library(DT)

books <- read.csv("Books_2020.csv")
books$month <- factor(books$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Maps with authors' nationality

labels <- sprintf(
  "%s<br/><strong>%s</strong>",
  books$nationality_author, books$author
) %>% lapply(htmltools::HTML)
books %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = books$lng, lat = books$lat,
                   color = "blue",
                   weight = 1, 
                   fillColor = "blue",
                   fillOpacity = 0.2,
                   clusterOptions = markerClusterOptions(),
                   label = labels)

# Gauge with average rating
average_rating <- round(mean(books$rating), 1)
gauge(average_rating, min = 0, max = 5)

# Bar chart with number of books per month
books %>%
  count(month) %>%
  plot_ly(x = ~month, y = ~n, type = 'bar',
          marker = list(color = '#CC99FF', opacity = 0.4,
                        line = list(color = '#330066', width = 1)),
          hoverinfo = "text",
          text = ~paste0(month, ": ", n, " books read")) %>%
  layout(xaxis = list(title = " ", tickangle = 90),
         yaxis = list(showgrid = FALSE, title = "Number of books read")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)

# Donut charts with stats
colors <- c('#FFCC33', '#CC99FF', '#CCCCCC', '#FF9966', '#FFCCFF')
plot_ly(insidetextorientation = 'horizontal',
        textposition = 'inside') %>%
  add_pie(data = count(books, sex_author), values = ~n,
          hole = 0.4,
          domain = list(row = 0, column = 0),
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
          title = "Gender",
          textinfo = 'percent',
          hoverinfo = 'text', text = ~paste0(sex_author, ": ", n)) %>%
  add_pie(data = count(books, support), values = ~n,
          hole = 0.4,
          name = "Support", domain = list(row = 1, column = 0),
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
          title = "Support",
          textinfo = 'percent',
          hoverinfo = 'text', text = ~paste0(support, ": ", n)) %>%
  add_pie(data = count(books, genre), labels = ~genre, values = ~n,
          hole = 0.4,
          name = "Genre", domain = list(row = 2, column = 0),
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
          title = "Genre",
          textinfo = 'percent',
          hoverinfo = 'text', text = ~paste0(genre, ": ", n)) %>%
  add_pie(data = count(books, lenguage), labels = ~lenguage, values = ~n,
          hole = 0.4,
          name = "Lenguage", domain = list(row = 3, column = 0),
          marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
          title = "Language",
          textinfo = 'percent',
          hoverinfo = 'text', text = ~paste0(lenguage, ": ", n)) %>%
  layout(showlegend = F,
         grid=list(rows=4, columns=1),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)

# Interactive DT table with all data
books %>%
  select(-lat, -lng) %>%
  datatable(rownames = FALSE, 
            colnames = c("Title", "Author", "Author's gender", "Author's nationality", "Language", "Genre", "Month", "Rating", "Support"), 
            options = list(pageLength = nrow(books)))
