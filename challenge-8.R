# Challenge #8: Inzidenzzahlen der Hamburger Bezirke

# Lade den Datensatz stadtteile_wsg84.RDS
# Recherchiere die Fallzahlen der letzten sieben Tage für die Hamburger Bezirke
# https://www.hamburg.de/corona-zahlen/
# Erstelle eine leaflet Map und visualisiere die Inzidenzzahlen (Achtung: Nicht die Fallzahlen)
# Nutze dafür Shapes, Legende, Hovereffekte und Labels
# Exportiere die Map als HTML file

#load packages
library(dplyr)
library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(htmltools)
library(readxl)


# load data
letzte_7_tage <- c(298, 141, 238, 571, 248, 237, 455)
name_bezirk <- c("Altona", "Bergedorf", "Eimsbüttel", "Hamburg-Mitte", "Hamburg-Nord", "Harburg", "Wandsbek")
letzte_7_tage_df <- data.frame(name_bezirk, letzte_7_tage)

hamburg_districts <- readRDS("data/hamburg_districts.rds")

bezirke <- readRDS("data/bezirke_wsg84.RDS") %>% 
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84')

bezirke <- bezirke %>%
  left_join(hamburg_districts, by = c("Bezirk_Name" = "bezirk")) %>% 
  left_join(letzte_7_tage_df, by = c("Bezirk_Name" = "name_bezirk")) %>% 
  mutate(inzidenz = round(letzte_7_tage / einwohner * 100000))

# plot map
bins <- c(50, 10, 150, 200, 250, 300)
pal <- colorBin("YlOrRd", domain = bezirke$inzidenz, bins = bins)


labels <- sprintf("<strong>%s</strong><br>Inzidenzwert: %g<br>Neuinfektionen: %g", 
                  bezirke$Bezirk_Name,
                  bezirke$inzidenz,
                  bezirke$letzte_7_tage) %>% 
  map(HTML)

hamburg_coronamap <- leaflet(data = bezirke) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(9.993682, 53.551086, zoom = 10) %>% 
  addPolygons(fillColor = ~pal(inzidenz),
              weight = 2,
              opacity = 1,
              color = "white",
              fillOpacity = 0.75,
              highlight = highlightOptions(
                weight = 3,
                color = "lightgrey",
                bringToFront = TRUE,
                fillOpacity = 0.8),
              label = labels) %>% 
  addLegend(pal = pal, values = ~inzidenz,
            title = "7-Tage-Inzidenz", position = "bottomright")


# safe map
hamburg_coronamap
htmlwidgets::saveWidget(as_widget(hamburg_coronamap), "hamburg_coronamap.html")

