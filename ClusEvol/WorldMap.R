#-----------------------------------------------------------------
# Enterprise: 
# Proyect: WorldMap GT
# Author: Víctor Morales Oñate
# Date: 24-09-2019
#-----------------------------------------------------------------
rm( list = ls())
cat("\014")
gc()
graphics.off()

global.dir = ("~/Documents/Ayudas/Gi/Mapas")
code.dir <- paste(global.dir,"/Code", sep = "")
res.dir <- paste(global.dir,"/Results", sep = "")
data.dir <- paste(global.dir,"/Data", sep = "")


setwd(data.dir)
population_records <- readLines("API_SP.POP.TOTL_DS2_en_csv_v2_180567.csv")[-(1:4)]
population_data <- read.csv(text=population_records, header = TRUE)



population_data$Growth.5.Year <- 
  ((population_data$X2014 - population_data$X2009) / population_data$X2014) * 100


library(rworldmap)

population_data$Country.Code

mapped_data <- joinCountryData2Map(population_data, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")

mapCountryData(mapped_data, nameColumnToPlot = "Growth.5.Year")



qq = quantile(population_data$Growth.5.Year,na.rm = TRUE)
population_data$qqGrow = (cut(population_data$Growth.5.Year,qq ,labels = c(1,2,3,4)))

table(population_data$qqGrow)
mapped_data <- joinCountryData2Map(population_data, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")

# Colores en paleta:
mapCountryData(mapped_data, 
               nameColumnToPlot = "qqGrow",
               catMethod='categorical',
               colourPalette = "topo"
)


# Colores independientes:
mapCountryData(mapped_data, 
               nameColumnToPlot = "qqGrow",
               catMethod='categorical',
               colourPalette = c("blue","green","black","yellow")
               )



# datos = population_data[,c(1,2)]
# setwd(res.dir)
# rio::export(datos, file = "CodigoPaises.xlsx")





library(highcharter)
hcmap("custom/world-palestine-highres")


library(dplyr)
mapdata <- get_data_from_map(download_map_data("custom/world-palestine-highres"))
glimpse(mapdata)




data_ecuador <- mapdata %>% 
  select(PROVINCIA = `hc-a2`) %>% 
  mutate(X = 1:215, Cat = c(rep(1,100),rep(2,80),rep(3,35)))
glimpse(data_ecuador)


series <- data_ecuador %>% 
  group_by(name = Cat) %>% 
  do(data_ecuador = list_parse(select(., PROVINCIA))) %>%
  ungroup() %>% 
  mutate(color = c("red", "darkred", "pink"))


hcmap("custom/world-palestine-highres", data = data_ecuador, value = "X",
      joinBy = c("hc-a2", "PROVINCIA"),
      dataLabels = list(enabled = TRUE, format = '{point.name}')) 



# **************************** ST: Hig
rm( list = ls())
library(highcharter)
library(dplyr)
mapdata <- get_data_from_map(download_map_data("custom/world-palestine-highres"))
glimpse(mapdata)
mapdata$`iso-a3`


data_ecuador <- mapdata %>% 
  select(PROVINCIA = `hc-a2`) %>% 
  mutate(X = 1:215, Cat = c(rep(1,100),rep(2,80),rep(3,35)))
glimpse(data_ecuador)


hcmap("custom/world-palestine-highres", data = data_ecuador, value = "Cat",
      joinBy = c("hc-a2", "PROVINCIA"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "lightblue", borderWidth = 0.5,
      tooltip = list(valueDecimals = 2, valuePrefix = "R$")
      ) %>%
  # hc_add_series(series)%>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Mapa Categorico",
           align = "center", style = list(color = "#000000", fontWeight = "bold"))%>%
  hc_colorAxis(dataClasses = color_classes(c(1,2,3,4)))%>%
hc_legend(layout = "horizontal", align = "bottom", valueDecimals = 2) 


# **************************** END: Hig

highchart(type = "map") %>% 
  hc_plotOptions(map = list(
    allAreas = FALSE,
    joinBy = c("hc-a2", "PROVINCIA"),
    data = data_ecuador,
    map = "custom/world-palestine-highres"
  )) %>% 
  hc_add_series_list(series)


# https://stackoverflow.com/questions/40877282/highcharter-map-with-categorical-variables-in-r

provincia = "custom/world-palestine-highres"

highchart(type = "map") %>%
  hc_tooltip(followPointer =  FALSE) %>%
  hc_add_series_map(provincia, data_ecuador, name = "Total Procedimientos",
                    value = "Total", joinBy = c("name", "Provincia"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.name}')) %>%
  hc_legend(enabled=FALSE) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_sandsignika())





http://rpubs.com/biapinna/maps_high