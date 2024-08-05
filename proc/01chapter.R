# Chapter 1
#1. Cargar paquetes 

if (!require("pacman"))install.packages("pacman")

#1.1 Manipulacion general
pacman::p_load("tidyverse","dplyr","ggplot2",
               "haven","forcats","ggsci",
               "hrbrthemes",
               "gganimate", "broom", "plotly", "magick",
               "png","gifski", "dygraphs") #interactive 

#1.2 Manipulacion de mapas
pacman::p_load("sf","raster","spData","spDataLarge","leaflet", 
               "htmlwidgets", "tmap", "mapview", "shiny", "maps")


library(gganimate)

remotes::install_github("ropensci/geojsonio")
library()


#2. Cargar bases de datos
load(file = "input/data/database_FDL.RData")

# 3. Figuras 2.1 y Mapa
# Gif y Mapa interactivo

# 4. Manipular base de datos

library(tidyverse)

db_1 <- db %>% dplyr::select(country, year,UD, UD_s_female, UD_s_male)

##### Mapa 1 ######
#1. Manipulacion datos
#Seleccionar último dato de sindicalización por país
#Me di cuenta que tendré que imputar datos antes (paquete tidyr, funcion fill)

db_1$UD[db_1$UD == -88] <- NA
db_1$UD_s_female[db_1$UD_s_female == -88] <- NA
db_1$UD_s_male[db_1$UD_s_male == -88] <- NA

db_1 <- db_1[!is.na(db_1$UD), ]


db_2 <- db_1 %>% fill(UD) %>% filter(!is.na(UD_s_female))%>% 
  group_by(country) %>%
  filter(year == max(year)) %>% mutate(fudi = UD_s_female / UD_s_male)







#2. Cargar mapa
#Make a Spdb object (spatial polygon data frame).

my_map <- geojsonio::geojson_read('input/world-administrative-boundaries.geojson', what = 'sp')

#Rename Korea, Republic of
my_map$name <- gsub("Korea, Republic of", "South Korea", my_map$name)
my_map$name <- gsub("United States Minor Outlying Islands", "United States", my_map$name)
my_map$name <- gsub("United States Virgin Islands", "United States", my_map$name)

db_2$country <- gsub("Russian Federation", "Russia", db_2$country)

#Join map and data
map <- merge(my_map@data, db_2, by.x = "name", by.y = "country")



# 3. Create a color palette with handmade bins (RColorBrewer)

mybins <- c(0, 0.5, 0.7, 0.9, 1, 1.3, 1.4, Inf)
mypalette <- colorBin(palette = c("#0099CC", "white", "magenta4"), 
                      domain = map$fudi, 
                      na.color = "transparent", 
                      bins = mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "<b>", map$name,"</b>","<br/>", 
  "Union Density: ", round(map$UD,2),"%", "<br/>", 
  "Female union density: ", map$UD_female,"%", "<br/>",
  "Male union density: ", map$UD_male,"%", "<br/>",
  "FUDi: ", round(map$fudi, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

##Para el futuro
#Si quiero linkear algo:
# content <- paste(sep = "<br/>",
#                  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
#                  "606 5th Ave. S",
#                  "Seattle, WA 98138")


# Final Map
map2 <- leaflet(my_map) %>% 
  addTiles(providers$Stamen.TonerLite)  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="black", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~fudi, opacity=0.9, title = "Feminization", position = "bottomleft" )

m <- leaflet() %>% 
  addTiles(providers$Stamen.TonerLite)  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(data = getMapData()) %>%
  addLegend( pal=mypalette, values=~fudi, opacity=0.9, title = "Feminization", position = "bottomleft" )


map2

#Guardar
saveWidget(map2, file="figures/map1.html")  


############ figura 1 ###############
#Gif 1 feminizadas y gif 2 masc

#1. Manipular datos 
#usar db

library(tidyverse)
library(ggplot2)

db_2 <- db %>% filter(!is.na(UD_s_female)) %>%
  dplyr::select(year, country, UD, UD_s_female, UD_s_male)

db_2 <- db_2 %>% gather(key = "sexud", value = "rate", -UD, -country, -year)

db_2  <- mutate(db_2, sex = ifelse(sexud %in% c("UD_s_female"), "Female", "Male"))

db_2  <- mutate(db_2, femin = ifelse(country %in% c("Australia",
                                                  "Brazil",
                                                  "Canada",
                                                  "Chile",
                                                  "Croatia",
                                                  "Czech Republic",
                                                  "Denmark",
                                                  "Estonia",
                                                  "Finland",
                                                  "Hungary",
                                                  "Iceland",
                                                  "Ireland",
                                                  "Israel",
                                                  "Latvia",
                                                  "Lithuania",
                                                  "Malaysia",
                                                  "Mexico",
                                                  "New Zealand",
                                                  "Norway",
                                                  "Poland",
                                                  "Russian Federation",
                                                  "Slovenia",
                                                  "South Africa",
                                                  "Sweden",
                                                  "United Kingdom",
                                                  "United States",
                                                  "Uruguay"), 1, 0))

db_2$country <- gsub("Russian Federation", "Russia", db_2$country)

feminizado <- db_2 %>% filter(femin == 1)
masculinizado <- db_2 %>% filter(femin == 0)

# Figura 1.1
figura1.1  <- ggplot(feminizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Densidad sindical", breaks=c("Female", "Male"),
                      labels=c("Femenina", "Masculina"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Año", y = "Densidad sindical") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0), axis.text.x = element_text(size = 5))

figura1.1

ggsave(
  plot = figura1.1,
  filename = "output/graphs/figura1.1.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

figura1.1gif <- figura1.1 + gganimate::transition_reveal(year) 
figura1.1gif

#Guardar
anim_save(plot = figura1.1gif, filename = "output/graphs/gif1.1.gif", animation = last_animation(), path = NULL, width = 5000, height = 1000)


# figura 1.2
figura1.2  <- ggplot(masculinizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Densidad sindical", breaks=c("Female", "Male"),
                      labels=c("Femenina", "Masculina"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Año", y = "Densidad sindical") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0), axis.text.x = element_text(size = 5))

figura1.2

ggsave(
  plot = figura1.2,
  filename = "output/graphs/figura1.2.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

gif1.2 <- figura1.2 + gganimate::transition_reveal(year) 
gif1.2

#Guardar
anim_save(plot = gif1.2, filename = "../output/graphs/gif1.2.gif", animation = last_animation(), path = NULL, width = 5000, height = 1000)
