# Chapter 1 --> Introducción
#----1. Cargar paquetes ---- 

library(tidyverse)
library(rworldmap)
library(sf)
library(ggplot2)

#---- 2. Cargar bases de datos ----
load(file = "input/data/database_FDL.RData")

#---- 3. Figura 1.1 ----
# Mapa

# 1. Manipular base de datos

db_1 <- db %>% dplyr::select(country, iso3c, year,UD, UD_fem, UD_male)

#Seleccionar último dato de sindicalización por país

db_1$UD_fem <- as.numeric(db_1$UD_fem)
db_1$UD_male <- as.numeric(db_1$UD_male)

db_2 <- db_1 %>% fill(UD) %>% filter(!is.na(UD_fem))%>% 
  group_by(country) %>%
  filter(year == max(year)) %>% mutate(fudi = UD_fem / UD_male)


#2. Cargar mapa

world_map <- joinCountryData2Map(db_2, joinCode = "ISO3", nameJoinColumn = "iso3c")

# 3. Create a color palette with handmade bins (RColorBrewer)

mybins <- c(0, 0.5, 0.7, 0.9, 1, 1.3, 1.4, Inf)
mypalette <- colorRampPalette(c("#0099CC", "white", "magenta4"))
world_map$fudi_bins <- cut(world_map$fudi, breaks = mybins, include.lowest = TRUE)
colors <- mypalette(length(levels(world_map$fudi_bins)))


world_sf <- st_as_sf(world_map)

map <- ggplot(data = world_sf) +
  geom_sf(aes(fill = fudi_bins), color = "black") +  # Añadir color a los bordes
  scale_fill_manual(values = colors, na.value = "transparent", 
                    name = "FUDI", 
                    labels = c("0 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "0.9 - 1", "1 - 1.3", "1.3 - 1.4", "1.4 - Inf")) +  # Añadir etiquetas a la leyenda
  theme_void() +
  theme(
    legend.position = "right",  # Posicionar la leyenda en el costado derecho
    plot.title = element_text(hjust = 0.5)  # Centrando el título
  ) +
  labs(title = "FUDI por País")

#Guardar
ggsave('output/figures/figure1.1.jpg')


#----4. Figura 1.2 ----

#1. Manipular datos 
#usar db

db_3 <- db %>% filter(!is.na(UD_fem)) %>%
  dplyr::select(year, country, UD, UD_fem, UD_male)

db_3 <- db_3 %>% gather(key = "sexud", value = "rate", -UD, -country, -year)

db_3  <- mutate(db_3, sex = ifelse(sexud %in% c("UD_fem"), "Female", "Male"))

db_3  <- mutate(db_3, femin = ifelse(country %in% c("Australia",
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

db_3$country <- gsub("Russian Federation", "Russia", db_3$country)

feminizado <- db_3 %>% filter(femin == 1)
masculinizado <- db_3 %>% filter(femin == 0)

feminizado$rate <- as.numeric(feminizado$rate)

# 2. Figura
figura1.2  <- ggplot(feminizado, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Densidad sindical", breaks=c("Female", "Male"),
                      labels=c("Femenina", "Masculina"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Año", y = "Densidad sindical") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position.inside = c(1,0), axis.text.x = element_text(size = 5))

figura1.2

ggsave(
  plot = figura1.2,
  filename = "output/figures/figura1.2.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)


#----5. Figura 1.3 ----


masculinizado$rate <- as.numeric(masculinizado$rate)

# figura 1.3
figura1.3  <- ggplot(masculinizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Densidad sindical", breaks=c("Female", "Male"),
                      labels=c("Femenina", "Masculina"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Año", y = "Densidad sindical") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0), axis.text.x = element_text(size = 5))

figura1.3

ggsave(
  plot = figura1.3,
  filename = "output/figures/figura1.3.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)
