# Chapter 1 --> Introducción
#----1. Cargar paquetes ---- 

library(tidyverse)
library(rworldmap)
library(sf)
library(openxlsx)

#---- 2. Cargar bases de datos ----
#load(file = "input/data/database_FDL.RData")


ictwss <- openxlsx::read.xlsx('input/data/ictwss_og.xlsx')

ictwss  <- ictwss  %>%
  group_by(country) %>%
  mutate(valid_years = ifelse(!is.na(UD_fem) & UD_fem != "", year, NA)) 

ictwss <- ictwss %>% filter(!is.na(UD_fem))

ictwss <- ictwss %>% filter(UD_fem != " ")

ictwss_2 <- ictwss %>%  
  summarise(min_year = min(valid_years, na.rm = TRUE), keep_all = TRUE) 

ictwss_2  <- ictwss_2  %>%
  mutate(coverage = ifelse(substr(as.character(min_year), 1, 2) == "19", 1,
                           ifelse(substr(as.character(min_year), 1, 2) == "20", 0, NA))) %>%
  select(country, coverage)

ictwss <- full_join(ictwss, ictwss_2, by = 'country')

ictwss <- ictwss %>% select(-coverage.x, -valid_years)

ictwss <- ictwss %>% rename(coverage = coverage.y)

openxlsx::write.xlsx(ictwss, 'input/data/UD_data.xlsx')



ictwss$country[which(ictwss$country == "United States of America")] <- "United States"
ictwss <- select(ictwss, country, year, UD, UD_fem, UD_male)


code <- read.xlsx("input/data/iso-code.xlsx")
code <- code %>% mutate(year = 1960)
code <- code %>%
  complete(country, year = 1960:2019) %>% fill(iso2c,iso3c,code, .direction = "down") #cambiar cuando sea 2019

db <- merge(code, ictwss, by = c("year","country"), all.y = T)



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

db_2 <- db_2 %>% filter(!is.na(fudi))

#2. Cargar mapa

world_map <- joinCountryData2Map(db_2, joinCode = "ISO3", nameJoinColumn = "iso3c")

world_map_filtered <- subset(world_map, NAME != "Antarctica")


# 3. Create a color palette with handmade bins (RColorBrewer)

mybins <- c(0, 0.5, 0.7, 0.9, 1, 1.3, 1.4, Inf)
mypalette <- colorRampPalette(c("#0099CC", "#c3c3c3", "magenta4"))
world_map_filtered$fudi_bins <- cut(world_map_filtered$fudi, 
                                    breaks = mybins, 
                                    include.lowest = TRUE)
colors <- mypalette(length(levels(world_map_filtered$fudi_bins)))



# Convertir a objeto sf
world_sf <- st_as_sf(world_map_filtered)


map <- ggplot(data = world_sf) +
  geom_sf(aes(fill = fudi_bins), color = "black") +  # Añadir color a los bordes
  scale_fill_manual(values = colors, na.value = "transparent", 
                    name = "",
                    labels = c("0 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "0.9 - 1", "1 - 1.3", "1.3 - 1.4", "1.4 - Inf")) +  # Añadir etiquetas a la leyenda
  theme_void() +
  theme(
    legend.position = "right",  # Posicionar la leyenda en el costado derecho
    plot.title = element_text(hjust = 0.5)  # Centrando el título
  )

#Guardar
ggsave('output/figures/figure1.1.jpg')


#----4. Figura 1.2 ----

#1. Manipular datos 
#usar db

db_3 <- db %>%
  mutate(UD_fem = na_if(UD_fem, ""))

db_3 <- db %>%
  mutate(UD_fem = na_if(UD_fem, " "))


db_3 <- db_3 %>% filter(!is.na(UD_fem)) %>%
  dplyr::select(year, country, UD, UD_fem, UD_male)

db_3$UD_fem <- as.numeric(db_3$UD_fem)
db_3$UD_male <- as.numeric(db_3$UD_male)


db_3 %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  mutate(femin = ifelse(UD_fem > UD_male, 1, 0)) %>% 
  filter(femin == 1)

db_3 <- db_3 %>% gather(key = "sexud", value = "rate", -UD, -country, -year)

db_3  <- mutate(db_3, sex = ifelse(sexud %in% c("UD_fem"), "Female", "Male"))

db_3  <- mutate(db_3, femin = ifelse(country %in% c("Romania", "Israel", "Slovenia", "Finland", "Russian Federation", "Poland", "Estonia", "Hungary", "Latvia", "Lithuania", "Malaysia", "Croatia", "Denmark", "Ireland", "Mexico", "Canada", "Norway", "Australia", "United Kingdom"), 1, 0))

db_3$country <- gsub("Russian Federation", "Russia", db_3$country)

feminizado <- db_3 %>% filter(femin == 1)
masculinizado <- db_3 %>% filter(femin == 0)

feminizado$rate <- as.numeric(feminizado$rate)

# 2. Figura
figura1.2  <- ggplot(feminizado, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Femenine", "Masculine"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position.inside = c(1,0), axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2018, by = 5), limits = c(1980, 2018))

figura1.2

ggsave(
  plot = figura1.2,
  filename = "output/figures/figura1.2_ejeylibre.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

figura1.2_b  <- ggplot(feminizado, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Femenine", "Masculine"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position.inside = c(1,0), axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2018, by = 5), limits = c(1980, 2018)) +
  scale_y_continuous(breaks = seq(0, 98, by = 10), limits = c(0, 98))

figura1.2_b

ggsave(
  plot = figura1.2_b,
  filename = "output/figures/figura1.2_ejeyfijo.png",
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
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Femenine", "Masculine"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2018, by = 5), limits = c(1980, 2018))

figura1.3

ggsave(
  plot = figura1.3,
  filename = "output/figures/figura1.3_ejeylibre.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

figura1.3_b  <- ggplot(masculinizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Femenine", "Masculine"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2018, by = 5), limits = c(1980, 2018)) +
  scale_y_continuous(breaks = seq(0, 98, by = 10), limits = c(0, 98))

figura1.3_b

ggsave(
  plot = figura1.3_b,
  filename = "output/figures/figura1.3_ejeyfijo.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

saveRDS(db, file = 'ud_database.rds')

