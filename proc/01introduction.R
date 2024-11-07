# Chapter 1 --> Introducción
#----1. Cargar paquetes ---- 

library(tidyverse)
library(rworldmap)
library(sf)
library(openxlsx)

#---- 2. Cargar bases de datos ----
load(file = "input/data/database_FDL_AC.RData")


ictwss <- openxlsx::read.xlsx('input/data/database_FDL_AC.xlsx')

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

openxlsx::write.xlsx(ictwss, 'input/data/database_FDL_AC.xlsx')

saveRDS(ictwss, file = 'input/data/database_FDL_AC.RData')

 
# ictwss$country[which(ictwss$country == "United States of America")] <- "United States"
# 
# 
# code <- read.xlsx("input/data/iso-code.xlsx")
# code <- code %>% mutate(year = 1960)
# code <- code %>%
#   complete(country, year = 1960:2019) %>% fill(iso2c,iso3c,code, .direction = "down") #cambiar cuando sea 2019
# 
# db <- merge(code, ictwss, by = c("year","country"), all.y = T)
# 
# db <- db %>% select(-iso2c.y)
# 
# db <- db %>% select(-country_code)

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
  geom_sf(aes(fill = fudi_bins), color = "black") + 
  scale_fill_manual(values = colors, na.value = "transparent", 
                    name = "",
                    labels = c("0 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "0.9 - 1", "1 - 1.3", "1.3 - 1.4", "1.4-Inf"),
                    na.translate = F
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 1))
  


#Guardar
ggsave('output/figures/figure1.1.jpg')


#----4. Figura 1.2 ----

#1. Manipular datos 
#usar db

db$UD_fem <- as.numeric(db$UD_fem)
db$UD_male <- as.numeric(db$UD_male)



db_3 <- db %>% select(year, country, UD, UD_fem, UD_male, coverage)

db_3 %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  mutate(femin = ifelse(UD_fem > UD_male, 1, 0)) %>% 
  filter(femin == 1) %>% 
  print(n = 30)

db_3 <- db_3 %>% gather(key = "sexud", value = "rate", -UD, -country, -year, -coverage)

db_3  <- mutate(db_3, sex = ifelse(sexud %in% c("UD_fem"), "Female", "Male"))

db_3  <- mutate(db_3, femin = ifelse(country %in% c("Romania", "Israel", "Slovenia", "Finland", "Russian Federation", "Czech Republic", "Norway", "Poland", "Estonia", "Hungary", "Latvia", "Lithuania", "Malaysia", "Sweden", "Australia", "Croatia", "Denmark", "Iceland", "Ireland", "Mexico", "New Zealand", "Canada", "United Kingdom", "Chile"), 1, 0))

db_3$country <- gsub("Russian Federation", "Russia", db_3$country)

feminizado <- db_3 %>% filter(femin == 1)
masculinizado <- db_3 %>% filter(femin == 0)

feminizado$rate <- as.numeric(feminizado$rate)

# 2. Figura
figura1.2  <- ggplot(feminizado, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020),expand = c(0.05, 0.05))

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


feminizado_2obs <- feminizado %>%
  group_by(country) %>%
  filter(n() >= 4) %>%
  ungroup()


figura1.2.2  <- ggplot(feminizado_2obs, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020),expand = c(0.05, 0.05))

figura1.2.2

ggsave(
  plot = figura1.2.2,
  filename = "output/figures/figura1.2_2obs.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

feminizado_4obs <- feminizado %>%
  group_by(country) %>%
  filter(n() >= 8) %>%
  ungroup()

figura1.2.3  <- ggplot(feminizado_4obs, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020),expand = c(0.05, 0.05))

figura1.2.3

ggsave(
  plot = figura1.2.3,
  filename = "output/figures/figura1.2_4obs.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

feminizado_2000 <- feminizado %>% filter(coverage == 0)

feminizado_2000 <- feminizado_2000 %>% filter(country != 'Romania')

figura1.2.4  <- ggplot(feminizado_2000, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(2000, 2020, by = 5), limits = c(2000, 2020),expand = c(0.05, 0.05))

figura1.2.4

ggsave(
  plot = figura1.2.4,
  filename = "output/figures/figura1.2_2000.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

feminizado_1980 <- feminizado %>% filter(coverage == 1)

figura1.2.5  <- ggplot(feminizado_1980, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 5), limits = c(1980, 2020),expand = c(0.05, 0.05))

figura1.2.5

ggsave(
  plot = figura1.2.5,
  filename = "output/figures/figura1.2_1980.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)




#----5. Figura 1.3 ----


masculinizado$rate <- as.numeric(masculinizado$rate)

masculinizado <- masculinizado %>% filter(country != 'India')

# figura 1.3
figura1.3  <- ggplot(masculinizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020))

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

masculinizado_2obs <- masculinizado %>%
  group_by(country) %>%
  filter(n() >= 4) %>%
  ungroup()

masculinizado_2obs <- masculinizado_2obs %>% filter(country != 'Uruguay')

masculinizado_2obs <- masculinizado_2obs %>% filter(country != 'India')

masculinizado_2obs <- masculinizado_2obs %>% filter(country != 'Philippines')



figura1.3.2  <- ggplot(masculinizado_2obs, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020),expand = c(0.05, 0.05))

figura1.3.2

ggsave(
  plot = figura1.3.2,
  filename = "output/figures/figura1.3_2obs.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

masculinizado_4obs <- masculinizado %>%
  group_by(country) %>%
  filter(n() >= 8) %>%
  ungroup()


masculinizado_4obs <- masculinizado_4obs %>% filter(country != 'India')


figura1.3.3  <- ggplot(masculinizado_4obs, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 10), limits = c(1980, 2020),expand = c(0.05, 0.05))

figura1.3.3

ggsave(
  plot = figura1.3.3,
  filename = "output/figures/figura1.3_4obs.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)


masculinizado_2000 <- masculinizado %>%
  filter((coverage == 0 | country == "France") & country != "Philippines")


figura1.3.4  <- ggplot(masculinizado_2000, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(2000, 2020, by = 5), limits = c(2000, 2020),expand = c(0.05, 0.05))

figura1.3.4

ggsave(
  plot = figura1.3.4,
  filename = "output/figures/figura1.3_2000.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

masculinizado_1980 <- masculinizado %>% filter(coverage == 1 & country != "France")


figura1.3.5  <- ggplot(masculinizado_1980, aes(x= round(year, digits= 0), y = round(rate, digits = 0), group = sex, colour = sex)) + 
  geom_line(linewidth = 1) +
  scale_colour_manual(name="Union Density", breaks=c("Female", "Male"),
                      labels=c("Women", "Men"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Year", y = "Union Density") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(size = 5)) +
  geom_point(size = 1) + 
  scale_x_continuous(breaks = seq(1980, 2020, by = 5), limits = c(1980, 2020),expand = c(0.05, 0.05))

figura1.3.5

ggsave(
  plot = figura1.3.5,
  filename = "output/figures/figura1.3_1980.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)



