#---- 1. Librerías y base de datos ----

library(tidyverse)
library(openxlsx)
library(WDI)

# Abrir BBDD
db <- openxlsx::read.xlsx('input/data/UD_data.xlsx')

#----2. Variable iso3c ----

code <- read.xlsx("input/data/iso-code.xlsx")
code <- code %>% mutate(year = 1960)
code <- code %>%
  complete(country, year = 1960:2019) %>% fill(iso2c,iso3c,code, .direction = "down")

db <- merge(code, db, by = c("year","country"), all.y = T)

db <- db %>% select(-iso2c.y)

db <- db %>% select(-country_code)

#----3. Variable OCDE ----

ocde_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", 
                    "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", 
                    "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

db$OCDE <- ifelse(db$iso3c %in% ocde_countries, 1, 0)

#----4. Variable continent ----

db <- db %>%
  mutate(continent = ifelse(country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Mexico", "Uruguay"),"Latin America",
                            ifelse(country  %in% c("United States","Canada"),"North America",
                                   ifelse(country %in% c("Austria", "Belgium","Bulgaria","Croatia",
                                                         "Cyprus", "Czech Republic", "Denmark",
                                                         "Estonia", "Finland", "France", "Germany",
                                                         "Greece","Hungary", "Ireland", "Italy",
                                                         "Latvia", "Lithuania", "Luxembourg","Malta",
                                                         "Netherlands", "Norway", "Poland","Portugal","Romania", "Slovak Republic",
                                                         "Slovenia", "Spain", "Sweden", "Switzerland",
                                                         "United Kingdom"), "Europe",
                                          ifelse(country %in% c("Australia", "Iceland","New Zealand"), "Oceania",
                                                 ifelse(country %in% c("China", "Hong Kong, China",
                                                                       "India", "Israel", "Japan",
                                                                       "Korea, Republic of",
                                                                       "Malaysia", "Russian Federation",
                                                                       "Singapore", "Taiwan, China","Turkey", "Indonesia", "Philippines"), "Asia", "Africa"))))))

#----5. Variables Employment por sector ----

new_wdi_cache <- WDIcache() 
WDIsearch("industry.*employment.", cache = new_wdi_cache)

wdi_dat <- WDI(indicator = c("SL.SRV.EMPL.ZS", 
                             "SL.AGR.EMPL.ZS", 
                             "SL.IND.EMPL.ZS"), start = 1960, 
               end = 2024, extra = TRUE)

names(wdi_dat)

wdi_dat <- wdi_dat %>% select(country, year, iso3c, iso2c,
                              region, ser_empl="SL.SRV.EMPL.ZS",
                              ind_empl="SL.IND.EMPL.ZS",
                              agr_empl="SL.AGR.EMPL.ZS", income)

db <- db %>% left_join(wdi_dat, by = c('country', 'year'))

db <- db %>% select(-iso2c, -iso3c.y)

db <- db %>% rename(c('iso2c' = 'iso2c.x', 'iso3c' = 'iso3c.x'))

