#---- Análisis descriptivo ----

library(tidyverse)
library(openxlsx)
library(expss)
library(summarytools)

load('input/data/database_FDL_AC.RData')

#----1. Tabla general ----

non_numeric_columns <- c("year", "country", "iso2c", "iso3c", 
                         "code", "coverage", "OCDE", 
                         "continent", "region", 
                         "income", "voc")

for(col in setdiff(names(db), non_numeric_columns)) {
  db[[col]] <- as.numeric(db[[col]])
}

data_descr <- db %>% select(!c(non_numeric_columns)) %>% ungroup()

data_descr <- data_descr %>% select(!country)

#?descr

tabla_descriptiva <- descr(data_descr, stats = c("mean", "med", "min", "max", "sd", "n.valid",
                                                 'pct.valid'),
                           style = 'simple', transpose = TRUE)

view(tabla_descriptiva, file = 'output/tables/descriptive_table_general.html')

#----2. Tablas por continente ----

data_descr_continent <- db %>% ungroup() %>% 
  select(!c("year", "country", "iso2c", "iso3c", 
                                         "code", "coverage", "OCDE", "region", 
                                         "income", "voc"))

(tabla_descriptiva_continent <- stby(data      = data_descr_continent, 
                               INDICES   = data_descr_continent$continent, 
                               FUN       = descr, 
                               stats     = c("mean", "med", "max", "min", "sd", "n.valid", "pct.valid"), 
                               transpose = TRUE))

view(tabla_descriptiva_continent, file = 'output/tables/descriptive_table_continent.html')

#----3. Tablas por coverage ----

data_descr_coverage <- db %>% ungroup() %>% 
  select(!c("year", "country", "iso2c", "iso3c", 
            "code", "continent", "OCDE", "region", 
            "income", "voc"))

data_descr_coverage$coverage <- as.character(data_descr_coverage$coverage)

(tabla_descriptiva_coverage <- stby(data      = data_descr_coverage, 
                                     INDICES   = data_descr_coverage$coverage, 
                                     FUN       = descr, 
                                     stats     = c("mean", "med", "max", "min", "sd", "n.valid", "pct.valid"), 
                                     transpose = TRUE))

view(tabla_descriptiva_coverage, file = 'output/tables/descriptive_table_coverage.html')
