# Procesamiento ----
# Valentina Andrade

#1. Packages
pacman::p_load(dplyr, ggplot2, ggsci, tidyr, readxl)


#lead() next and lag() past; first() el primero

#2. Cargar bases de datos
db <- readRDS(file = "input/data/database_FDL_AC.RData")

# 3. Explorar base
names(db)

## Limpiar duplicados
db <- db %>% distinct(country,year, .keep_all=T)

## Crear variables -------------

db$UD <- as.numeric(db$UD)

db$UD_fem <- as.numeric(db$UD_fem)

db$UD_male <- as.numeric(db$UD_male)

#1. Variables lag
db <- db %>%  group_by(country) %>%  
  mutate(growth_ud = UD - lag(UD),
         growth_udpercent = growth_ud/lag(UD) * 100,
         growth_udfem = UD_fem - lag(UD_fem),
         growth_udfempercent = growth_udfem/lag(UD_fem) * 100,
         growth_udmale = UD_male - lag(UD_male),
         growth_udmalepercent = growth_udmale/lag(UD_male) * 100)
        
         
         
         #growth_e = EPR_MW - lag(EPR_MW),
         #growth_epercent = growth_e/lag(EPR_MW) * 100,
         #growth_p = T_GDPHRS_V - lag(T_GDPHRS_V),
         #growth_ppercent = growth_p/lag(T_GDPHRS_V) * 100,
         #growth_rmw = rmw - lag(rmw),
         #growth_rmwpercent = growth_rmw/lag(rmw) * 100)

# 4. Variables VoC
# En base a Hall y Soskice (2001). Estados Bálticos (LMEs), Mediterraneos, Nórdicos, Centrales
db <- db %>%
  mutate(voc = ifelse(country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Mexico", "Uruguay"),"HLEs",
                            ifelse(country  %in% c("United States","Canada", "United Kingdom", "Australia", "Canada", "New Zealand", "Ireland", "Estonia",
                                                   "Hungary", "Romania", "Poland", "Russian Federation", "Israel", "Slovak Republic", "Korea, Republic of", "Latvia", "Lithuania"),"LMEs",
                                   ifelse(country %in% c("Austria", "Belgium", "Denmark", "Finland", "Iceland","Japan", "Germany",
                                                         "Netherlands", "Norway",  "Sweden", "Switzerland", "Slovenia", "Luxembourg", "Czech Republic"), "CMEs",
                                                 ifelse(country %in% c("France", "Italy", "Spain","Portugal","Greece","Turkey"), "Ambiguos", "No-Se")))))

# # NA in isoc3
# db <- db %>% mutate(iso3c = if_else(is.na(iso3c)&country=="Uruguay", "URY",
#                               if_else(is.na(iso3c)&country=="Slovak Republic", "SVK",
#                                       if_else(is.na(iso3c)&country=="Czech Republic", "CZE",iso3c))))
# 8. Guardar ---
save(db, file ="input/data/database_FDL_AC.RData")

openxlsx::write.xlsx(db, 'input/data/database_FDL_AC.xlsx')
rm(list = ls())
