# Chapter 2 ----

#1. Packages
pacman::p_load(dplyr, ggplot2,
               ggsci, tidyr, WDI, plotly,
               RColorBrewer, htmlwidgets,
               kableExtra)

#0. Theme
theme_set(theme_classic() + theme(axis.title =  element_text(size = 14),
                                  axis.text = element_text(size =10),
                                  legend.text = element_text(size =14)))

#lead() next and lag() past; first() el primero
options(kableExtra.html.bsTable = T)
options(knitr.kable.NA = '')

#2. Database
load(file = "input/data/database_FDL.RData")

# 3. Variables
names(db)

# 4. Figures

# Figure 2.1  ------------
# Densidad sindical en países OCDE ultimo año

db$UD <- as.numeric(db$UD)

figure2.1 <- db %>% filter(!is.na(UD), OCDE ==1) %>% group_by(country) %>% filter(year == max(year)) %>% 
  mutate(n_ud = ifelse(UD <=30, "Bajo","Alto")) %>% 
  ggplot(aes(x= reorder(country, UD), y = UD, fill =UD,
             text = paste("Country:", country, "</br>Year:", year))) + 
  geom_bar(stat="identity", color = "black") + scale_fill_viridis_c(name = "") + geom_hline(aes(yintercept = 35.6), linetype = "dashed", color = "gray40") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  labs(x= "", y = "Union Density") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 40, vjust = 0.63))

ggsave('output/figures/figure2.1.png', plot = figure2.1, device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)

# Figure 2.2-------------
#Densidad sindical por contiente 1960 a 2020



figure2.2  <- db %>% select(UD, year, country, continent) %>% filter(!is.na(UD), continent  !="Africa") %>% 
  ggplot(aes( x= year, y=  UD, color = country))  + geom_line() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  facet_wrap(.~continent) +
  theme(legend.position='none') +  
  labs(y ="Union Density", x ="", caption= "Source: Author's own elaboration based on ICTWSS (2019)(*))")


ggsave('output/figures/figure2.2.png', plot = figure2.2, device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


# Figure 2.3 -------------

# Figure 2.4 -----------------

# Figure 2.5 ----------------------


# Figure 2.6 -------
# Union density and sectors
new_wdi_cache <- WDIcache() 
WDIsearch("industry.*employment.", cache = new_wdi_cache)

wdi_dat <- WDI(indicator = c("SL.SRV.EMPL.ZS", 
                             "SL.AGR.EMPL.ZS", 
                             "SL.IND.EMPL.ZS"), start = 1960, 
               end = 2024, extra = TRUE)

names(wdi_dat)

# WDI() returns a data frame in wide format. Rows are per year and country/region, columns are the different indicators.
# wb() returns a data frame in long format. Each row is one observation

wdi_dat <- wdi_dat %>% select(country, year, iso3c, iso2c,
                              region, ser_empl="SL.SRV.EMPL.ZS",
                              ind_empl="SL.IND.EMPL.ZS",
                              agr_empl="SL.AGR.EMPL.ZS", income)

fig2.6 <- wdi_dat %>% filter(country %in% c("World", "Latin America & Caribbean", "Europe & Central Asia", "East Asia & Pacific", "Middle East & North Africa","North America")) %>% 
  gather(key = "sector", value = "percent", -country, -year, -iso3c, -iso2c, -region, -income) %>%  filter(!is.na(percent))

fig2.6$country <- as.factor(fig2.6$country)
levels(fig2.6$country) <- c("East Asia and Pacific", "Europe and Central Asia", "Latin America", "Africa", "North America", "World")

figure2.6<- ggplot(fig2.6, aes(x = year, y = percent)) + 
  geom_line(aes(color = sector), size = 1) + 
  scale_color_brewer(palette = "Dark2",name = "", labels =c("Agriculture", "Industry", "Services")) +
  theme(legend.position ="bottom") + facet_wrap(.~country, scales = "free")+ 
  labs(y ="%", x ="", caption= "Source: Author's own elaboration based on World Bank (2024) using estimates from ILO (2019)")

ggsave(plot = figure2.6,
       filename = "output/figures/figure2.6.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


# Union density by sex and
# Wage and salaried workers (employees) are those workers who hold the type of jobs defined as "paid employment jobs," where the incumbents hold explicit (written or oral) or implicit employment contracts that give them a basic remuneration that is not directly dependent upon the revenue of the unit for which they work.


fig2.7 <- db %>% select(year, LF_RATE__T, LF_RATE_F,LF_RATE_M, country) %>%   
  group_by(year)  %>% 
  filter(!is.na(LF_RATE_F)) %>% 
  filter(!is.na(country)) %>% 
  gather(key = "lf", value = "percent", -year, -country)


fig2.7 <- fig2.7 %>%
  group_by(year, lf) %>%
  summarize(promedio_percent = mean(percent, na.rm = TRUE))


figure2.7 <- fig2.7 %>% filter(!is.na(promedio_percent)) %>% 
  ggplot(aes(x = year, y = promedio_percent)) + 
  geom_line(aes(color = lf), size = 1) + 
  geom_hline(aes(yintercept = 50), linetype = "dashed", color = "gray40")+
  scale_color_brewer(palette = "Dark2", name = "", labels = c("Total", "Women", "Men")) +
  theme(legend.position ="bottom") + 
  labs(y ="%", x ="", caption= "Source: Author's own elaboration based on OECD (2024). Wage and salaried workers by sex")

ggsave(plot = figure2.7,
       filename = "output/figures/figure2.7.png", device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)

# Figura 2.8 ----------------

fig2.8 <- db %>% select(year, country, `Part-time__T`, `Part-time_F`, `Part-time_M`) %>% 
  gather(key = "pt", value = "percent", -year, -country)

fig2.8 <- fig2.8 %>%
  group_by(year, pt) %>%
  summarize(promedio_percent = mean(percent, na.rm = TRUE))


figure2.8 <- fig2.8 %>%
  filter(!is.na(promedio_percent)) %>% 
  ggplot(aes(x = year, y = promedio_percent)) + 
  geom_line(aes(color = pt), size = 1) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 50))+
  scale_color_brewer(palette = "Dark2",name = "", labels = c( "Total","Women", "Men")) +
  theme(legend.position ="bottom") + 
  labs(y ="Part-time Employment", x ="", caption= "Source: Author's own elaboration based on OECD (2024). Wage and salaried workers by sex")

ggsave(plot = figure2.8,
       filename = "output/figures/figure2.8.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


## Table2.1 -----------
table <- db %>% group_by(country, year) %>% 
  do(add_row(., country = unique(.$country),
             year = unique(.$year),
             growth_ud = mean(.$growth_ud, na.rm = T))) %>%
  mutate(growth = ifelse(growth_ud < 0, "Decenso","Aumento")) %>% 
  select(country, growth_ud) %>% 
  filter(!is.na(growth_ud)) %>%  
  unique()

# Paises que ha disminuido y aumentado sindicalizacion (ultimos 20 anos)
table %>% filter(year > 2000)%>% group_by(country) %>% summarise(mean = mean(growth_ud)) %>%
  mutate(growth = ifelse(mean < 0, "Decenso","Aumento")) %>% unique() %>% group_by(growth) %>% summarise(n())


table %>% group_by(country)  %>%
  filter(n()>56)%>% summarise(mean = mean(growth_ud)) %>%
  mutate(growth = ifelse(mean < 0, "Decenso","Aumento")) %>% unique() %>% group_by(growth) %>% summarise(n())

# Manipular y crear tabla
table1 <- table %>% 
  mutate(periodo=case_when(year %in% c(1961:1979)~1,
                           year %in% c(1980:1999)~2,
                           year %in% c(2000:2020)~3)) %>% 
  group_by(periodo,country) %>% summarise(growth_ud=mean(growth_ud,na.rm = TRUE)) %>% 
  spread(periodo,growth_ud)


table_1 <- knitr::kable(table1, digits = 3,  booktabs = T,
                      col.names = c("Country", "60-70", "80-90", "2000-")) %>%
  add_header_above(c(" ", "Decades" = 3)) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")%>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)%>%
  kableExtra::column_spec(1, width = "9cm") %>%
  kableExtra::footnote(general = "Source: Own elaboration based on ICTWSS (2019)",general_title = "")

save_kable(table1, file ="output/tables/tabla2.1.png")


## Figure 2.9 -----------------------

figure2.9 <- db  %>% fill(rmw, .direction = "downup") %>% filter(!is.na(UD)) %>% 
  group_by(country)  %>% filter(year == max(year)) %>% arrange(desc(UD))%>% 
  ggplot(aes(rmw, UD, size = UD, fill= voc, 
             text = paste("País:", country, "</br>Año:", year))) +
  geom_point(alpha=0.5, shape=21, color="black")  +
  geom_text(aes(label = country), vjust = -0.5, size = 3) +
  scale_fill_viridis_d(guide=FALSE, option="A") + 
  scale_size(range = c(.1, 10))  + labs(x = "US $ Purchasing Power Parities (PPPs)") + scale_x_continuous(labels = function(x) paste0(x, "USD")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,55)) 

ggsave(plot = figure2.9,
       filename = "output/figures/figure2.9.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


##  Nota: RMW are in constant prices at last year USD PPPs

