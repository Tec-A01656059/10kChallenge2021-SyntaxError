### Syntax error
### 10K Challenge - Explorer
### Etapa 1: Exploraci√≥n
### 12 de noviembre de 2021

# Librerias -------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

install.packages("grid")
library(grid)

install.packages("gridExtra")
library(gridExtra)

install.packages("ggcorrplot")
library(ggcorrplot)

install.packages("hrbrthemes")
library(hrbrthemes)

install.packages("patchwork")
library(patchwork)

# Lectura bases de datos y %notin% --------------------------------------------
expectancy_full <- read.csv("https://raw.githubusercontent.com/Tec-A01656059/10kChallenge2021-SyntaxError/main/Life%20Expectancy%20Data.csv")
attach(expectancy_full)

gdp_wb <- read.csv("https://raw.githubusercontent.com/Tec-A01656059/10kChallenge2021-SyntaxError/main/GDP_WorldBank.csv")
attach(gdp_wb)

'%notin%' <- Negate('%in%')

# Filtrados y segmentacion ----------------------------------------------------
expectancy_15 <- expectancy_full %>% 
  filter(is.na(Life.expectancy)==F) %>% 
  filter(Year == 2015) %>% 
  arrange(Country)

gdp_wb_match <- gdp_wb %>% 
  filter(is.na(X2015)==F) %>% 
  filter(Ô..Country %in% expectancy_15$Country) %>% 
  mutate(clasificacion.gdp15 = case_when(
    X2015 < 1005 ~ "low",
    X2015 >= 1005 & X2015 < 3955 ~ "lower middle",
    X2015 >= 3955 & X2015 < 12235 ~ "upper middle",
    X2015 >= 12235 ~ "high",
  )) %>% 
  rename(Country = Ô..Country) %>% 
  arrange(Country)

expectancy_15 <- expectancy_15 %>% 
  filter(Country %in% gdp_wb_match$Country)

clasificacion.gdp15 <- rep(gdp_wb_match$clasificacion.gdp15,each=16)

GDP_wb <- c()
for(i in 1:nrow(gdp_wb_match))
{ 
  GDP_wb <- c(GDP_wb,rev(as.numeric(as.vector(gdp_wb_match[i, ]))[2:17]))
}

expectancy_full_gdp15 <- expectancy_full %>%
  arrange(Country) %>% 
  filter(Country %in% expectancy_15$Country) %>% 
  cbind(GDP_wb, clasificacion.gdp15) %>% 
  mutate(clasificacion.gdp15=factor(clasificacion.gdp15),
         Country = factor(Country))

# Valores maximos minimos y promedio de esperanza de vida por GDP -------------
## Maxima esperanza de vida por GDP--------------------------------------------
expectancy_full_gdp15 %>% 
  filter(is.na(clasificacion.gdp15)==F) %>% 
  group_by(clasificacion.gdp15) %>% 
  filter(Life.expectancy == max(Life.expectancy)) %>% 
  filter(Adult.Mortality == min(Adult.Mortality)) %>% 
  select(Country, Life.expectancy, Adult.Mortality, clasificacion.gdp15)

## Minima esperanza de vida por GDP -------------------------------------------
expectancy_full_gdp15 %>% 
  filter(is.na(clasificacion.gdp15)==F) %>% 
  group_by(clasificacion.gdp15) %>% 
  filter(Life.expectancy == min(Life.expectancy, na.rm=T)) %>% 
  filter(Adult.Mortality == max(Adult.Mortality, na.rm=T)) %>% 
  select(Country, Life.expectancy, Adult.Mortality, clasificacion.gdp15)

## Promedios por GDP (Esperanza de vida + GDP) ---------------------------------
liex_prom <- function(rank){
  av.liex <- round((expectancy_full_gdp15 %>%
                      filter(Year == 2015) %>% 
                      filter(clasificacion.gdp15 == rank) %>%
                      summarise(mean(Life.expectancy, na.rm = T))),1)
  expectancy_full_gdp15 %>% 
    filter(clasificacion.gdp15 == rank) %>% 
    filter(abs(Life.expectancy-av.liex[1,1]) == min(abs(Life.expectancy-av.liex[1,1]))) %>% 
    select(Country,Life.expectancy,GDP_wb)
}

gdp_liex_prom <-  function(rank)
{
  av.gdp <- round((expectancy_full_gdp15 %>% 
                     filter(Year == 2015) %>% 
                     filter(clasificacion.gdp15 == rank) %>% 
                     summarise(as.numeric(mean(GDP_wb, na.rm = T)))),4)
  liex_prom(rank) %>% 
    filter(abs(GDP_wb-av.gdp[1,1])==min(abs(GDP_wb-av.gdp[1,1])))
}

gdp_liex_prom("high")
gdp_liex_prom("upper middle")
gdp_liex_prom("lower middle")
gdp_liex_prom("low")

# Base reducida ---------------------------------------------------------------
countries_final <- c("Costa Rica","Egypt","Sweden","Tajikistan","Angola",
                     "Haiti","Sierra Leone","Trinidad and Tobago","Austria",
                     "Mauritius","Micronesia","Rwanda")

expectancy_short <- expectancy_full_gdp15 %>%
  filter(Country %in% countries_final) %>%
  mutate(category = case_when(
    Country %in% c("Costa Rica","Egypt","Sweden","Tajikistan") ~ "max",
    Country %in% c("Angola","Haiti","Sierra Leone","Trinidad and Tobago") ~ "min",
    Country %in% c("Austria","Mauritius","Micronesia","Rwanda") ~ "avr"
  ))
attach(expectancy_short)

## Exportaci√≥n de base reducida (archivo en GitHub) ---------------------------
write.csv(expectancy_short,"C:\\Users\\soalv\\Documents\\LifeExpectancy_Short1.csv",row.names=F)
expectancy_short <- read.csv("https://raw.githubusercontent.com/Tec-A01656059/10kChallenge2021-SyntaxError/main/LifeExpectancy_Short1.csv")

# Correlaciones ---------------------------------------------------------------
## Completa--------------------------------------------------------------------
data.cor <- expectancy_short %>% 
  select(c(4:16,18:23))

cor1 <- round(cor(data.cor, method = "pearson", use = "complete.obs"),2)

ggcorrplot(cor1,
           sig.level=0.05,
           legend.title = "Coeficiente",
           colors=c("dodgerblue","white","orangered2"),
           title = "Correlaci√≥n entre factores",
           tl.cex = 7,
           lab=T,
           lab_size = 1.8)

## Socioeconomica -------------------------------------------------------------
socioeconomic.var <- c("Life.expectancy","percentage.expenditure",
                       "Total.expenditure","Population",
                       "Income.composition.of.resources","Schooling","GDP_wb")

data.cor.socio <- data.cor %>%
  select(socioeconomic.var)

cor.socio <- round(cor(data.cor.socio, method = "pearson", use = "complete.obs"),2)

ggcorrplot(cor.socio,
           legend.title = "Coeficiente",
           colors=c("dodgerblue","white","orangered2"),
           title = "Correlaci√≥n entre factores socioecon√≥micos y esperanza de vida",
           tl.cex = 8,
           lab=T,
           lab_size = 3)

## Salud ----------------------------------------------------------------------
health.var <- c("Life.expectancy","Adult.Mortality","infant.deaths","Alcohol",
                "Hepatitis.B","Measles","BMI","under.five.deaths","Polio",
                "Diphtheria","HIV.AIDS","thinness..1.19.years",
                "thinness.5.9.years")

data.cor.health <- data.cor %>% 
  select(health.var)

cor.health<- round(cor(data.cor.health, method = "pearson", use = "complete.obs"),2)

ggcorrplot(cor.health,
           legend.title = "Coeficiente",
           colors=c("dodgerblue","white","orangered2"),
           title = "Correlaci√≥n entre factores de salud y esperanza de vida",
           tl.cex = 8,
           lab=T,
           lab_size = 2.5)

## Infantes -------------------------------------------------------------------
infant.var <- c("Life.expectancy","infant.deaths","Hepatitis.B",
                "under.five.deaths","Polio","Diphtheria","HIV.AIDS",
                "thinness..1.19.years","thinness.5.9.years",
                "Schooling","GDP_wb")

data.cor.infant <- data.cor %>% 
  select(infant.var)

cor.infant <- round(cor(data.cor.infant, method = "pearson", use = "complete.obs"),2)

ggcorrplot(cor.infant,
           legend.title = "Coeficiente",
           colors=c("dodgerblue","white","orangered2"),
           title = "Correlaci√≥n entre factores en ni√±os e infantes
           y la esperanza de vida",
           tl.cex = 8,
           lab=T,
           lab_size = 3)

# Evolucion paises representativos --------------------------------------------
expectancy_short_graph <- expectancy_short %>%
  mutate(clasificacion.gdp15 = factor(clasificacion.gdp15,
                                      levels = c("high","low","upper middle",
                                                 "lower middle"),
                                      labels = c("Alto (mayor a 12,235 USD)",
                                                 "Bajo (menor a 1,005 USD)",
                                                 "Medio alto (entre 3,956 USD y 12,235 USD)",
                                                 "Medio bajo (entre 1,006 USD y 3,955 USD)"))) %>% 
  mutate(Country.cat = paste(Country," (", category,")",sep=""))

expectancy_short_graph_s <- split(expectancy_short_graph, 
                                  f=expectancy_short_graph$clasificacion.gdp15)
p1 <- ggplot(expectancy_short_graph_s$`Bajo (menor a 1,005 USD)`,
             aes(Year,Life.expectancy, color = Country.cat)) +
  geom_line() + geom_point(size = 1.5)+
  scale_colour_discrete(name ="PaÌs y valor en su \n categorÌa: m√°ximo,\n mÌnimo y prom (avr)") +
  theme(legend.title = element_text(size=9)) +
  labs(x="A√±o", y= "Esperanza de vida (a√±os)") +
  facet_wrap(~clasificacion.gdp15,ncol=1)
p2 <- p1 %+% expectancy_short_graph_s$`Medio bajo (entre 1,006 USD y 3,955 USD`
p3 <- p1 %+% expectancy_short_graph_s$`Medio alto (entre 3,956 USD y 12,235 USD`
p4 <- p1 %+% expectancy_short_graph_s$`Alto (mayor a 12,235 USD`
grid.arrange(p1,p2,p3,p4,top=textGrob("Evoluci√≥n de la esperanza de vida en los paÌses representativos de su rango de PIB"))

# Gr·fica de relaciÛn aÒos de escolaridad y litros de alcohol consumidos--------

r1 <- expectancy_short %>% 
  filter(Country == "Sweden" | Country == "Costa Rica" |
           Country == "Egypt" | Country == "Tajikistan")
r2 <- expectancy_short %>% 
  filter(Country == "Trinidad and Tobago" | Country == "Angola" |
           Country == "Haiti" | Country == "Sierra Leone")
r3 <- expectancy_short %>% 
  filter(Country == "Austria" | Country == "Mauritius" |
           Country == "Micronesia" | Country == "Rwanda")

# Gr·fica primer rango
rango1 <- ggplot(r1, aes(Alcohol, Schooling))+
  geom_count(aes(color = Country), size = 3.5)+
  labs(title ="Ingesta de alcohol de acuerdo con aÒos de escolaridad",
       subtitle = "PaÌses del primer rango",
       x     =    "Alcohol en litros",
       y     =    "AÒos de escolaridad")+
  scale_color_ipsum() +
  theme_ipsum_rc()+
  scale_y_continuous(name="Escolaridad", labels = scales::comma) + 
  theme(legend.position = "bottom")

# Gr·fica segundo rango
rango2 <- ggplot(r2, aes(Alcohol, Schooling))+
  geom_count(aes(color = Country), size = 3.5)+
  labs(subtitle = "PaÌses del segundo rango",
       x     =    "Alcohol en litros",
       y     =    "AÒos de escolaridad")+
  scale_color_ipsum() +
  theme_ipsum_rc()+
  scale_y_continuous(name="Escolaridad", labels = scales::comma) + 
  theme(legend.position = "bottom")

# Gr·fica tercer rango
rango3 <- ggplot(r3, aes(Alcohol, Schooling))+
  geom_count(aes(color = Country), size = 3.5)+
  labs(subtitle = "PaÌses del tercer rango",
       x     =    "Alcohol en litros",
       y     =    "AÒos de escolaridad",
       caption =  "Equipo: Syntax error")+
  scale_color_ipsum() +
  theme_ipsum_rc()+
  scale_y_continuous(name="Escolaridad", labels = scales::comma) + 
  theme(legend.position = "bottom")

# Uniendo gr·ficas
design0 <- "ABC"
wrap_plots(list(A = rango1, B = rango2, C = rango3), design = design0) 

# Grafica de frecuencia de indice de masa corporal en la poblacion--------------

# Gr·fica 1
BMI1 <- ggplot(r1, aes(BMI)) +   
  geom_freqpoly(aes(color = Country), size = 2.5)+
  labs(title ="Frecuencia de Ìndice de masa corporal",
       subtitle = "PaÌses del primer rango",
       x     =    "Õndice de masa corporal",
       y     =    "Conteo")+
  scale_color_ipsum() +
  theme_ipsum_rc() + 
  theme(legend.position = "bottom")  

# Gr·fica 2
BMI2 <- ggplot(r2, aes(BMI)) +   
  geom_freqpoly(aes(color = Country), size = 2.5)+
  labs(subtitle = "PaÌses del segundo rango",
       x     =    "Õndice de masa corporal",
       y     =    "Conteo")+
  scale_color_ipsum() +
  theme_ipsum_rc() + 
  theme(legend.position = "bottom") 

# Gr·fica 3
BMI3 <- ggplot(r3, aes(BMI)) +   
  geom_freqpoly(aes(color = Country), size = 2.5)+
  labs(subtitle = "PaÌses del tercer rango",
       x     =    "Õndice de masa corporal",
       y     =    "Conteo",
       caption =  "Equipo: Syntax error")+
  scale_color_ipsum() +
  theme_ipsum_rc() + 
  theme(legend.position = "bottom") 

# Uniendo gr·ficas
design0 <- "ABC"
wrap_plots(list(A = BMI1, B = BMI2, C = BMI3), design = design0) 
