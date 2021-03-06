### An�lisis estad�stico descriptivo """

## Histograma
## Media
## Varianza
## Desviaci�n est�ndar
## Simetr�a
## Curtosis 

library(tidyverse)
library(moments)

## Importar base de datos

LaBuena <- read.csv("https://raw.githubusercontent.com/Tec-A01656059/10kChallenge2021-SyntaxError/main/LifeExpectancy_Short1.csv")

LaBuena <- LaBuena %>% 
  mutate(Stats = case_when(
    LaBuena$category == "max" ~ "M�ximo",
    LaBuena$category == "min" ~ "M�nimo",
    LaBuena$category == "avr" ~ "Promedio")) %>% 
  mutate(Stats = factor(Stats)) %>% 
  filter(is.na(Stats) == F) %>% 
  mutate(GDP_Clasification = case_when(
    LaBuena$clasificacion.gdp15 == "high" ~ "Alto",
    LaBuena$clasificacion.gdp15 == "upper middle" ~ "Medio alto",
    LaBuena$clasificacion.gdp15 == "lower middle" ~ "Medio bajo",
    LaBuena$clasificacion.gdp15 == "low" ~ "Bajo")) %>% 
  mutate(GDP_Clasification = factor(GDP_Clasification)) %>% 
  filter(is.na(GDP_Clasification) == F) 

LaBuena$GDP_Clasification <- factor(LaBuena$GDP_Clasification, levels = c("Alto", "Medio alto", "Medio bajo", "Bajo"))
LaBuena <- LaBuena[, c(1, 2, 23, 24, 25, 26, 27, 3, 4, 5:22)]

LaBuena2015 <- LaBuena %>% filter (Year == 2015)

## Funci�n de boceto de gr�ficas 

## Distribuci�n de variable 1: Esperanza de vida

ggplot(LaBuena2015, aes(Life.expectancy)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 10) +
  geom_vline(xintercept = mean(LaBuena2015$Life.expectancy), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Life.expectancy) + sd(LaBuena2015$Life.expectancy), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Life.expectancy) - sd(LaBuena2015$Life.expectancy), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Esperanza de vida", 
       caption = "Equipo: Syntax error",
       x = "Esperanza de vida (a�os)",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 45, y = 4.3, 
             label = paste(" Media:", round(mean(LaBuena2015$Life.expectancy), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Life.expectancy), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Life.expectancy), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Life.expectancy), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Life.expectancy), 3), ""), 
             hjust = 0, vjust = 0.5)



## Distribuci�n de variable 2: Mortalidad en adultos

ggplot(LaBuena2015, aes(Adult.Mortality)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 30) +
  geom_vline(xintercept = mean(LaBuena2015$Adult.Mortality), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Adult.Mortality) + sd(LaBuena2015$Adult.Mortality), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Adult.Mortality) - sd(LaBuena2015$Adult.Mortality), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Mortalidad en adultos", 
       caption = "Equipo: Syntax error",
       x = "Muertes por cada mil habitantes (personas entre 15 y 60)",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 440, y = 2.5, 
             label = paste(" Media:", round(mean(LaBuena2015$Adult.Mortality), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Adult.Mortality), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Adult.Mortality), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Adult.Mortality), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Adult.Mortality), 3), ""), 
             hjust = 1, vjust = 0.5)


## Distribuci�n de variable 3: Mortalidad en infantes

ggplot(LaBuena2015, aes(infant.deaths)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 10) +
  geom_vline(xintercept = mean(LaBuena2015$infant.deaths), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$infant.deaths) + sd(LaBuena2015$infant.deaths), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$infant.deaths) - sd(LaBuena2015$infant.deaths), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Mortalidad en infantes", 
       caption = "Equipo: Syntax error",
       x = "Tasa de mortalidad en menores de un a�o",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 74, y = 5, 
             label = paste(" Media:", round(mean(LaBuena2015$infant.deaths), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$infant.deaths), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$infant.deaths), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$infant.deaths), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$infant.deaths), 3), ""), 
             hjust = 1, vjust = 0.5)


## Distribuci�n de variable 4: Hepatitis B

ggplot(LaBuena2015, aes(Hepatitis.B)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 12) +
  geom_vline(xintercept = mean(LaBuena2015$Hepatitis.B), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Hepatitis.B) + sd(LaBuena2015$Hepatitis.B), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Hepatitis.B) - sd(LaBuena2015$Hepatitis.B), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Hepatitis B", 
       caption = "Equipo: Syntax error",
       x = "N�mero de personas inmunizadas a lo largo de su primer a�o de vida contra la Hepatitis B",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 7, y = 5, 
             label = paste(" Media:", round(mean(LaBuena2015$Hepatitis.B), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Hepatitis.B), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Hepatitis.B), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Hepatitis.B), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Hepatitis.B), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 5: Measles

ggplot(LaBuena2015, aes(Measles)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 43) +
  geom_vline(xintercept = mean(LaBuena2015$Measles), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Measles) + sd(LaBuena2015$Measles), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Measles) - sd(LaBuena2015$Measles), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Sarampi�n", 
       caption = "Equipo: Syntax error",
       x = "N�mero de casos registrados de sarampi�n por cada 1,000 habitantes.",
       y = "Conteo de paises") +
  xlim(-30, 5460) +
  geom_label(geom = "text", x = 5300, y = 5.8, 
             label = paste(" Media:", round(mean(LaBuena2015$Measles), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Measles), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Measles), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Measles), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Measles), 3), ""), 
             hjust = 1, vjust = 0.5)


## Distribuci�n de variable 6: IBM

ggplot(LaBuena2015, aes(BMI)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 10) +
  geom_vline(xintercept = mean(LaBuena2015$BMI), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$BMI) + sd(LaBuena2015$BMI), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$BMI) - sd(LaBuena2015$BMI), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "�ndice de masa corporal", 
       caption = "Equipo: Syntax error",
       x = "IMC promedio en la poblaci�n",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = -6.5, y = 2.5, 
             label = paste(" Media:", round(mean(LaBuena2015$BMI), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$BMI), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$BMI), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$BMI), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$BMI), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 7: Under.five.deaths

ggplot(LaBuena2015, aes(under.five.deaths)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 10) +
  geom_vline(xintercept = mean(LaBuena2015$under.five.deaths), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$under.five.deaths) + sd(LaBuena2015$under.five.deaths), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$under.five.deaths) - sd(LaBuena2015$under.five.deaths), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Muertes por debajo de los cinco a�os", 
       caption = "Equipo: Syntax error",
       x = "Tasa de mortalidad de ni�os menores a 5 a�os",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 100, y = 5, 
             label = paste(" Media:", round(mean(LaBuena2015$under.five.deaths), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$under.five.deaths), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$under.five.deaths), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$under.five.deaths), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$under.five.deaths), 3), ""), 
             hjust = 1, vjust = 0.5)


## Distribuci�n de variable 8: Schooling

ggplot(LaBuena2015, aes(Schooling)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 3) +
  geom_vline(xintercept = mean(LaBuena2015$Schooling), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Schooling) + sd(LaBuena2015$Schooling), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Schooling) - sd(LaBuena2015$Schooling), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Escolaridad", 
       caption = "Equipo: Syntax error",
       x = "A�os de escolaridad en la poblaci�n",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 7.5, y = 5, 
             label = paste(" Media:", round(mean(LaBuena2015$Schooling), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Schooling), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Schooling), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Schooling), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Schooling), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 9: Polio

ggplot(LaBuena2015, aes(Polio)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 15) +
  geom_vline(xintercept = mean(LaBuena2015$Polio), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Polio) + sd(LaBuena2015$Polio), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Polio) - sd(LaBuena2015$Polio), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Polio", 
       caption = "Equipo: Syntax error",
       x = "N�mero de personas inmunizadas a lo largo de su primer a�o de vida contra la poliomielitis",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 2.5, y = 5, 
             label = paste(" Media:", round(mean(LaBuena2015$Polio), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Polio), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Polio), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Polio), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Polio), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 10: Diphtheria

ggplot(LaBuena2015, aes(Diphtheria)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 10) +
  geom_vline(xintercept = mean(LaBuena2015$Diphtheria), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Diphtheria) + sd(LaBuena2015$Diphtheria), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Diphtheria) - sd(LaBuena2015$Diphtheria), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Difteria", 
       caption = "Equipo: Syntax error",
       x = "N�mero de personas inmunizadas a lo largo de su primer a�o de vida contra DTP3",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 3, y = 4, 
             label = paste(" Media:", round(mean(LaBuena2015$Diphtheria), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Diphtheria), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Diphtheria), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Diphtheria), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Diphtheria), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 11: HIV.AIDS

ggplot(LaBuena2015, aes(HIV.AIDS)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 30) +
  geom_vline(xintercept = mean(LaBuena2015$HIV.AIDS), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$HIV.AIDS) + sd(LaBuena2015$HIV.AIDS), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$HIV.AIDS) - sd(LaBuena2015$HIV.AIDS), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "HIV / AIDS", 
       caption = "Equipo: Syntax error",
       x = "Muertes por cada 1,000 nacimientos entre los 0 y 4 a�os a causa del virus de inmunodeficiencia humana",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 3, y = 4.5, 
             label = paste(" Media:", round(mean(LaBuena2015$HIV.AIDS), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$HIV.AIDS), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$HIV.AIDS), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$HIV.AIDS), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$HIV.AIDS), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 12: Income composition of resources

ggplot(LaBuena2015, aes(Income.composition.of.resources)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 30) +
  geom_vline(xintercept = mean(LaBuena2015$Income.composition.of.resources), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$Income.composition.of.resources) + sd(LaBuena2015$Income.composition.of.resources), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$Income.composition.of.resources) - sd(LaBuena2015$Income.composition.of.resources), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Income composition of resources", 
       caption = "Equipo: Syntax error",
       x = "Porcentaje total de ingresos que le corresponde a cada persona en un pa�s",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 3, y = 4.5, 
             label = paste(" Media:", round(mean(LaBuena2015$Income.composition.of.resources), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$Income.composition.of.resources), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$Income.composition.of.resources), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$Income.composition.of.resources), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$Income.composition.of.resources), 3), ""), 
             hjust = 0, vjust = 0.5)


## Distribuci�n de variable 13: Delgadez

ggplot(LaBuena2015, aes(thinness..1.19.years)) +
  geom_histogram(aes(fill = GDP_Clasification, fill = Country), color = "white", binwidth = 4) +
  geom_vline(xintercept = mean(LaBuena2015$thinness..1.19.years), size = 1) +
  geom_vline(xintercept = mean(LaBuena2015$thinness..1.19.years) + sd(LaBuena2015$thinness..1.19.years), linetype = 4, size = 0.72, alpha = 0.3) +
  geom_vline(xintercept = mean(LaBuena2015$thinness..1.19.years) - sd(LaBuena2015$thinness..1.19.years), linetype = 4, size = 0.72, alpha = 0.3) +
  labs(fill = "Clasificaci�n del PIB") +
  labs(title = "Distribuci�n de variable", 
       subtitle = "Delgadez", 
       caption = "Equipo: Syntax error",
       x = "Porcentaje de delgadez entre 10 y 19 a�os",
       y = "Conteo de paises") +
  geom_label(geom = "text", x = 10, y = 4.3, 
             label = paste(" Media:", round(mean(LaBuena2015$thinness..1.19.years), 3),"\n",
                           "Varianza:", round(var(LaBuena2015$thinness..1.19.years), 3),"\n",
                           "Desviaci�n est�ndar:", round(sd(LaBuena2015$thinness..1.19.years), 3), "\n", 
                           "Simetr�a:", round(skewness(LaBuena2015$thinness..1.19.years), 3), "\n", 
                           "Curtosis:", round(kurtosis(LaBuena2015$thinness..1.19.years), 3), ""), 
             hjust = 1, vjust = 0.5)

## Gr�ficas de pa�ses 

library(patchwork)

ggplot(LaBuena2015, aes(GDP_wb, Life.expectancy)) +
  geom_point(aes(color = GDP_Clasification, shape = Stats)) +
  geom_text(aes(label = Country, color = GDP_Clasification), 
            check_overlap = T, vjust = 0, nudge_y = 1) +
  labs(fill = factor(c("Clasificaci�n del PIB", "Rango en clasificac�n")),
       title = "Esperanza de vida seg�n el producto interno bruto", 
       subtitle = "Pa�ses seleccionados en 2015", 
       caption = "Equipo: Syntax error",
       x = "PIB per capita (en d�lares)",
       y = "Esperanza de vida (en a�os)") +
  annotate("text", vjust = 75, hjust = 1000, label = "Tajikistan", size = 30, color = "red")

z <- LaBuena %>%  filter(Year == 2014)

#Cantidad de dinero que gasta el pa�s por persona en su salud promedio

## Sierra Leone gasta much�simo en salud a partir del 2011, 
## pero no tiene buena LE (esperanza de vida)
ggplot(z, aes(Total.expenditure, Life.expectancy)) +
  geom_text(aes(label = Country, color = GDP_Clasification), 
            check_overlap = T, vjust = 0, nudge_y = 1) +
  geom_jitter(aes(color = GDP_Clasification, shape = Stats)) +
  labs(fill = "Clasificaci�n del PIB",
       title = "Esperanza de vida seg�n el presupuesto dedicado a la salud", 
       subtitle = "Pa�ses seleccionados en 2014", 
       caption = "Equipo: Syntax error",
       x = "Presupuesto destinado a la salud (porcentaje del PIB)",
       y = "Esperanza de vida (a�os)") 

##Importante notar que Sierra Leone parece dedicar 

## Se dejan de registrar datos desde el 2014; outbreak de �bola
## 2001 y 2004 - aumento de GDP
## 2006 y 2008 - baj� mucho el GDP
## Se empieza a recuperar en 2009

CostaRica <- LaBuena %>% filter(Country == "Costa Rica")
a1 <- ggplot(CostaRica, aes(Year, Total.expenditure)) +
  geom_line() + geom_point() +
  labs(title = "Presupuesto dedicado a la salud en Costa Rica (%)", 
       x = "",
       y = "Porcentaje del PIB") + 
  geom_text(label = max(CostaRica$Total.expenditure))


a2 <- ggplot(CostaRica, aes(Year, GDP_wb)) +
  geom_line() + geom_point() +
  labs(title = "Producto interno bruto per c�pita de Costa Rica", 
       x = "Tiempo (a�os 2010 - 2015)",
       caption = "Equipo = Syntax error", 
       y = "PIB en d�lares")

## Aumento evidente de LE al principio (despu�s de la civil war)
a3 <- ggplot(CostaRica, aes(Year, Life.expectancy)) +
  geom_line() + geom_point() +
  labs(title = "Esperanza de vida en Costa Rica", 
       subtitle = "2000 - 2015", 
       x = "",
       y = "A�os") 

design0 <- "C
            A
            B"

wrap_plots(list(A = a1, B = a2, C = a3), design = design0)

