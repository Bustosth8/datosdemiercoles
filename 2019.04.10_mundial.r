library(tidyverse)
library(ggplot2)
library(dplyr)

##carga de datos
#install.packages("readr")

library(readr)
partidos <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

##cuantos goles argentinos por año
goles_arg <- filter(partidos, equipo_1 == "Argentina" | equipo_2 == "Argentina") %>%
            select(anio, fecha, equipo_1, equipo_2, equipo_1_final, equipo_2_final) %>% 
            mutate(goles_arg = case_when(
              equipo_1 == "Argentina" ~ equipo_1_final,
              equipo_2 == "Argentina" ~ equipo_2_final))

goles_arg_anio <- group_by(goles_arg, anio) %>% 
  summarise(goles_por_anio = sum(goles_arg))

##cuantos goles brasileros por año
goles_br <- filter(partidos, equipo_1 == "Brasil" | equipo_2 == "Brasil") %>%
  select(anio, fecha, equipo_1, equipo_2, equipo_1_final, equipo_2_final) %>% 
  mutate(goles_br = case_when(
    equipo_1 == "Brasil" ~ equipo_1_final,
    equipo_2 == "Brasil" ~ equipo_2_final))

goles_br_anio <- group_by(goles_br, anio) %>% 
  summarise(goles_por_anio = sum(goles_br))

##cuantos goles colombianos por año
goles_col <- filter(partidos, equipo_1 == "Colombia" | equipo_2 == "Colombia") %>%
  select(anio, fecha, equipo_1, equipo_2, equipo_1_final, equipo_2_final) %>% 
  mutate(goles_col = case_when(
    equipo_1 == "Colombia" ~ equipo_1_final,
    equipo_2 == "Colombia" ~ equipo_2_final))

goles_col_anio <- group_by(goles_col, anio) %>% 
  summarise(goles_por_anio = sum(goles_col))

##unificar
goles_anio <- goles_br_anio
colnames(goles_anio)[2] <- "goles_br"
goles_anio <- left_join(goles_anio, goles_arg_anio, by = c("anio" = "anio"))
colnames(goles_anio)[3] <- "goles_arg"
goles_anio <- left_join(goles_anio, goles_col_anio, by = c("anio" = "anio"))
colnames(goles_anio)[4] <- "goles_col"

#visualizacion goles_arg_anio
ggplot(data = goles_arg_anio, aes(x = anio, y = goles_por_anio)) + 
  geom_line(color = "blue", size = 0.7)+
  geom_point(color = "blue") +
  labs(title = "Total de goles argentinos por año",
       subtitle = "#DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "Goles",
       y = "Año")

##visualizacion goles_anio
goles_plot <- ggplot(goles_anio, aes(x=anio))+
  geom_line(aes(y = goles_arg), color = "blue") +
  geom_line(aes(y = goles_br), color = "green") +
  geom_line(aes(y = goles_col), color = "red") +
  labs(title = "Total de goles por año",
       subtitle = "#DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "Goles",
       y = "Año")

##visualización tercer intento
ggplot(goles_anio, aes(x=anio)) +
  geom_line(aes(y=goles_arg, colour = "Argentina")) +
  geom_line(aes(y=goles_br, colour="Brasil")) +
  geom_line(aes(y=goles_col, colour="Colombia")) +
  scale_color_manual(values = c("blue", "green", "red"), name = "Paises" ) +
  labs(title = "Total de goles por año",
       subtitle = "#DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "Goles",
       y = "Año")+
  theme_classic()


ggsave(goles_plot, filename = "goles por equipo.png", width = 14, height = 7)
