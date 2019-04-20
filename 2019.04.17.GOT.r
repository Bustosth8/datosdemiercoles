# install.packages(readr)
#install.packages("oce")

library(readr)
library(stringr)
library(tidyr)
library(dbplyr)
library(tidyverse)
library(oce)
library(ggthemes)

tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")

cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")

personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")


##contando cambio de lealtades por pj

cambio_lealtades <- mutate(cambio_lealtades, total_cambios = NA)

for (i in 1:nrow(cambio_lealtades)) {
contador <- 0

if (cambio_lealtades$lealtad_inicial[i] != cambio_lealtades$fin_t1[i]) {contador <- contador + 1}
if (cambio_lealtades$fin_t1[i] != cambio_lealtades$fin_t2[i]) {contador <- contador + 1}
if (cambio_lealtades$fin_t2[i] != cambio_lealtades$fin_t3[i]) {contador <- contador + 1}
if (cambio_lealtades$fin_t3[i] != cambio_lealtades$fin_t4[i]) {contador <- contador + 1}
if (cambio_lealtades$fin_t4[i] != cambio_lealtades$fin_t5[i]) {contador <- contador + 1}
if (cambio_lealtades$fin_t5[i] != cambio_lealtades$fin_t6[i]) {contador <- contador + 1}
if (cambio_lealtades$fin_t6[i] != cambio_lealtades$fin_t7[i]) {contador <- contador + 1}

cambio_lealtades$total_cambios[i] <-  contador

}

##Graficamos - cambio de lealtades por apariciones

#ggplot(cambio_lealtades)+
  #geom_point(aes(x = episodios, y = total_cambios, color = origen))
  #geom_histogram(aes(x = total_cambios))

#no encontramos nada significativo
  
##cruzamos total cambios por minutos en pantalla
  
cambio_lealtades_por_minutos_en_pantalla <- merge(select(cambio_lealtades, nombre, total_cambios), tiempo_pantalla, by = "nombre")

##graficamos - cambio de lealtades por minutos en pantalla
ggplot(cambio_lealtades_por_minutos_en_pantalla) +
  #geom_point(aes(x = minutos_pantalla, y = total_cambios))
  #geom_histogram(aes(x = minutos_pantalla))
  geom_point(aes(x = minutos_pantalla, y = episodios))

## y si consideramos el género?
pj_genero_lealtades <- merge(cambio_lealtades_por_minutos_en_pantalla, select(personajes_libros, nombre, genero))
#pj_genero_lealtades <- arrange(pj_genero_lealtades, -minutos_pantalla) %>% 
                        #head(50)

##graficamos

ggplot(pj_genero_lealtades)+
  #geom_point(aes(x = minutos_pantalla, y = total_cambios, color = genero))
  #geom_histogram(aes(x = total_cambios))+
  #facet_wrap(~genero)
  geom_histogram(aes(x = minutos_pantalla, fill = genero))+
  theme(axis.text.x = element_text(vjust = 0.5))+
  #scale_x_continuous("Cantidad de minutos en los que aparece", 
                     #breaks = pj_genero_lealtades$minutos_pantalla)+
  #scale_y_continuous("Cantidad de personajes", 
                     #breaks = pj_genero_lealtades$minutos_pantalla)+
  labs(title = "Total de minutos que aparece cada personaje",
       subtitle = "#DatosDeMiércoles")+
  theme_minimal()

##total minutos pantalla x genero
pj_genero_lealtades %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  group_by(genero) %>% 
  summarise(minutos = sum(minutos_pantalla), suma_episodios = sum(episodios)) %>% 
  ggplot(aes(x=genero, fill = genero))+
  geom_bar(aes(y = minutos), stat = "identity")+
  geom_bar(aes(y = suma_episodios), stat = "identity")+
  labs(title = "Total de minutos en pantalla por género",
       subtitle = "#DatosDeMiércoles",
       x = "Género",
       y = "Minutos en pantalla")+
  theme_bw()

##filtramos personajes que aparecen poco

pj_genero_lealtades_mas_apariciones <- arrange(pj_genero_lealtades, -minutos_pantalla) %>% 
                                      head(10)

ggplot(pj_genero_lealtades_mas_apariciones)+
  geom_histogram(aes(x = minutos_pantalla, fill = genero))+
  theme(axis.text.x = element_text(vjust = 0.5))+
  #scale_x_continuous("x", breaks = pj_genero_lealtades$episodios)+
  #scale_y_continuous("y", breaks = pj_genero_lealtades$episodios)+
  labs(title = "Total de minutos que aparece cada personaje",
       subtitle = "#DatosDeMiércoles")+
  theme_minimal()

  ##tasa de muertes por genero
total_masculino <- nrow(personajes_libros[personajes_libros$genero == "masculino",])
total_femenino <- nrow(personajes_libros[personajes_libros$genero == "femenino",])

personajes_libros_muertos <- filter(personajes_libros, !is.na(libro_muerte))
muertos_masculino <- nrow(personajes_libros_muertos[personajes_libros_muertos$genero == "masculino",])
muertos_femenino <- nrow(personajes_libros_muertos[personajes_libros_muertos$genero == "femenino",])

muertos_masculino / total_masculino
muertos_femenino / total_femenino
