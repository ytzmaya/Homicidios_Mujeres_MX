#------------------------------------------------------------------------------#
#          EVALUACIÓN PARA VACANTE DE ANALISTA MID EN DATA CÍVICA
#
# Realizada por: Ytzel Maya Jiménez 
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 00. Configuración inicial ---------------------------------------------------#
#-------------------------------------------------------------------------------

# Cargar librerías 
require(pacman)
pacman::p_load(tidyverse, sf, scales, biscale, cowplot, RColorBrewer, viridis,
               foreign, purrr)

# Paleta de color 
color <- c("lightpink", "snow4", "royalblue", "mediumaquamarine", "turquoise4",
           "yellow3", "violetred1", "palegreen3", "plum3")

# # Creación de tema para grafs
tema <-  theme_linedraw() +
  theme(text = element_text(family = "Georgia", color = "grey35"),
        plot.title = element_text(size = 20, face = "bold", margin = margin(10,4,
                                                                            5,4), 
                                  family="Georgia", color = "black"),
        plot.subtitle = element_text(size = 16, face = "bold", color = "#666666", 
                                     margin = margin(5, 5, 5, 5), family="Georgia"
        ),
        plot.caption = element_text(hjust = 0, size = 10, family = "Georgia"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "top",
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 14, face = "bold", family="Georgia"),
        legend.text = element_text(size = 12, family="Georgia"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 14, hjust = .2, face = "bold", margin = 
                                    margin(1,1,1,1), family="Georgia"),
        axis.text = element_text(size = 12, face = "bold", family="Georgia"),
        strip.background = element_rect(fill="#525252"),
        strip.text.x = element_text(size=12, face="bold", family = "Georgia"),
        strip.text.y = element_text(size=12, face="bold", family = "Georgia"))

#-------------------------------------------------------------------------------
# 01. Ejercicio práctico ------------------------------------------------------#
#-------------------------------------------------------------------------------

###                    Realización de tablas

###----------------------------------------------2.a SESNSP (homicidios dolosos)
sesnsp <- IDVFC_NM_jun22 %>% 
  filter(`Tipo de delito` == "Homicidio" & 
           `Subtipo de delito` == "Homicidio doloso" &
           Año <= 2020) %>% 
  group_by(Año, Entidad, Modalidad, `Rango de edad`, Sexo) %>% 
  mutate(Total = sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio,
                     Agosto, Septiembre, Octubre, Noviembre, Diciembre)) %>% 
  select(Año, Entidad, Sexo, Total) %>% 
  group_by(Año, Entidad, Sexo) %>%                        
  summarise_at(vars(Total),                 
               list(TOTHOM = sum)) 

sesnsp$Entidad[sesnsp$Entidad=="Coahuila de Zaragoza"]<-"Coahuila"
sesnsp$Entidad[sesnsp$Entidad=="Michoacán de Ocampo"]<-"Michoacán" 
sesnsp$Entidad[sesnsp$Entidad=="Veracruz de Ignacio de la Llave"]<-"Veracruz" 

# Guardar tabla
write.csv(sesnsp, "pregunta_2_a.csv") 

# se eliminan los no identificados para unir las tablas
sesnsp <- sesnsp %>% 
  filter(Sexo != "No identificado")


###------------------ 3.a Mortalidad INEGI (víctimas de homicidio por modalidad)

# Catálogo de causas de defunción (descrip). Se usa el más reciente
causas_def<-read.dbf("CATMINDE.DBF", as.is = TRUE) %>% 
  rename(causa_def = CVE)
Encoding(causas_def$DESCRIP) <-"UTF-8"

# Unir bases
df_unida <- defunciones_generales_2015 %>% 
  full_join(defunciones_generales_2016) %>% 
  full_join(conjunto_de_datos_defunciones_generales_2017) %>% 
  select(-lista1, -edad_agru) %>% 
  full_join(conjunto_de_datos_defunciones_registradas_2018) %>% 
  select(-lista1, -edad_agru) %>% 
  full_join(conjunto_de_datos_defunciones_registradas_2019) %>% 
  full_join(conjunto_de_datos_defunciones_registradas_2020) %>% 
  # aligeramos la base filtrando la variable que nos interesa:
  # homicidio
  filter(presunto == "2")

# Relización de tabla

# Seleccionar variables que nos interesan:
# Año, causa de defunción (modalidad de homicidio)
hom_mod <- df_unida %>% 
  select(anio_ocur, causa_def, sexo) %>% 
  # unir con descriptivos 
  left_join(causas_def, by = "causa_def") %>% 
  # categorización de homicidios
  mutate(Modalidad = case_when(grepl("con disparo de", DESCRIP) ~ "Arma de fuego",
                               grepl("objeto cortante", DESCRIP) ~ "Arma blanca",
                               grepl("objeto romo", DESCRIP) ~ "Arma blanca",
                               grepl("cuchillo", DESCRIP) ~ "Arma blanca",
                               grepl("ahorcamiento", DESCRIP) ~ "Ahorcamiento o ahogamiento",
                               grepl("ahogamiento", DESCRIP) ~ "Ahorcamiento o ahogamiento",
                               grepl("sustancias nocivas", DESCRIP) ~ "Envenenamiento o sustancia nociva",
                               grepl("drogas", DESCRIP) ~ "Envenenamiento o sustancia nociva",
                               grepl("gases", DESCRIP) ~ "Envenenamiento o sustancia nociva",
                               grepl("plaguicidas", DESCRIP) ~ "Envenenamiento o sustancia nociva",
                               grepl("sexual", DESCRIP) ~ "Agresión sexual",
                               grepl("golpe contra", DESCRIP) ~ "Fuerza corporal",
                               grepl("fuerza corporal", DESCRIP) ~ "Fuerza corporal",
                               grepl("maltrato", DESCRIP) ~ "Maltrato o abandono",
                               grepl("abandono", DESCRIP) ~ "Maltrato o abandono",
                               grepl("llamas", DESCRIP) ~ "Agresiones con fuego o explosivos",
                               grepl("explosivo", DESCRIP) ~ "Agresiones con fuego o explosivos",
                               grepl("Secuelas de", DESCRIP) ~ "Secuelas de agresiones",
                               grepl("no especificado", DESCRIP) ~ "Medios no especificados")) %>% 
  mutate(Sexo = 
           case_when(sexo == 1 ~ "Hombre",
                     sexo == 2 ~ "Mujer",
                     sexo == 9 ~ "No especificado")) %>% 
  select(anio_ocur, Modalidad, Sexo) %>% 
  group_by(anio_ocur, Modalidad, Sexo) %>% 
  count() %>% 
  filter(anio_ocur >= 2015 & anio_ocur != 9999) %>% 
  rename(Año = anio_ocur, Total = n) 

# Guardar tabla
write.csv(hom_mod, "pregunta_3_a.csv") 
  
###-------- 3.b Mortalidad INEGI (víctimas de homicidio por año, entidad y sexo)

# Agregar nombres a las variables de entidad y sexo

hom_inegi <- df_unida %>% 
  mutate(Sexo = 
           case_when(sexo == 1 ~ "Hombre",
                     sexo == 2 ~ "Mujer",
                     sexo == 9 ~ "No especificado")) %>% 
  mutate(Entidad =
           case_when(ent_ocurr == "01" ~ "Aguascalientes",
                     ent_ocurr == "02" ~ "Baja California",
                     ent_ocurr == "03" ~ "Baja California Sur",
                     ent_ocurr == "04" ~ "Campeche",
                     ent_ocurr == "05" ~ "Coahuila",
                     ent_ocurr == "06" ~ "Colima",
                     ent_ocurr == "07" ~ "Chiapas",
                     ent_ocurr == "08" ~ "Chihuahua",
                     ent_ocurr == "09" ~ "Ciudad de México",
                     ent_ocurr == "10" ~ "Durango",
                     ent_ocurr == "11" ~ "Guanajuato",
                     ent_ocurr == "12" ~ "Guerrero",
                     ent_ocurr == "13" ~ "Hidalgo",
                     ent_ocurr == "14" ~ "Jalisco",
                     ent_ocurr == "15" ~ "México",
                     ent_ocurr == "16" ~ "Michoacán",
                     ent_ocurr == "17" ~ "Morelos",
                     ent_ocurr == "18" ~ "Nayarit",
                     ent_ocurr == "19" ~ "Nuevo León",
                     ent_ocurr == "20" ~ "Oaxaca",
                     ent_ocurr == "21" ~ "Puebla",
                     ent_ocurr == "22" ~ "Querétaro",
                     ent_ocurr == "23" ~ "Quintana Roo",
                     ent_ocurr == "24" ~ "San Luis Potosí",
                     ent_ocurr == "25" ~ "Sinaloa",
                     ent_ocurr == "26" ~ "Sonora",
                     ent_ocurr == "27" ~ "Tabasco",
                     ent_ocurr == "28" ~ "Tamaulipas",
                     ent_ocurr == "29" ~ "Tlaxcala",
                     ent_ocurr == "30" ~ "Veracruz",
                     ent_ocurr == "31" ~ "Yucatán",
                     ent_ocurr == "32" ~ "Zacatecas")) %>% 
  select(anio_ocur, Entidad, Sexo) %>% 
  group_by(anio_ocur, Entidad, Sexo) %>% 
  count() %>% 
  rename(Año = anio_ocur, TOTHOM_INEGI = n) %>% 
  filter(Año >= 2015 & Año != 9999) 

# Guardar tabla
write.csv(hom_inegi, "pregunta_3_b.csv") 

# se eliminan "no identificados" para unir tablas
hom_inegi<- hom_inegi %>% 
  filter(Sexo != "No especificado")

###--------------------- 4,a Población CONAPO (habitantes x año, entidad y sexo)

# Se usan las proyecciones por inicio de año
#pob <- 

pob <- pob_ini_proyecciones %>% 
  filter(AÑO >= 2015 & AÑO <= 2020 & ENTIDAD != "República Mexicana") %>% 
  select(AÑO, ENTIDAD, SEXO, POBLACION) %>% 
  group_by(AÑO, ENTIDAD, SEXO) %>%                        
  summarise_at(vars(POBLACION),                 
               list(POBTOT = sum)) %>% 
  rename(Año = AÑO, Entidad = ENTIDAD, Sexo = SEXO)

pob$Sexo[pob$Sexo=="Mujeres"]<-"Mujer"
pob$Sexo[pob$Sexo=="Hombres"]<-"Hombre"
  
# Guardar tabla
write.csv(pob, "pregunta_4_a.csv") 

###-------------------------------------------------- 5. Generar una sola tabla

homicidios <- list(pob, sesnsp, hom_inegi) %>% 
  reduce(full_join)

write.csv(homicidios, "pregunta_5.csv") 

# Notas: INEGI no registró algunas entidades de defunción, se conservan para 
# cotejo


#-------------------------------------------------------------------------------
# 02. Visualizaciones ---------------------------------------------------------#
#-------------------------------------------------------------------------------


#                              Mortalidad INEGI

#  3.c Realiza un máximo de dos visualizaciones que te permitan plasmar cómo se
# diferencian los asesinatos de hombres y los asesinatos de mujeres en México y
# cómo ha cambiado su naturaleza a través del tiempo.


# Gráfica (x modalidad)

grafhom_mod <- hom_mod %>% 
  group_by(Sexo, Modalidad, Año) %>% 
  summarize(Total = sum(Total, na.rm = T)) %>% 
  ungroup() %>% 
  filter(Sexo != "No especificado") %>% 
  group_by(Sexo, Año) %>% 
  mutate(denomin = sum(Total, na.rm = T),
         porcent = round(Total / denomin * 100, 2)) %>% 
  na.omit()

ggplot(grafhom_mod, 
       aes(x = as.integer(Año), y = porcent, fill= reorder(Modalidad, porcent), 
           order = Modalidad)) +
  #Geoms
  geom_col(position = "fill") +
  tema +
  scale_x_continuous(breaks=seq(from=2015, to=2020, by=1)) +
  scale_y_continuous(labels = percent)+
  scale_fill_manual(values=color) +
  labs(title = "Porcentaje de homicidios por sexo y modalidad",
       x = "Año",
       y = "Porcentaje",
       subtitle = "De 2015 a 2020",
       fill = "",
       caption = "Fuente: Registro de mortalidad del INEGI
Nota: Se usa el año de ocurrencia del homicidio
Datos procesados por Ytzel Maya")+
  theme(legend.position = "top")+
  facet_wrap(~Sexo)


#               Datos de homicidios: INEGI, SESNSP y CONAPO

# 6.Genera un máximo de dos visualizaciones que te permitan comparar las fuentes 
# de datos utilizadas y resaltar los estados más peligrosos para las mujeres en 
# México


# sacar tasas con info de CONAPO
tasa_hom <- homicidios %>% 
  group_by(Año, Entidad, Sexo) %>% 
  mutate(tasa_sesnsp = round((TOTHOM/ POBTOT * 100000), 1),
         tasa_inegi = round((TOTHOM_INEGI / POBTOT * 100000), 1)) 

# tasa de 2015 a 2020 x entidad (sólo mujeres)
df1 <- homicidios %>% 
  filter(Sexo == "Mujer") %>% 
  select(Año, Entidad, POBTOT, TOTHOM_INEGI, TOTHOM) %>% 
  group_by(Entidad) %>%                        
  summarise_at(vars(POBTOT),                 
               list(pob15_20 = sum)) 
df2 <- homicidios %>% 
  filter(Sexo == "Mujer") %>% 
  select(Año, Entidad, POBTOT, TOTHOM_INEGI, TOTHOM) %>% 
  group_by(Entidad) %>%                        
  summarise_at(vars(TOTHOM_INEGI),                 
               list(inegi15_20 = sum))  
df3 <- homicidios %>% 
  filter(Sexo == "Mujer") %>% 
  select(Año, Entidad, POBTOT, TOTHOM_INEGI, TOTHOM) %>% 
  group_by(Entidad) %>%                        
  summarise_at(vars(TOTHOM),                 
               list(sesnsp15_20 = sum)) 
homicidios1520 <- list(df1, df2, df3) %>% 
  reduce(full_join) %>% 
  group_by(Entidad) %>% 
  mutate(SESNSP = round((sesnsp15_20/ pob15_20 * 100000), 1),
         INEGI = round((inegi15_20 / pob15_20 * 100000), 1)) %>% 
  na.omit() %>% 
  select(Entidad, SESNSP, INEGI) %>% 
#para diferenciar datos de SESNSP e INEGI
  pivot_longer(!Entidad, names_to = "tipo_tasa", values_to = "tasa")

# Mapa x entidad con tasas

ents <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
  rename(Entidad = ENTIDAD) 

ents$Entidad[ents$Entidad=="Coahuila de Zaragoza"]<-"Coahuila"
ents$Entidad[ents$Entidad=="Michoacan de Ocampo"]<-"Michoacán" 
ents$Entidad[ents$Entidad=="Querétaro de Arteaga"]<-"Querétaro"
ents$Entidad[ents$Entidad=="San Luís Potosi"]<-"San Luis Potosí" 
ents$Entidad[ents$Entidad=="Veracruz de Ignacio de La Llave"]<-"Veracruz" 

mapaxtasa <-homicidios1520 %>% 
  left_join(ents, by = c("Entidad")) 

mapaxtasa %>% 
  ggplot(aes(geometry = geometry, fill=INEGI)) +
  geom_sf(colour = "white", size = 0.07) +
  tema+
  labs(title = "Tasa de homicidio de mujeres por entidad",
       subtitle = "De 2015 a 2020",
       x = "",
       y = "",
       caption = "Fuente: Registro de mortalidad del INEGI y CONAPO
Nota: Se usa el año de ocurrencia del homicidio
Datos procesados por Ytzel Maya",
       fill = "tasa")+
  scale_fill_distiller(palette = "PuRd", direction = 1)


#                Segunda parte: análisis complementario


### Comparación tasa hombres / mujeres INEGI x entidad

df1 <- homicidios %>%  
  select(Año, Entidad, Sexo, POBTOT, TOTHOM_INEGI) %>% 
  group_by(Entidad, Sexo) %>%                        
  summarise_at(vars(POBTOT),                 
               list(pob15_20 = sum)) 
df2 <- homicidios %>% 
  select(Año, Entidad, Sexo, POBTOT, TOTHOM_INEGI) %>% 
  group_by(Entidad, Sexo) %>%                        
  summarise_at(vars(TOTHOM_INEGI),                 
               list(inegi15_20 = sum))  
homicidios1520_wide <- df1 %>% 
  full_join(df2) %>% 
  group_by(Entidad, Sexo) %>% 
  mutate(Tasa = round((inegi15_20 / pob15_20 * 100000), 1)) %>% 
  na.omit() %>% 
  select(Entidad, Sexo, Tasa) %>% 
  pivot_wider(names_from = Sexo, values_from = Tasa)

  homicidios1520_long <- df1 %>% 
  full_join(df2) %>% 
  group_by(Entidad, Sexo) %>% 
  mutate(Tasa = round((inegi15_20 / pob15_20 * 100000), 1)) %>% 
  na.omit() %>% 
  select(Entidad, Sexo, Tasa) 
  
# Gráfica tasa x entidad y sexo

ggplot() +
  geom_segment(data = homicidios1520_wide, 
               aes(x    = Hombre, 
                   xend = Mujer, 
                   y    = reorder(Entidad, Hombre), 
                   yend = reorder(Entidad, Mujer)),
               size = 1, colour = '#D0D0D0') +
  geom_point(data = homicidios1520_long,
             aes(x      = Tasa, 
                 y      = Entidad, 
                 colour = Sexo),
             size = 4) +
  labs(title = "Tasa de homicidios por entidad",
       x = "Tasa por 100mil habitantes",
       y = "Entidad",
       subtitle = "De 2015 a 2020",
       fill = "",
       caption = "Fuente: Registro de mortalidad del INEGI y CONAPO
Nota: Se usa el año de ocurrencia del homicidio
Datos procesados por Ytzel Maya")+
  theme(legend.position = "top")+
  scale_colour_manual(values = c("plum3", "mediumaquamarine")) +
  tema



#-------------------------------------------------------------------------------
# 03. Análisis complementario -------------------------------------------------#
#-------------------------------------------------------------------------------


### Preguntas para el análisis

#-----------------------¿Cuántos homicidios se registraron de 2015 a 2020?

#INEGI (184,587, sin contar las personas no identificadas)
sum(hom_inegi$TOTHOM_INEGI)

#SESNSP (168,250)
sum(sesnsp$TOTHOM)

#---------------------- ¿Cuántos son de hombres y cuántos de mujeres?
hom_inegi %>% 
  group_by(Sexo) %>%                        
  summarise_at(vars(TOTHOM_INEGI),                 
               list(x = sum)) 
# INEGI: 164,560 hombres / 20,027 mujeres

sesnsp %>% 
  group_by(Sexo) %>%                        
  summarise_at(vars(TOTHOM),                 
               list(x = sum)) 
#SESNSP: 153,368 hombres / 14,882 mujeres

### Gráfica comparación de tasas INEGI y SESNSP

ggplot(homicidios1520, 
       aes(x = tasa, y =reorder (Entidad, tasa), fill = as.factor(tipo_tasa))) +
  geom_col(position = position_dodge())+
  
  geom_label(aes(group = factor(tipo_tasa),
                 label = tasa), 
             position = position_dodge(width = .5), fill = "white", 
             family = "Avenir Next Condensed") +
  tema +
  scale_fill_manual(values=color) +
  labs(title = "Tasa de homicidios de mujeres",
       subtitle = "De 2015 a 2020 (por 100mil habitantes)",
       x = "Tasa",
       y = "Entidad",
       fill = "Institución que registró el homicidio",
       caption = "Fuente: Registro de mortalidad del INEGI, SESNSP y CONAPO
Nota: Se usa el año de ocurrencia del homicidio
Datos procesados por Ytzel Maya")+
  theme(legend.position = "top")+
  facet_wrap(~tipo_tasa)


# Gráfica tasa x entidad y sexo

ggplot() +
  geom_segment(data = homicidios1520_wide, 
               aes(x    = Hombre, 
                   xend = Mujer, 
                   y    = reorder(Entidad, Hombre), 
                   yend = reorder(Entidad, Mujer)),
               size = 1, colour = '#D0D0D0') +
  geom_point(data = homicidios1520_long,
             aes(x      = Tasa, 
                 y      = Entidad, 
                 colour = Sexo),
             size = 4) +
  labs(title = "Tasa de homicidios por entidad",
       x = "Tasa por 100mil habitantes",
       y = "Entidad",
       subtitle = "De 2015 a 2020",
       fill = "",
       caption = "Fuente: Registro de mortalidad del INEGI y CONAPO
Nota: Se usa el año de ocurrencia del homicidio
Datos procesados por Ytzel Maya")+
  theme(legend.position = "top")+
  scale_colour_manual(values = c("plum3", "mediumaquamarine")) +
  tema

