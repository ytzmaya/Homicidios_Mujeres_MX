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
pacman::p_load(tidyverse, purrr)

#-------------------------------------------------------------------------------
# 01. Ejercicio lógico --------------------------------------------------------#
#-------------------------------------------------------------------------------

# Imagínate que tienes 4 listas: lista1, lista2, lista3, lista4. La lista1 con-
# tiene todas las letras del alfabeto, las otras listas contienen subconjuntos 
# distintos del alfabeto. 

### Creación de lista1:
lista1 <- data.frame(LETTERS)

### Creación de lista2, lista3 y lista4 a partir de los subconjuntos de lista1

# tamaño de la muestra
n<-10
# fijar semillas para que el ejercicio sea reproducible
set.seed(123)
lista2 <- sample_n(lista1, size=n,replace=TRUE)

set.seed(456)
lista3 <- sample_n(lista1, size=n,replace=TRUE)

set.seed(789)
lista4 <- sample_n(lista1, size=n,replace=TRUE)

# A partir de las funciones descritas en la siguiente documentación 
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join 
# muestra cómo responderías a las siguientes preguntas:

## 1. ¿Qué tipo de objeto tienen que ser las listas para poder encontrar elemen-
#     tos en común entre ellas?

### R1: Un data frame, ya que son "listas" cuyos elementos se componen de vecto-
#       res de la misma longitud. Así, se pueden hacer comparaciones. 

### 2. ¿Qué letras del abecedario aparecen en la lista2, lista3 y lista4?

### R2: C y E (son las letras que apareden en todas las listas)
list(lista1, lista2, lista3, lista4) %>% 
  reduce(inner_join)

### 3. ¿Qué letras del abecedario no aparecen en la lista2, lista3 ni en la 
# lista4?

### R3: A, B, G, H, Q, W, X
list(lista1, lista2, lista3, lista4) %>% 
  reduce(anti_join)

### 4. ¿Qué letras del abecedario aparecen en la lista4 y en la lista3 pero no 
# en la lista2?

### R4: M y Z aparecen en la lista3 y lista4, pero no en la lista2
list(lista1, lista3, lista4) %>% 
  reduce(inner_join) %>% 
  anti_join(lista2)





                    