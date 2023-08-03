#instalar los packetes antes
#nombre :alex Sandro cayllahua chire
library(dplyr)
library(stats)
library(base)
library(UsingR)
library(naniar)
library(factoextra)
library(ggplot2)

RecienNacidos <- read.csv("https://raw.githubusercontent.com/kair069/Kmeans/main/RecienNacidos.csv")


View(RecienNacidos)

 
##
#VER DATA NA
#

str(RecienNacidos)
vis_miss(RecienNacidos)
#VEMOS QUE UN .1 DE VALORES ESTAN OMITIDOS EN LA VARIABLE PESOANTESEMB,PESODESPUESEMB,PESOGANADO,SEMANAGESTAPESO
gg_miss_upset(RecienNacidos)
#VEMOS QUE PESO?GANADO NA TIENE MAS VALORES FALTANTES Y 3 CASOS DONDE JUNTO CON PESO DESPUES EMB Y PESO ANTES  CONJUNTAMENTE 
#NO TIENEN DATA AL IGUAL QUE  HAY 1 CASO DONDE NO HAY DATA EN PESO A,SEMANA D,PESO DE,Y PESO GANA
datos <- na.omit(RecienNacidos)
#eliminar filas con NA
vis_miss(datos)
#VERIFICAMOS LA DATA
View(datos)
#ya no hya data con na
#vemos otra ves la estructura de datos
str(datos)

datou<-data.frame(datos$ID_MUJER,datos$EDAD_MADRE,datos$PESO_ANTES_EMB,datos$PESO_DESPUES_EMB,datos$PESO_GANADO,datos$metodo_parto,datos$METODO_PARTO,datos$SEMANA_GESTACION,datos$PESO_BEBE)
df <- scale(datou)
head(df)


?scale

#librerias para k optimo


fviz_nbclust(x = df, FUNcluster = kmeans, method = "wss", k.max = 50, 
             diss = get_dist(df, method = "euclidean"), nstart = 5)

#-----------------------------------------------------------------
calcular_totwithinss <- function(n_clusters, datos, iter.max=1000, nstart=5){
  # Esta función aplica el algoritmo kmeans y devuelve la suma total de
  # cuadrados internos.
  cluster_kmeans <- kmeans(centers = n_clusters, x = df, iter.max = iter.max,
                           nstart = nstart)
  return(cluster_kmeans$tot.withinss)
}

# Se aplica esta función con para diferentes valores de k
total_withinss <- map_dbl(.x = 1:100,
                          .f = calcular_totwithinss,
                          datos = df)

data.frame(n_clusters = 1:100, suma_cuadrados_internos = total_withinss) %>%
  ggplot(aes(x = n_clusters, y = suma_cuadrados_internos)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:15) +
  labs(title = "Evolución de la suma total de cuadrados intra-cluster") +
  theme_bw()
#diagrama de kmeans

set.seed(123)
km_clusters <- kmeans(x = df, centers = 100, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = df, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")





#solo para 2 variables 

datou2<-data.frame(datos$PESO_BEBE,datos$EDAD_MADRE)
df2 <- scale(datou2)
head(df2)





#+---------------------------------------------------------------
fviz_nbclust(x = df2, FUNcluster = kmeans, method = "wss", k.max = 10, 
diss = get_dist(df2, method = "euclidean"), nstart =2)

#-----------------------------------------------------------------
calcular_totwithinss <- function(n_clusters, df2, iter.max=10, nstart=2){
  # Esta función aplica el algoritmo kmeans y devuelve la suma total de
  # cuadrados internos.
  cluster_kmeans <- kmeans(centers = n_clusters, x = df2, iter.max = iter.max,
                           nstart = nstart)
  return(cluster_kmeans$tot.withinss)
}

# Se aplica esta función con para diferentes valores de k
total_withinss <- map_dbl(.x = 1:5,
                          .f = calcular_totwithinss,
                          datos = df2)


#diagrama de kmeans

set.seed(123)
km_clusters <- kmeans(x = df2, centers = 5, nstart = 2)

#grafica

fviz_cluster(object = km_clusters, data = df2, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")