#install.packages("googlesheets4")
#install.packages("RColorBrewer")  # si no está instalado

library(igraph)
library(googlesheets4)
library(RColorBrewer)

#gs4_auth() # autentificación, si el archivo es privado
gs4_deauth() #para correr esto sin autentificación (tiene que ser un archivo compartido, de preferencia, sólo lectura)
url <- "https://docs.google.com/spreadsheets/d/1_Yzm89LhvoDSFAxL5fh3sYdBXVrfR0qTT6UUGBtk1ok/edit?usp=sharing"

#cargando de cada hoja 
aristas <- read_sheet(url, sheet = "edges")
nodos <- read_sheet(url, sheet = "vertex")

class(aristas)

#como data.frame (no tibble) para evitar problemas de compatibilidad con igraph
aristas <- as.data.frame(aristas) 
nodos <- as.data.frame(nodos)
class(nodos)

#haciendo un objeto igraph 
grafo <- graph_from_data_frame(
  d = aristas, 
  directed = FALSE, 
  vertices = nodos
)

####propiedades del grafo####
grafo 
summary(grafo)
vertex.attributes(grafo)
edge.attributes(grafo)
########----------###########

V(grafo)$tipo_espacio

#calculando la antigüedad a partir de la fecha de creación 
E(grafo)$weight <- as.integer(format(Sys.Date(), "%Y")) - as.integer(E(grafo)$antiguedad) 

#Creando factores 
tipo_esp <- as.factor(V(grafo)$tipo_espacio)
paleta <- brewer.pal(nlevels(tipo_esp), "Set3")
V(grafo)$color <- paleta[as.integer(tipo_esp)]

#centralidad por intermediación
edge_betweenness(grafo) 
E(grafo)$edge_betweenness <- edge_betweenness(grafo)

#fuerza por edge_betweennes
V(grafo)$strength_e_b <- strength(grafo, weights = E(grafo)$edge_betweenness)

#escalando tamaño 
tamaño_nodos <- strength(grafo, weights = E(grafo)$edge_betweenness)
tamaño_nodos
tamano_minimo <- 7
tamano_maximo <- 25
tamanos_escalados <- tamano_minimo + 
  (tamaño_nodos - min(tamaño_nodos)) * 
  (tamano_maximo - tamano_minimo) / 
  (max(tamaño_nodos) - min(tamaño_nodos))


#creando el grafo
set.seed(67)
plot.igraph(grafo,
            vertex.frame.color = "#ffffff",
            vertex.color = V(grafo)$color,
            vertex.shape = "circle",
            vertex.size  = tamanos_escalados,
            vertex.label = V(grafo)$name,
            vertex.label.color = "black",
            vertex.label.family = "sans",
            vertex.label.font = 2,
            vertex.label.cex = 0.5,
            edge.width = E(grafo)$weight * .20,
            edge.color = "grey",
            layout = layout_with_dh)


