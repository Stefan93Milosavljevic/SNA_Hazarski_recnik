##### VIZUALIZACIJA MREŽE ODREDNICA HAZARSKOG RECNIKA

### Uvodna napomena: 
# Ova R-skripta delimicno je zasnovana na:
# https://github.com/jeljov/SNALabs_at_UB (lab_5)
# https://www.markanthonyhoffman.com/

###### 
### POTREBNE BIBLIOTEKE
#####
library(igraph)
library(ggplot2)
library(tidyr)
library(visNetwork)
library(qgraph)
#####
### KREIRANJE MREZE
##### 

Odrednice_veze<-read.csv('Lista_ivica.csv', header=TRUE, stringsAsFactors = F) # Ucitavamo podatke sa odrednicama i njihovim vezama
# View(Odrednice_veze) # Za pregled uvezenih podataka
# U 3. koloni frekvenca, koja ce sluziti kao tezina
Odrednice_atributi<-read.csv("Odrednice_atributi.csv", header=T, stringsAsFactors = F) # Ucitavamo podatke sa atributima
Odrednice_mreza_atributi<-graph_from_data_frame(Odrednice_veze, directed = T, vertices = Odrednice_atributi) # Na osnovu ucitanih podataka formiramo mrezu sa atributima cvorova
Odrednice_mreza_atributi
# Sledecim funkcijama proveravamo atribute u dobijenoj mrezi:
edge_attr_names(Odrednice_mreza_atributi) # Imena atributa ivica (tezine)
vertex_attr_names(Odrednice_mreza_atributi) # Imena atributa cvorova 


#####
### POCETNA VIZUALIZACIJA MREZE
#####

# Pocetna vizualizacija...
plot(Odrednice_mreza_atributi,
     layout=layout_with_kk(Odrednice_mreza_atributi),
     vertex.label.cex = .7, # Velicina oznaka (labels)
     edge.curved = .1,  # Zakrivljenost ivica
     vertex.frame.color = "black", # Boja okvira cvora
     vertex.color = 'white', # Boja cvorova; spisak boja: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
     edge.arrow.size = .3, # Velicina strelice ivica
     edge.width = .7, # sirina ivice
     edge.color = "grey30", # Boja ivica
     vertex.size = 20) # Velicina cvora 

# Specifikovanje boja da odgovaraju bojama u knjizi (Crvena, Zelena i Zuta knjiga)
V(Odrednice_mreza_atributi)$color <- NA # Resetovanje boja 
V(Odrednice_mreza_atributi)$color <- ifelse(V(Odrednice_mreza_atributi)$Knjiga == "0", "red", V(Odrednice_mreza_atributi)$color)
V(Odrednice_mreza_atributi)$color <- ifelse(V(Odrednice_mreza_atributi)$Knjiga == "1", "green", V(Odrednice_mreza_atributi)$color)
V(Odrednice_mreza_atributi)$color <- ifelse(V(Odrednice_mreza_atributi)$Knjiga == "2", "yellow", V(Odrednice_mreza_atributi)$color) 

plot(Odrednice_mreza_atributi,
     layout=layout_with_kk(Odrednice_mreza_atributi),
     vertex.label.cex = .7,
     edge.curved = .1,  
     vertex.frame.color = "black", 
     edge.arrow.size = .3, 
     edge.width = .7, 
     edge.color = "grey30", 
     vertex.size = 20)

# Debljinu ivica specifikujemo prema tezinama (frekvencama)
plot(Odrednice_mreza_atributi,
     layout=layout_with_kk(Odrednice_mreza_atributi),
     vertex.label.cex = .7,
     edge.curved = .1,  
     vertex.frame.color = "black", 
     edge.arrow.size = .7, 
     edge.width = E(Odrednice_mreza_atributi)$Frekvenca, 
     edge.color = "grey30", 
     vertex.size = 20)

# Dinamicna vizualizacija pomocu paketa VisNetwork

nodes_df <- data.frame(id=V(Odrednice_mreza_atributi)$name, stringsAsFactors = FALSE)
head(nodes_df)
edges_df <- data.frame(as_edgelist(Odrednice_mreza_atributi), stringsAsFactors = FALSE)
colnames(edges_df) <- c('from', 'to')
head(edges_df)

# Najjednostavnija mreza:
visNetwork(nodes = nodes_df, edges = edges_df, 
           main="Mreza odrednica")

# Sa ukljucenim bojama:
nodes_df$color <- V(Odrednice_mreza_atributi)$color
visNetwork(nodes = nodes_df, edges = edges_df, 
           main="Mreza odrednica")

# Specifikovanje jos nekih osobina:
edges_df$smooth <- TRUE # Za zakrivljenje ivica
edges_df$arrows <- c('to', "from") # Za strelice
nodes_df$shadow <- FALSE        # Za senke cvorova
nodes_df$title  <- nodes_df$id  # Prikazivanje teksta kad se mis postavi preko cvora
nodes_df$borderWidth <- 1.5     # Debljina granica cvora

visNetwork(nodes = nodes_df, edges = edges_df, 
           main="Mreza odrednica")

# Sa debljinama ivica prema tezini (= frekvenci)
edges_df$width <- E(Odrednice_mreza_atributi)$Frekvenca / 3
head(edges_df)
visNetwork(nodes = nodes_df, edges = edges_df, 
           main="Mreza odrednica")

# Layout iz paketa qgraph
e<-Odrednice_veze[,1:2]                                       
l.1 <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(Odrednice_mreza_atributi))
l.2 <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(Odrednice_mreza_atributi),
                                       area=8*(vcount(Odrednice_mreza_atributi)^2.5),repulse.rad=(vcount(Odrednice_mreza_atributi)^3.1))

plot(Odrednice_mreza_atributi,
     layout=l.2,
     vertex.label.cex = .7,
     edge.curved = .1,  
     vertex.frame.color = "black", 
     edge.arrow.size = .3, 
     edge.width = .7, 
     edge.color = "grey30", 
     vertex.size = 17)

# Sa tezinama:

plot(Odrednice_mreza_atributi,
     layout=l.2,
     vertex.label.cex = .7,
     edge.curved = .1,  
     vertex.frame.color = "black", 
     edge.arrow.size = .7, 
     edge.width = (E(Odrednice_mreza_atributi)$Frekvenca)/4+1, 
     edge.color = "grey30", 
     vertex.size = 17)
