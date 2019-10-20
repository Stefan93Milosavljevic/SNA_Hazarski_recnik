#### POVEZANOST U MREZI ODREDNICA HAZARSKOG RECNIKA

### Uvodna napomena: 
# Ova R-skripta delimicno je zasnovana na:
# https://sna.stanford.edu/lab.php?l=2
# https://github.com/jeljov/SNALabs_at_UB (lab_3)
# https://www.markanthonyhoffman.com/

###### Struktura R-skripte:

### POTREBNE BIBLIOTEKE
### KREIRANJE MREZE I PODMREZA
### OSNOVNE METRIKE I POVEZANOST U MREZI:
  ### BROJ CVOROVA
  ### BROJ IVICA
  ### GUSTINA
  ### POVEZANOST
  ### STEPEN
  ### RECIPROCITET
    # Cenzus dijada
    # Reciprocitet
  ### TRANZITIVNOST
    ## Tranzitivnost i hijerarhizovanost mreze
    # Cenzus trijada
### DOSEZNOST
### GEODEZICI
### PROSEcNA (KARAKTERISTIcNA) PUTANJA
### DIJAMETAR
### EKSCENTRICNOST
### NAJUDALJENIJI CVOR
### DZINOVSKA KOMPONENTA - OTPORNOST NA CILJANE NAPADE
### KLIKE
### HOMOFILIJA / ASORTATIVNOST
### ZAJEDNICE U MREZI (WALKTRAP)

###### 
### POTREBNE BIBLIOTEKE
#####
library(igraph)
library(ggplot2)
library(tidyr)
library(lsa)
#####
### KREIRANJE MREZE I PODMREZA
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
# Za neke analize trebace nam neusmerena mreza: 
Odrednice_mreza_atributi_neusmerena<-as.undirected(Odrednice_mreza_atributi, mode="collapse", edge.attr.comb="sum") # Tezinu u neusmerenoj mrezi specifikujemo kao zbir frekvenci u oba smera; v. https://igraph.org/r/doc/igraph-attribute-combination.html

# Za neke analize trebace nam izdvojena dzinovska komponenta (najveca povezana komponenta u mrezi)
Odrednice_komponente <- components(Odrednice_mreza_atributi, mode = 'strong') # Izdvaja komponente u mrezi
count_components(Odrednice_mreza_atributi, mode='strong') # Broj komponenti u mrezi (7)
Odrednice_komponente$membership # Koja odrednica spada u koju komponentu
# Id-ovi komponenti koji ne pripadaju dzinovskoj komponenti (Svi koji ne pripadaju 8. komponenti, koja je najveca):
ne_dz_k <- names(Odrednice_komponente$membership[Odrednice_komponente$membership!=7])
ne_dz_k # Sest odrednica ne pripadaju dzinovskoj: 18, 25, 27, 32, 43, 45
# Kreirati mrezu bez cvorova koji su van dzinovske komponente:
Odrednice_mreza_atributi_dz_k <- delete.vertices(Odrednice_mreza_atributi, ne_dz_k)
Odrednice_mreza_atributi_dz_k

# Formiracemo i podmreze zasnovane na cvorovima u svakoj od knjiga (Crvenoj, Zelenoj, zutoj):
Podmreza_crvena<-induced.subgraph(Odrednice_mreza_atributi, 
                                  which(V(Odrednice_mreza_atributi)$Knjiga == 0)) # Crvena
Podmreza_crvena_neusmerena<-as.undirected(Podmreza_crvena, mode="collapse", edge.attr.comb="sum") 
Podmreza_zelena<-induced.subgraph(Odrednice_mreza_atributi, 
                                  which(V(Odrednice_mreza_atributi)$Knjiga == 1)) # Zelena
Podmreza_zelena_neusmerena<-as.undirected(Podmreza_zelena, mode="collapse", edge.attr.comb="sum") 
Podmreza_zxuta<-induced.subgraph(Odrednice_mreza_atributi, 
                                 which(V(Odrednice_mreza_atributi)$Knjiga == 2)) # Zuta
Podmreza_zxuta_neusmerena<-as.undirected(Podmreza_zxuta, mode="collapse", edge.attr.comb="sum")


#####
### OSNOVNE METRIKE I POVEZANOST U MREZI
#######

### BROJ CVOROVA

broj_cvorova<-vcount(Odrednice_mreza_atributi)
broj_cvorova_CK<-vcount(Podmreza_crvena)
broj_cvorova_ZeK<-vcount(Podmreza_zelena)
broj_cvorova_ZuK<-vcount(Podmreza_zxuta)

broj_cvorova.df<-data.frame(broj_cvorova, broj_cvorova_CK, broj_cvorova_ZeK, broj_cvorova_ZuK)
colnames(broj_cvorova.df)<-c('Cela_mreza', 'CK', "ZeK", "ZuK")
broj_cvorova.df

### BROJ IVICA

broj_ivica<-ecount(Odrednice_mreza_atributi)
broj_ivica_CK<-ecount(Podmreza_crvena)
broj_ivica_ZeK<-ecount(Podmreza_zelena)
broj_ivica_ZuK<-ecount(Podmreza_zxuta)

broj_ivica.df<-data.frame(broj_ivica, broj_ivica_CK, broj_ivica_ZeK, broj_ivica_ZuK)
colnames(broj_ivica.df)<-c('Cela_mreza', 'CK', "ZeK", "ZuK")
broj_ivica.df

### GUSTINA

gustina<-graph.density(Odrednice_mreza_atributi)
gustina_CK<-graph.density(Podmreza_crvena)
gustina_ZeK<-graph.density(Podmreza_zelena)
gustina_ZuK<-graph.density(Podmreza_zxuta)

gustina.df<-data.frame(round(gustina, 2), round(gustina_CK, 2), round(gustina_ZeK, 2), round(gustina_ZuK, 2))
colnames(gustina.df)<-c('Cela_mreza', 'CK', "ZeK", "ZuK")
gustina.df

### POVEZANOST

povezanost_usmerena<-vertex.connectivity(Odrednice_mreza_atributi)
povezanost_CK<-vertex.connectivity(Podmreza_crvena)
povezanost_ZeK<-vertex.connectivity(Podmreza_zelena)
povezanost_ZuK<-vertex.connectivity(Podmreza_zxuta)

povezanost.df<-data.frame(povezanost_usmerena, povezanost_CK, povezanost_ZeK, povezanost_ZuK)
colnames(povezanost.df)<-c('Povezanost_cela_mreza', 'Povezanost_CK', "Povezanost_ZeK", "Povezanost_ZuK")
povezanost.df

### PROSECNI STEPEN / VREDNOST MEDIJANE

# Proveravamo normalnost raspodele

shapiro.test(degree(Odrednice_mreza_atributi, mode='in')) # Raspodela nije normalna (p<0.001)
shapiro.test(degree(Podmreza_crvena, mode='in')) # Raspodela nije normalna (p<0.05)
shapiro.test(degree(Podmreza_zelena, mode='in')) # Raspodela nije normalna (p<0.05)
shapiro.test(degree(Podmreza_zxuta, mode='in')) # Raspodela je normalna (p=0.08)
# Posto raspodela nije normalna u vecini slucajeva, za sve slucajeve racunacemo medijanu i kvartile

summary(degree(Odrednice_mreza_atributi, mode='in'))
summary(degree(Podmreza_crvena, mode='in'))
summary(degree(Podmreza_zelena, mode='in'))
summary(degree(Podmreza_zxuta, mode='in'))

# Testiranje normalnosti raspodele
apply(degree(Odrednice_mreza_atributi, mode='in'), 2, shapiro.test)

shapiro.test(degree(Odrednice_mreza_atributi, mode='in'))

?shapiro.test()

### RECIPROCITET

# Cenzus dijada

# Oznake cenzusa dijada:
oznake_cenzusa_dijada<-c("Obostrano",
                         "Asimetricno",
                         "Nepovezano")

cenzus_dijada<-dyad_census(Odrednice_mreza_atributi)
cenzus_dijada_CK<-dyad_census(Podmreza_crvena)
cenzus_dijada_ZeK<-dyad_census(Podmreza_zelena)
cenzus_dijada_ZuK<-dyad_census(Podmreza_zxuta)
cenzus_dijada.df<-data.frame(oznake_cenzusa_dijada,
                             c(cenzus_dijada[[1]], cenzus_dijada[[2]], cenzus_dijada[[3]]),
                             c(cenzus_dijada_CK[[1]], cenzus_dijada_CK[[2]], cenzus_dijada_CK[[3]]),
                             c(cenzus_dijada_ZeK[[1]], cenzus_dijada_ZeK[[2]], cenzus_dijada_ZeK[[3]]),
                             c(cenzus_dijada_ZuK[[1]], cenzus_dijada_ZuK[[2]], cenzus_dijada_ZuK[[3]]))
colnames(cenzus_dijada.df)<-c('Oznake_cenzusa_dijada', "Cela_mreza", "CK", "ZeK", "ZuK")
cenzus_dijada.df

# Da bismo lakse poredili mreze sa razlicitom gustinom, izracunacemo i procente:
cenzus_dijada_procenti.df<-round((cenzus_dijada.df$Cela_mreza)/(sum(cenzus_dijada.df$Cela_mreza))*100, 2)
cenzus_dijada_CK_procenti.df<-round((cenzus_dijada.df$CK)/(sum(cenzus_dijada.df$CK))*100, 2)
cenzus_dijada_ZeK_procenti.df<-round((cenzus_dijada.df$ZeK)/(sum(cenzus_dijada.df$ZeK))*100, 2)
cenzus_dijada_ZuK_procenti.df<-round((cenzus_dijada.df$ZuK)/(sum(cenzus_dijada.df$ZuK))*100, 2)
cenzus_dijada_procenti.df<-data.frame(oznake_cenzusa_dijada,
                                      cenzus_dijada_procenti.df,
                                      cenzus_dijada_CK_procenti.df,
                                      cenzus_dijada_ZeK_procenti.df,
                                      cenzus_dijada_ZuK_procenti.df)
colnames(cenzus_dijada_procenti.df)<-c("Oznake_cenzusa_dijada", "Cela_mreza(%)", "CK(%)", "ZeK(%)", "ZuK(%)")
cenzus_dijada_procenti.df
cenzus_dijada_sve.df<-merge(cenzus_dijada.df, cenzus_dijada_procenti.df, by='Oznake_cenzusa_dijada')
cenzus_dijada_sve.df<-cenzus_dijada_sve.df[, c(1,2,6,3,7,4,8,5,9)] # Malo menjamo redosled kolona
cenzus_dijada_sve.df<-cenzus_dijada_sve.df[c(3, 1, 2), ] # ... onda i redova
cenzus_dijada_sve.df
write.csv(cenzus_dijada_sve.df, "Cenzus_dijada.csv")

# Reciprocitet

reciprocitet<-reciprocity(Odrednice_mreza_atributi) 
reciprocitet_CK<-reciprocity(Podmreza_crvena)
reciprocitet_ZeK<-reciprocity(Podmreza_zelena)
reciprocitet_ZuK<-reciprocity(Podmreza_zxuta)

reciprocitet.df<-data.frame(round(reciprocitet, 2), round(reciprocitet_CK, 2), round(reciprocitet_ZeK, 2), round(reciprocitet_ZuK, 2))
colnames(reciprocitet.df)<-c('Reciprocitet_cela_mreza', 'Reciprocitet_CK', "Reciprocitet_ZeK", "Reciprocitet_ZuK")
reciprocitet.df

### TRANZITIVNOST

tranzitivnost<-transitivity(Odrednice_mreza_atributi)
tranzitivnost_CK<-transitivity(Podmreza_crvena)
tranzitivnost_ZeK<-transitivity(Podmreza_zelena)
tranzitivnost_ZuK<-transitivity(Podmreza_zxuta)

tranzitivnost.df<-data.frame(round(tranzitivnost, 2), round(tranzitivnost_CK, 2), round(tranzitivnost_ZeK, 2), round(tranzitivnost_ZuK, 2))
colnames(tranzitivnost.df)<-c('Tranzitivnost_cela_mreza', 'Tranzitivnost_CK', "Tranzitivnost_ZeK", "Tranzitivnost_ZuK")
tranzitivnost.df

# Tranzitivnost se cesto izjednacuje sa koeficijentom grupisanja
# Koeficijent grupisanja nekad se racuna i kao prosek lokalnih tranzitivnosti (tranzitivnosti na nivou svakog cvora)
tranzitivnost_l_prosek<-mean(transitivity(Odrednice_mreza_atributi, type="local", isolates="zero"))
round(tranzitivnost_l_prosek, 2)

# Izracunacemo oba tipa tranzitivnosti i za nasumicno generisanu mrezu, kako bismo dobijenu vrednost uporedili sa mrezom odrednica
# To je jedan parametar za utvrdjivanje da li mreza ima osobine malog sveta
# Ovo cemo uraditi za celu mrezu
# Izracunacemo koeficijent grupisanja i kao tranzitivnost i kao prosek lokalne tranzitivnosti

# Ponovicemo postupak generisanja mreze 100 puta i izracunati prosek
for (i in 1:100) {
  tranzitivnost_nasumicna_mreza <- erdos.renyi.game(n = vcount(Odrednice_mreza_atributi), p.or.m = graph.density(Odrednice_mreza_atributi), directed = TRUE)
  trial.t[[i]] <- transitivity(tranzitivnost_nasumicna_mreza) # Rezultat pripisujemo i-om mestu. Za prvu iteraciju, rezultat ce biti uskladisten na 1. mesto u listi
  trial.l.t[[i]]<-mean(transitivity(tranzitivnost_nasumicna_mreza, type="local"))
}
tranzitivnost_nasumicna_mreza<-Reduce("+",trial.t)/100 # Funkcija Reduce() ce sabrati sve elemente vektora u listi
tranzitivnost_nasumicna_mreza.l<-Reduce("+",trial.l.t)/100
round(tranzitivnost_nasumicna_mreza, 2)
round(tranzitivnost_nasumicna_mreza.l, 2)

# U oba slucaja koeficijent grupisanja dosta je veci kod mreze odrednica nego u nasumicnoj mrezi:
# 0.62 : 0.36 za globalnu tranzitivnost i 0.5 : 0.3 za prosek lokalnih tranzitivnosti
# Ti podaci sugerisu da mreza odrednica ima osobine malog sveta

## Tranzitivnost i hijerarhizovanost mreze
# U hijerarhizovanim mrezama postoji medjuzavisnost izmedju koeficijenta grupisanja i stepena cvorova,
# tako da cvorovi sa vecim stepenom imaju manji koeficijent grupisanja (Ravasz, Barabasi, 2008: https://arxiv.org/pdf/cond-mat/0206130.pdf)

tranzitivnost_l.<-transitivity(Odrednice_mreza_atributi, type = "local"); tranzitivnost_l.
stepen<-degree(Odrednice_mreza_atributi); stepen # ukupan stepen, ulazni i izlazni, posto se i tranzitivnost racuna za neusmerenu mrezu
stepen_reciprocno<-1/degree(Odrednice_mreza_atributi); stepen_reciprocno
hijerarhija.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name), stepen, tranzitivnost_l., stepen_reciprocno)
hijerarhija.df[order(-hijerarhija.df$tranzitivnost_l.),]
plot(stepen, tranzitivnost_l.)
# Nekoliko cvorova sa najvecim stepenom zaista ima najmanji koeficijent grupisanja
# Takodje, nekoliko cvorova sa malim stepenom ima najveci koeficijent grupisanja
# Ali, u sredini je poprilicno "mesovito"
# Da bismo jasno videli odnos između lokalne tranzitivnosti i stepena, izracunacemo korelaciju
# izmedju reciprocnih vrednosti stepena i lokalne tranzitivnosti cvorova
# View(hijerarhija.df)
# Proveravamo da li podaci imaju normalnu raspodelu
apply(hijerarhija.df[,3:4], 2, shapiro.test) # Raspodela nije normalna, pa cemo koristiti Spirmanov test korelacije
hijerarhija_korelacija<- cor(hijerarhija.df$tranzitivnost_l., hijerarhija.df$stepen_reciprocno,
                             use='complete.obs',
                             method = 'spearman')
hijerarhija_korelacija
# Korelacija između lokalne tranzitivnosti i reciprocne vrednosti stepena je pozitivna (0,48), iako ne narocitno visoka
# Pozitivna korelacija sugerise da ispitivana mreza ima osobine hijerarhijskih mreza

# Cenzus trijada [Triad census]
# Tranzitivnost se racuna za neusmerenu mrezu, zato nam treba cenzus trijada
# V.: https://www.markanthonyhoffman.com/social_network_analysis/measuring-transitivity-and-structural-balance.html#calculating-a-triad-census
# http://ceur-ws.org/Vol-1710/paper38.pdf 
?triad.census
# Napravicemo vektor za oznakama mogucih cenzusa: (v. ?triad.census)
oznake_cenzusa = c('003',
                   '012',
                   '102',
                   '021D',
                   '021U',
                   '021C',
                   '111D',
                   '111U',
                   '030T',
                   '030C',
                   '201',
                   '120D',
                   '120U',
                   '120C',
                   '210',
                   '300')
cenzus_trijada <- triad.census(Odrednice_mreza_atributi)
# Da bismo interpretirali dobijene podatke, valja ih uporediti
# sa cenzusima trijada nasumicno generisane mreze sa istom gustinom
# v. npr.: https://www.markanthonyhoffman.com/social_network_analysis/measuring-transitivity-and-structural-balance.html#calculating-a-triad-census
# za nasumicno generisanje grafa: ?erdos.renyi.game
# Za generisanje nasumicne mreze ponovljeno 100 puta:
proba <- vector("list", 100) # Lista sa 100 mesta za skladistenje, u koju ce biti uskladisten svaki rezultat
for ( i in 1:100 ){ # Za ponavljanje 100 puta
  census_trijada_random_graf <- erdos.renyi.game(n = vcount(Odrednice_mreza_atributi), p.or.m = graph.density(Odrednice_mreza_atributi), directed = TRUE)
  # n -- broj cvorova; p.or.m -- verovatnoca crtanja ivice
  proba[[i]] <- triad.census(census_trijada_random_graf) # Rezultat pripisujemo i-om mestu. Za prvu iteraciju, rezultat ce biti uskladisten na 1. mesto u listi
}
cenzus_trijada_random_graf_prosek<-Reduce("+",proba)/100 # Funkcija Reduce() ce sabrati sve elemente vektora u listi. Deli se sa 100 da se dobije prosek

cenzus_trijada.df <- data.frame(oznake_cenzusa,               
                               cenzus_trijada,
                               cenzus_trijada_random_graf_prosek)
# View(cenzus_trijada.df)
# write.csv(cenzus_trijada.df, "Cenzus_trijada_mreza_i_nasumicna_mreza.csv")

# Radi poredjenja sa mrezama sa drugacijim brojem cvorova, pretvoricemo rezultate i u procente
cenzus_trijada_procenti.df<-round((cenzus_trijada.df$cenzus_trijada)/(sum(cenzus_trijada.df$cenzus_trijada))*100, 2)
cenzus_trijada_procenti.df

# Izracunajmo cenzuse trijada za 3 podmreze:

cenzus_trijada_Crvena<-triad_census(Podmreza_crvena)
cenzus_trijada_Zelena<-triad.census(Podmreza_zelena)
cenzus_trijada_Zxuta<-triad.census(Podmreza_zxuta)
cenzus_trijada_podmreze.df<-data.frame(oznake_cenzusa, cenzus_trijada_Crvena, cenzus_trijada_Zelena, cenzus_trijada_Zxuta)
cenzus_trijada_podmreze.df
cenzus_trijada_Crvena_procenti<-round((cenzus_trijada_podmreze.df$cenzus_trijada_Crvena)/(sum(cenzus_trijada_podmreze.df$cenzus_trijada_Crvena))*100, 2)
cenzus_trijada_Zelena_procenti<-round((cenzus_trijada_podmreze.df$cenzus_trijada_Zelena)/(sum(cenzus_trijada_podmreze.df$cenzus_trijada_Zelena))*100, 2)
cenzus_trijada_Zxuta_procenti<-round((cenzus_trijada_podmreze.df$cenzus_trijada_Zxuta)/(sum(cenzus_trijada_podmreze.df$cenzus_trijada_Zxuta))*100, 2)

# Poredjenje cele mreze i triju podmreza

cenzus_trijada_sve.df<-data.frame(cenzus_trijada.df$oznake_cenzusa,
                                  cenzus_trijada.df$cenzus_trijada, cenzus_trijada_procenti.df,
                                  cenzus_trijada_podmreze.df$cenzus_trijada_Crvena, cenzus_trijada_Crvena_procenti,
                                  cenzus_trijada_podmreze.df$cenzus_trijada_Zelena, cenzus_trijada_Zelena_procenti,
                                  cenzus_trijada_podmreze.df$cenzus_trijada_Zxuta, cenzus_trijada_Zxuta_procenti)

# View(cenzus_trijada_sve.df)
colnames(cenzus_trijada_sve.df)<-c("Oznake_cenzusa", "Cela_mreza", "Cela_mreza(%)", "CK", "CK(%)", "ZeK", "ZeK(%)", "ZuK", "ZuK(%)")
cenzus_trijada_sve.df
# write.csv(cenzus_trijada_sve.df, "Cenzus_trijada_mreze_i_podmreze.csv")


### DOSEZNOST [REACHABILITY]

# Za svaki individualni cvor, nekoliko primera:

subcomponent(Odrednice_mreza_atributi, 1, mode = 'in') # Odrednice iz kojih se moze doseci 1. odrednica
subcomponent(Odrednice_mreza_atributi, 45, mode= 'out') # Odrednice koje se mogu doseci iz 45. odrednice

# Matrica na nivou cele mreze:
# Funkcija preuzeta iz lab_3 [https://github.com/jeljov/SNALabs_at_UB]

reachability <- function(g, m) {
  reach_mat = matrix(nrow = vcount(g), ncol = vcount(g))
  for (i in 1:vcount(g)) {
    reach_mat[i,] = 0
    this_node_reach <- subcomponent(g, i, mode = m)  
    for (alter in this_node_reach) 
      reach_mat[i, alter] = 1
  }
  return(reach_mat)
}

# Za ulazne vrednosti:

doseznost_u_cela_mreza<-reachability(Odrednice_mreza_atributi, 'in')
colnames(doseznost_u_cela_mreza)<-c(1:45)
# View(doseznost_u_cela_mreza)
# write.csv(doseznost_u_cela_mreza, "Doseznost_ulazna_matrica.csv")
doseznost_u_Podmreza_crvena<-reachability(Podmreza_crvena, 'in')
colnames(doseznost_u_Podmreza_crvena)<-c(1:14)
# View(doseznost_u_Podmreza_crvena)
doseznost_u_Podmreza_zelena<-reachability(Podmreza_zelena, 'in')
colnames(doseznost_u_Podmreza_zelena)<-c(15:30); rownames(doseznost_u_Podmreza_zelena)<-c(15:30)
# View(doseznost_u_Podmreza_zelena)
doseznost_u_Podmreza_zxuta<-reachability(Podmreza_zxuta, 'in')
colnames(doseznost_u_Podmreza_zxuta)<-c(31:45); rownames(doseznost_u_Podmreza_zxuta)<-c(31:45)
# View(doseznost_u_Podmreza_zxuta)

# Da li su svi cvorovi dosezni u celoj mrezi sa svih ostalih cvorova?
all(doseznost_u_cela_mreza==1) # # NE!

# Za izlazne vrednosti:

doseznost_iz_cela_mreza<-reachability(Odrednice_mreza_atributi, 'out')
colnames(doseznost_iz_cela_mreza)<-c(1:45)
# View(doseznost_iz_cela_mreza)
# write.csv(doseznost_iz_cela_mreza, "Doseznost_izlazna_matrica.csv")
doseznost_iz_Podmreza_crvena<-reachability(Podmreza_crvena, 'out')
colnames(doseznost_iz_Podmreza_crvena)<-c(1:14)
# View(doseznost_iz_Podmreza_crvena)
doseznost_iz_Podmreza_zelena<-reachability(Podmreza_zelena, 'out')
colnames(doseznost_iz_Podmreza_zelena)<-c(15:30); rownames(doseznost_iz_Podmreza_zelena)<-c(15:30)
# View(doseznost_iz_Podmreza_zelena)
doseznost_iz_Podmreza_zxuta<-reachability(Podmreza_zxuta, 'out')
colnames(doseznost_iz_Podmreza_zxuta)<-c(31:45); rownames(doseznost_iz_Podmreza_zxuta)<-c(31:45)
# View(doseznost_iz_Podmreza_zxuta)

# Mreze koje mi razmatramo dovoljno su male da nam uvid u matrice omogucava
# solidan pregled doseznosti
# Ipak, moze se izracunati broj doseznih cvorova za svaki drugi cvor
# Svuda racunamo za 1 manje jer zbog doseznosti cvora iz samog sebe

# Ulazne vrednosti:
br_dos_cela_u<-(apply(doseznost_u_cela_mreza, 1, function(x) sum(x==1)))-1 # cela mreza
br_dos_C_u<-(apply(doseznost_u_Podmreza_crvena, 1, function(x) sum(x==1)))-1 # podmreza Crvena knjiga
br_dos_Z_u<-(apply(doseznost_u_Podmreza_zelena, 1, function(x) sum(x==1)))-1 # podmreza Zelena knjiga
br_dos_Zx_u<-(apply(doseznost_u_Podmreza_zxuta, 1, function(x) sum(x==1)))-1 # podmreza Zuta knjiga

# Izlazne vrednosti
br_dos_cela_iz<-(apply(doseznost_iz_cela_mreza, 1, function(x) sum(x==1)))-1 # cela mreza
br_dos_C_iz<-(apply(doseznost_iz_Podmreza_crvena, 1, function(x) sum(x==1)))-1 # podmreza Crvena knjiga
br_dos_Z_iz<-(apply(doseznost_iz_Podmreza_zelena, 1, function(x) sum(x==1)))-1 # podmreza Zelena knjiga
br_dos_Zx_iz<-(apply(doseznost_iz_Podmreza_zxuta, 1, function(x) sum(x==1)))-1 # podmreza Zuta knjiga

doseznost_cela.df<- data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
                               ime=V(Odrednice_mreza_atributi)$Ime, 
                               br_dos_cela_u, 
                               br_dos_cela_iz)
doseznost_cela.df

doseznost_CK.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name[1:14]),
                           ime=V(Odrednice_mreza_atributi)$Ime[1:14], 
                           br_dos_C_u, 
                           br_dos_C_iz,
                           stringsAsFactors = FALSE)
doseznost_CK.df

doseznost_ZeK.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name[15:30]),
                           ime=V(Odrednice_mreza_atributi)$Ime[15:30], 
                           br_dos_Z_u, 
                           br_dos_Z_iz,
                           stringsAsFactors = FALSE)
doseznost_ZeK.df

doseznost_ZuK.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name[31:45]),
                            ime=V(Odrednice_mreza_atributi)$Ime[31:45], 
                            br_dos_Zx_u, 
                            br_dos_Zx_iz,
                            stringsAsFactors = FALSE)
doseznost_ZuK.df

# Sve:
doseznost_sve.df<-Reduce(function(x, y) merge(x, y, all=TRUE), list(doseznost_cela.df, doseznost_CK.df, doseznost_ZeK.df, doseznost_ZuK.df))
doseznost_sve.df<-doseznost_sve.df[order(doseznost_sve.df$id),]; doseznost_sve.df[is.na(doseznost_sve.df)]<-""
colnames(doseznost_sve.df)<-c('id', 'Odrednica', 'Cela_mreza_u', 'Cela_mreza_iz', "CK_u", "CK_iz", "ZeK_u", "ZeK_iz", "ZuK_u", "ZuK_iz")
doseznost_sve.df

# Za psrosecne vrednosti doseznosti na nivou mreze i podmreza i/ili vrednosti medijane i kvartila

# Proveravamo najpre normalnost raspodele
shapiro.test(doseznost_sve.df$Cela_mreza_u) # Za celu mrezu raspodela nije normalna (p<0.001)
shapiro.test(as.numeric(doseznost_sve.df$CK_u)) # Za Crvenu knjigu raspodela nije normalna (p<0.001)
shapiro.test(as.numeric(doseznost_sve.df$ZeK_u)) # Za Zelenu knjigu raspodela nije normalna (p<0.001)
shapiro.test(as.numeric(doseznost_sve.df$ZuK_u)) # Za Zutu knjigu raspodela nije normalna (p<0.001)

# Stoga ce nam relevantniji biti podaci o medijani i kvartilima:
summary(doseznost_sve.df$Cela_mreza_u)
summary(na.omit(as.numeric(doseznost_sve.df$CK_u)))
summary(na.omit(as.numeric(doseznost_sve.df$ZeK_u)))
summary(na.omit(as.numeric(doseznost_sve.df$ZuK_u)))

# doseznost_prosek<-round(mean(doseznost_sve.df$Cela_mreza_u), 2)
# doseznost_prosek
# doseznost_CK_prosek<-round(mean(na.omit(as.numeric(doseznost_sve.df$CK_u))), 2) 
# doseznost_CK_prosek
# doseznost_ZeK_prosek<-round(mean(na.omit(as.numeric(doseznost_sve.df$ZeK_u))), 2) 
# doseznost_ZeK_prosek
# doseznost_ZuK_prosek<-round(mean(na.omit(as.numeric(doseznost_sve.df$ZuK_u))), 2) 
# doseznost_ZuK_prosek

# write.csv(doseznost_sve.df, 'Doseznost.csv')

### GEODEZICI

geodezici_u<-distances(Odrednice_mreza_atributi, mode='in')
# View(geodezici_u)
# write.csv(geodezici_u, "Geodezci_ulazna_matrica.csv")
geodezici_iz<-distances(Odrednice_mreza_atributi, mode='out')
# View(geodezici_iz)
# write.csv(geodezici_iz, "Geodezci_izlazna_matrica.csv")

# Uklanjamo Inf (beskonacne, tj. nepostojece putanje); zamenimo ih NA vrednostima:

geodezici_u[geodezici_u==Inf] <- NA
geodezici_iz[geodezici_iz==Inf] <- NA

# Sad mozemo da izracunamo prosecni geodezik za svaki cvor u mrezi:
geodezici_u_prosek = vector()
geodezici_iz_prosek = vector()

for(i in 1:vcount(Odrednice_mreza_atributi)) {
  geodezici_u_prosek[i] <- round(mean(geodezici_u[i,], na.rm = TRUE), 2)
  geodezici_iz_prosek[i] <- round(mean(geodezici_iz[i,], na.rm = TRUE), 2)
}
summary(geodezici_u_prosek)
summary(geodezici_iz_prosek)
geodezici_u_prosek
geodezici_iz_prosek
typeof(geodezici_u_prosek)
# View(geodezici_u_prosek)


# Za uporedni prikaz ulaznih i izlaznih vrednoszi:
geodezici_prosek.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name), # id
                                ime=V(Odrednice_mreza_atributi)$Ime, 
                                geodezici_u_prosek,
                                geodezici_iz_prosek)

# Malo sortiranja:
geodezici_prosek.df[order(-geodezici_prosek.df$geodezici_u_prosek),] 

# View(geodezici_prosek.df)
geodezici_prosek.df_long <- gather(data = geodezici_prosek.df, 
                                     key = "Tip", value = "Geodezik_prosek",
                                     geodezici_u_prosek:geodezici_iz_prosek, 
                                     factor_key = TRUE)
head(geodezici_prosek.df_long)
ggplot(data = geodezici_prosek.df_long,
       mapping = aes(x = id, y = Geodezik_prosek, fill=Tip)) +
  geom_col(position = 'dodge') +
  scale_fill_discrete(name='Vrsta geodezika',
                      breaks=c('geodezici_u_prosek', 'geodezici_iz_prosek'),
                      labels=c('Geodezik_u', 'Geodezik_iz')) +
  labs(x = 'id odrednice', y = "Prosecni geodezik") +
  scale_x_continuous(breaks = seq(1,45,1)) +
  theme_bw() +
  theme(legend.position = 'bottom') 

# Slicno racunamo vrednosti i za podmreze:

geodezici_CK_u<-distances(Podmreza_crvena, mode='in')
geodezici_CK_iz<-distances(Podmreza_crvena, mode='out')
geodezici_ZeK_u<-distances(Podmreza_zelena, mode='in')
geodezici_ZeK_iz<-distances(Podmreza_zelena, mode='out')
geodezici_ZuK_u<-distances(Podmreza_zxuta, mode='in')
geodezici_ZuK_iz<-distances(Podmreza_zxuta, mode='out')

geodezici_CK_u[geodezici_CK_u==Inf] <- NA
geodezici_CK_iz[geodezici_CK_iz==Inf] <- NA
geodezici_ZeK_u[geodezici_ZeK_u==Inf] <- NA
geodezici_ZeK_iz[geodezici_ZeK_iz==Inf] <- NA
geodezici_ZuK_u[geodezici_ZuK_u==Inf] <- NA
geodezici_ZuK_iz[geodezici_ZuK_iz==Inf] <- NA

geodezici_CK_u_prosek = vector()
geodezici_CK_iz_prosek = vector()
geodezici_ZeK_u_prosek = vector()
geodezici_ZeK_iz_prosek = vector()
geodezici_ZuK_u_prosek = vector()
geodezici_ZuK_iz_prosek = vector()

for(i in 1:vcount(Podmreza_crvena)) {
  geodezici_CK_u_prosek[i] <- round(mean(geodezici_CK_u[i,], na.rm = TRUE), 2)
  geodezici_CK_iz_prosek[i] <- round(mean(geodezici_CK_iz[i,], na.rm = TRUE), 2)
}

for(i in 1:vcount(Podmreza_zelena)) {
  geodezici_ZeK_u_prosek[i] <- round(mean(geodezici_ZeK_u[i,], na.rm = TRUE), 2)
  geodezici_ZeK_iz_prosek[i] <- round(mean(geodezici_ZeK_iz[i,], na.rm = TRUE), 2)
}

for(i in 1:vcount(Podmreza_zxuta)) {
  geodezici_ZuK_u_prosek[i] <- round(mean(geodezici_ZuK_u[i,], na.rm = TRUE), 2)
  geodezici_ZuK_iz_prosek[i] <- round(mean(geodezici_ZuK_iz[i,], na.rm = TRUE), 2)
}


geodezici_CK_prosek.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name)[1:14], # id
                                ime=V(Odrednice_mreza_atributi)$Ime[1:14], 
                                geodezici_CK_u_prosek,
                                geodezici_CK_iz_prosek)
geodezici_CK_prosek.df

geodezici_ZeK_prosek.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name)[15:30], # id
                                   ime=V(Odrednice_mreza_atributi)$Ime[15:30], 
                                   geodezici_ZeK_u_prosek,
                                   geodezici_ZeK_iz_prosek)
geodezici_ZeK_prosek.df

geodezici_ZuK_prosek.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name)[31:45], # id
                                    ime=V(Odrednice_mreza_atributi)$Ime[31:45], 
                                    geodezici_ZuK_u_prosek,
                                    geodezici_ZuK_iz_prosek)
geodezici_ZuK_prosek.df

# Sve:
geodezici_prosek_sve.df<-Reduce(function(x, y) merge(x, y, all=TRUE), list(geodezici_prosek.df, geodezici_CK_prosek.df, geodezici_ZeK_prosek.df, geodezici_ZuK_prosek.df))
geodezici_prosek_sve.df<-geodezici_prosek_sve.df[order(geodezici_prosek_sve.df$id),]; geodezici_prosek_sve.df[is.na(geodezici_prosek_sve.df)]<-""
colnames(geodezici_prosek_sve.df)<-c('id', 'Odrednica', 'Cela_mreza_u', 'Cela_mreza_iz', "CK_u", "CK_iz", "ZeK_u", "ZeK_iz", "ZuK_u", "ZuK_iz")
geodezici_prosek_sve.df

# Za soriranje objedinjenog df-a:
# View(geodezici_prosek_sve.df)
geodezici_prosek_sve.df[order(-geodezici_prosek_sve.df$Cela_mreza_u),] # primer za celu mrezu po ulaznim geodezicima, opadajuce

# write.csv(geodezici_prosek_sve.df, 'Geodezici.csv')

### PROSEcNA (KARAKTERISTIcNA) PUTANJA

prosecna_putanja<-average.path.length(Odrednice_mreza_atributi)
prosecna_putanja_CK<-average.path.length(Podmreza_crvena)
prosecna_putanja_ZeK<-average.path.length(Podmreza_zelena)
prosecna_putanja_ZuK<-average.path.length(Podmreza_zxuta)

prosecna_putanja.df<-data.frame(round(prosecna_putanja, 2), round(prosecna_putanja_CK, 2), round(prosecna_putanja_ZeK, 2), round(prosecna_putanja_ZuK, 2))
colnames(prosecna_putanja.df)<-c('Prosecna_putanja_cela_mreza', 'Prosecna_putanja_CK', "Prosecna_putanja_ZeK", "Prosecna_putanja_ZuK")
prosecna_putanja.df

## Prosecna (karakteristicna) putanja nasumicne mreze
# Radi utvrdjivanja da li je ispitivana mreza mali svet, uporedicemo karakteristicnu putanju mreze odrednica
# sa nasumicnom karakteristicnom putanjom sa istim brojem cvorova i istom gustinom
# Ovde nas pre svega zanima stanje u citavoj mrezi

trial.k.p <- vector("list", 100) # Lista sa 100 mesta za skladistenje, u koju ce biti uskladisten svaki rezultat
for ( i in 1:100 ){ # Ponavljamo 100 puta
  karakteristicna_putanja_nasumicna_mreza <- erdos.renyi.game(n = vcount(Odrednice_mreza_atributi), p.or.m = graph.density(Odrednice_mreza_atributi), directed = TRUE)
  trial.k.p[[i]] <- average.path.length(karakteristicna_putanja_nasumicna_mreza) # Rezultat pripisujemo i-om mestu. Za prvu iteraciju, rezultat ce biti uskladisten na 1. mesto u listi
}
karakteristicna_putanja_nasumicna_mreza<-Reduce("+",trial.k.p)/100 # Funkcija Reduce() ce sabrati sve elemente vektora u listi, koje delimo sa 100
round(karakteristicna_putanja_nasumicna_mreza, 2)
# Dobijamo rezultat 1,93, sto nije velika razlika u odnosu na mrezu odrednica (2,11)
# Stoga, po ovom paramteru, mreza odrednica bila bi tipa malog sveta

### DIJAMETAR

dijametar<-diameter(Odrednice_mreza_atributi, weights = NA)
dijametar_cK<-diameter(Podmreza_crvena, weights = NA)
dijametar_ZeK<-diameter(Podmreza_zelena, weights = NA)
dijametar_ZuK<-diameter(Podmreza_zxuta, weights = NA)

dijametar.df<-data.frame(round(dijametar, 2), round(dijametar_cK, 2), round(dijametar_ZeK, 2), round(dijametar_ZuK, 2))
colnames(dijametar.df)<-c('Dijametar_cela_mreza', 'Dijametar_CK', "Dijametar_ZeK", "Dijametar_ZuK")
dijametar.df

### EKSCENTRICNOST

# Najkraca putanja do najudaljenijeg cvora u mrezi

# Za celu mrezu:
ekscentricnost_u<-eccentricity(Odrednice_mreza_atributi, mode='in')
ekscentricnost_iz<-eccentricity(Odrednice_mreza_atributi, mode='out')
ekscentricnost.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
                              ime=V(Odrednice_mreza_atributi)$Ime, 
                              ekscentricnost_u, 
                              ekscentricnost_iz)
ekscentricnost.df

# Vizualizacija ulazne i izlazne ekscentricnosti za celu mrezu

ekscentricnost.df_long <- gather(data = ekscentricnost.df, 
                                 key = "Tip", value = "Ekscentricnost",
                                 ekscentricnost_u:ekscentricnost_iz, 
                                 factor_key = TRUE)
head(ekscentricnost.df_long)
ggplot(data = ekscentricnost.df_long,
       mapping = aes(x = id, y = Ekscentricnost, fill=Tip)) +
  geom_col(position = 'dodge') +
  scale_fill_discrete(name='Vrsta ekscentricnosti',
                      breaks=c('ekscentricnost_u', 'ekscentricnost_iz'),
                      labels=c('Ekscentricnost_u', 'Ekscentricnost_iz')) +
  labs(x = 'id odrednice', y = "Ekscentricnost") +
  scale_x_continuous(breaks = seq(1,45,1)) +
  theme_bw() +
  theme(legend.position = 'bottom') 

# CK
ekcentricnost_CK_u<-eccentricity(Podmreza_crvena, mode='in')
ekcentricnost_CK_iz<-eccentricity(Podmreza_crvena, mode='out')
ekscentricnost_CK.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name[1:14]),
                                 ime=V(Odrednice_mreza_atributi)$Ime[1:14], 
                                 ekcentricnost_CK_u, 
                                 ekcentricnost_CK_iz)
# ZeK
ekcentricnost_ZeK_u<-eccentricity(Podmreza_zelena, mode='in')
ekcentricnost_ZeK_iz<-eccentricity(Podmreza_zelena, mode='out')
ekscentricnost_ZeK.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name[15:30]),
                                 ime=V(Odrednice_mreza_atributi)$Ime[15:30], 
                                 ekcentricnost_ZeK_u, 
                                 ekcentricnost_ZeK_iz)
# ZuK
ekcentricnost_ZuK_u<-eccentricity(Podmreza_zxuta, mode='in')
ekcentricnost_ZuK_iz<-eccentricity(Podmreza_zxuta, mode='out')
ekscentricnost_ZuK.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name[31:45]),
                                  ime=V(Odrednice_mreza_atributi)$Ime[31:45], 
                                  ekcentricnost_ZuK_u, 
                                  ekcentricnost_ZuK_iz)

# Sve:
ekscentricnost_sve.df<-Reduce(function(x, y) merge(x, y, all=TRUE), list(ekscentricnost.df, ekscentricnost_CK.df, ekscentricnost_ZeK.df, ekscentricnost_ZuK.df))
ekscentricnost_sve.df<-ekscentricnost_sve.df[order(ekscentricnost_sve.df$id),]
colnames(ekscentricnost_sve.df)<-c('id', 'Odrednica', 'Cela_mreza_u', 'Cela_mreza_iz', "CK_u", "CK_iz", "ZeK_u", "ZeK_iz", "ZuK_u", "ZuK_iz")
ekscentricnost_sve.df
# write.csv(ekscentricnost_sve.df, "Ekscentricnost.csv")

# Malo vizualizacije:
barplot(sort(ekscentricnost_u), main="Ekscenticnost po ulaznom stepenu", xlab="Odrednica", las=2)
barplot(sort(ekscentricnost_iz), main="Ekscenticnost po izlaznom stepenu", xlab="Odrednica", las=2)

# Prosek i/ili medijana na nivou mreze
# Ispitivanje normalnosti raspodele
shapiro.test(ekscentricnost_sve.df$Cela_mreza_u) # za celu mrezu; raspodela nije normalna (p<0.001)
shapiro.test(ekscentricnost_sve.df$CK_u) # za Crvenu knjigu raspodela je normalna (p=0.051)
shapiro.test(ekscentricnost_sve.df$ZeK_u) # za Zelenu knjigu raspodela nije normalna (p<0.01)
shapiro.test(ekscentricnost_sve.df$ZuK_u) # za Zutu knjigu raspodela nije normalna (p<0.05)
# Posto u vecini slucajeva nema normalne raspodele, racunamo medijanu i kvartile

summary(ekscentricnost_sve.df$Cela_mreza_u)
summary(na.omit(ekscentricnost_sve.df$CK_u))
summary(na.omit(ekscentricnost_sve.df$ZeK_u))
summary(na.omit(ekscentricnost_sve.df$ZuK_u))

# Prosek na nivou mreze
# ekscentricnost_prosek<-round(mean(ekscentricnost_sve.df$Cela_mreza_u), 2); ekscentricnost_prosek
# ekscentricnost_CK_prosek<-round(mean(na.omit(ekscentricnost_sve.df$CK_u)), 2); ekscentricnost_CK_prosek
# ekscentricnost_ZeK_prosek<-round(mean(na.omit(ekscentricnost_sve.df$ZeK_u)), 2); ekscentricnost_ZeK_prosek
# ekscentricnost_ZuK_prosek<-round(mean(na.omit(ekscentricnost_sve.df$ZuK_u)), 2); ekscentricnost_ZuK_prosek

# Analiza na nivou pojedinacnog cvora:
eccentricity(Odrednice_mreza_atributi, vids = V(Odrednice_mreza_atributi)$name=='16', mode='in') # Za konkretan cvor

### NAJUDALJENIJI CVOR
farthest_vertices(Odrednice_mreza_atributi)$vertices
najudaljeniji_cvorovi<-farthest_vertices(Odrednice_mreza_atributi)$vertices
putanja_najudaljeniji_cvorovi<-shortest_paths(Odrednice_mreza_atributi, 
                                              from = najudaljeniji_cvorovi[1], 
                                              to = najudaljeniji_cvorovi[2], 
                                              output = 'epath')
putanja_najudaljeniji_cvorovi

# Za pronalazenje najkracih putanja medju cvorovima uopste, prema id-u
shortest_paths(Odrednice_mreza_atributi, 
               from = 1, 
               to = 2, 
               output = 'epath')

### DZINOVSKA KOMPONENTA - OTPORNOST NA CILJANE NAPADE

# Cela mreza jeste medjusobno povezana, kad se posmatra modul "weak"
# Drugim recima, povezana svaka odrednica povezana sa jednom drugom bar u jednom smeru
is.connected(Odrednice_mreza_atributi, mode = 'weak')

# Proveravacemo da li mreza ostaje povezana kako se uklanjaju vrednosti sa najvisom centralnoscu intermedijarnosti (CI),
# jer na osnovu toga prepoznajemo potencijalne brokere u mrezi
# Najpre nad neusmerenom mrezom, sa svim odrednicama
# Izlistavamo sortirane vrednosti CI:
# source('Odrednice_centralnost_novije.R')
# Uklonicemo najpre cvor-odrednicu sa najvecom vrednocu, sto je O23 (Masudi)
Mreza_bez_23<-delete.vertices(Odrednice_mreza_atributi, V(Odrednice_mreza_atributi)$name=="23") # Uklanjamo O23
is.connected(Mreza_bez_23) # Mreza je i dalje povezana
Mreza_bez_23_12<-delete.vertices(Mreza_bez_23, V(Mreza_bez_23)$name=="12") # Uklanjamo i 12
is.connected(Mreza_bez_23_12) # Povezana
Mreza_bez_23_12_29<-delete.vertices(Mreza_bez_23_12, V(Mreza_bez_23_12)$name=="29") # Uklanjamo i 29
is.connected(Mreza_bez_23_12_29) # Povezana
Mreza_bez_23_12_29_41<-delete.vertices(Mreza_bez_23_12_29, V(Mreza_bez_23_12_29)$name=="41") # Uklanjamo i 41
is.connected(Mreza_bez_23_12_29_41) # Vise nije povezana
components(Mreza_bez_23_12_29_41) # Medjutim, mreza je razbijena na 2 komponente, 
# od kojih samo jedna odrednica (O18) cini jednu komponentu, a sve ostale drugu
# Provericemo stoga sta se desava kad izbacimo jos dva cvora sa visokom vrednoscu CI (O2, O35)
Mreza_bez_23_12_29_41_2<-delete.vertices(Mreza_bez_23_12_29_41, V(Mreza_bez_23_12_29_41)$name=="2") # Uklanjamo i 2
components(Mreza_bez_23_12_29_41_2) # 4 komponente, najveca sa 37/40 odrednica
Mreza_bez_23_12_29_41_2_35<-delete.vertices(Mreza_bez_23_12_29_41_2, V(Mreza_bez_23_12_29_41_2)$name=="35") # Uklanjamo i 35
components(Mreza_bez_23_12_29_41_2_35) # 6 komponenti, medjutim, najveca je i dalje prilicno velika: 32/39 preostalih odrednica
# Rekli bismo da po ovim svojstvima mreza odrednica ima osobine fikcionalne mreze

# Pogledacemo sta se desava ako krenemo od dzinovske komponente usmerene mreze:
summary(Odrednice_mreza_atributi_dz_k) # Dzinovska Komponenta sa 38/45 odrednica
# Izracunacemo CI za dzinovsku komponentu usmerene mreze:
CI_dz_k<-betweenness(Odrednice_mreza_atributi_dz_k, directed=TRUE)
CI_dz_k.df<-data.frame(d=as.integer(V(Odrednice_mreza_atributi_dz_k)$name),
                       ime=V(Odrednice_mreza_atributi_dz_k)$Ime,
                       CI_dz_k)
CI_dz_k.df[order(-CI_dz_k.df$CI_dz_k),] # opadajuce
Mreza_dz_k_bez_23<-delete.vertices(Odrednice_mreza_atributi_dz_k, V(Odrednice_mreza_atributi_dz_k)$name=="23") # Uklanjamo najpre 23 (najveca vrednost SI)
is.connected(Mreza_dz_k_bez_23) # Mreza ostaje povezana
Mreza_dz_k_bez_23_2<-delete.vertices(Mreza_dz_k_bez_23, V(Mreza_dz_k_bez_23)$name=="") # Uklanjamo i cvor 2
is.connected(Mreza_dz_k_bez_23_2) # I dalje povezana
Mreza_dz_k_bez_23_29_35<-delete.vertices(Mreza_dz_k_bez_23_29, V(Mreza_dz_k_bez_23_29)$name=="35") # uklanjamo cvor sa 3. vrednoscu CI
is.connected(Mreza_dz_k_bez_23_29_35) # I dalje je povezana
# Navedene cinjenice opet, cini se, favorizuju vidjenje mreze odrednica kao blize fikcionalnim mrezama

### KLIKE

max_cliques(Odrednice_mreza_atributi_neusmerena, min = NULL, max = NULL, subset = NULL,
            file = NULL) # Pronalazi sve klike u neusmerenoj mrezi; najveca je sa 13 odrednica
max_cliques(Odrednice_mreza_atributi_neusmerena, min = 13, max = NULL, subset = NULL,
            file = NULL) # Sve klike sa 13 odrednica

### HOMOFILIJA / ASORTATIVNOST

# Asortativnost stepena / atributa

asortativnost<-assortativity.degree(Odrednice_mreza_atributi)
asortativnost_CK<-assortativity.degree(Podmreza_crvena)
asortativnost_ZeK<-assortativity.degree(Podmreza_zelena)
asortativnost_ZuK<-assortativity.degree(Podmreza_zxuta)

asortativnost.df<-data.frame(round(asortativnost, 2), round(asortativnost_CK, 2), round(asortativnost_ZeK, 2), round(asortativnost_ZuK, 2))
colnames(asortativnost.df)<-c('Asortativnost_cela_mreza', 'Asortativnost_CK', "Asortativnost_ZeK", "Asortativnost_ZuK")
asortativnost.df

# Funkcija za asortativnost prema atributima
# prema knjizi, vremenskom sloju i stepenu
compute_assortitivity <- function(g) {
  result = vector(mode = 'numeric', length = 3)
  result[1] <- assortativity(g, types1 = V(g)$Knjiga)
  result[2] <- assortativity(g, types1 = V(g)$Sloj)
  result[3] <- assortativity_degree(g)
  names(result) <- c('Knjiga', 'Sloj', 'Stepen')
  data.frame(result)
}

round(compute_assortitivity(Odrednice_mreza_atributi), 2)

# Za podmreze, koje su zasnovane na pripadnosti razlicitim knjigama,
# moze se gledati asortativnost prema vremenskom sloju i stepenu

compute_assortitivity(Podmreza_crvena)
compute_assortitivity(Podmreza_zelena)
compute_assortitivity(Podmreza_zxuta)

### ZAJEDNICE U MREZI (WALKTRAP)

?cluster_walktrap

# Za neke random procese:
seed <- 1912

# Lista za skladistenje vrednosti modularnosti razlicitih algoritama:
modularnost_vrednost <- list()

set.seed(seed)
Odrednice_mreza_atributi_neusmerena.wt<-cluster_walktrap(Odrednice_mreza_atributi_neusmerena, steps=5) # sa 5 koraka
Odrednice_mreza_atributi_neusmerena.wt
Odrednice_mreza_atributi_neusmerena.wt[1:length(Odrednice_mreza_atributi_neusmerena.wt)]
plot(Odrednice_mreza_atributi_neusmerena.wt, Odrednice_mreza_atributi_neusmerena) # Vizualizacija
modularnost_vrednost$WT_5koraka <- modularity(Odrednice_mreza_atributi_neusmerena.wt) # Upisujemo vrednost modularnosti

# Broj koraka 5 jer je taj broj davao najbolje rezultate u razlicitim mrezama
# Mozemo proveriti razlicite brojeve koraka i izabrati onaj sa najvecom modularnoscu:
wt_modularnost <- vector(mode = 'numeric', length = 20)
for (s in 1:20) {
  set.seed(seed)
  wt_rezultat <- cluster_walktrap(Odrednice_mreza_atributi_neusmerena, steps = s)
  wt_modularnost[s] <- modularity(wt_rezultat)
}
max_wt_modularnost <- max(wt_modularnost)
which(wt_modularnost == max_wt_modularnost)
# Kao najbolji se pokazuje br. koraka 3

# Ponovicemo sa tim brojem koraka:
Odrednice_mreza_atributi_neusmerena.wt.3k <- cluster_walktrap(Odrednice_mreza_atributi_neusmerena, steps=3)
Odrednice_mreza_atributi_neusmerena.wt.3k
Odrednice_mreza_atributi_neusmerena.wt.3k[1:length(Odrednice_mreza_atributi_neusmerena.wt.3k)]
# plot(Odrednice_mreza_atributi_neusmerena.wt.3k, Odrednice_mreza_atributi_neusmerena)
modularnost_vrednost$WT_3koraka <- modularity(Odrednice_mreza_atributi_neusmerena.wt.3k) # Upisujemo vrednost modularnosti sa 3 koraka

### POSTUPAK ZA IVICE SA TEZINOM:
# Da pronadjemo odgovarajuci broj koraka:
maks.br.koraka <- diameter(Odrednice_mreza_atributi_neusmerena, directed = FALSE, # Dijametar za najveci broj koraka
                           unconnected = FALSE)
maks.br.koraka # 3 koraka
wt_tezina_modularnost <- vector(mode = 'numeric', length = 3)
for (s in 1:maks.br.koraka) {
  set.seed(seed)
  wt_tezina_rezultat <- cluster_walktrap(Odrednice_mreza_atributi_neusmerena, steps = s, 
                                         weights = E(Odrednice_mreza_atributi_neusmerena)$Frekvenca)
  wt_tezina_modularnost[s] <- modularity(wt_tezina_rezultat)
}
which(wt_tezina_modularnost == max(wt_tezina_modularnost)) # Dobijamo da je najbolje 3

set.seed(seed)
Odrednice_mreza_atributi_neusmerena_t.wt.3k <- cluster_walktrap(Odrednice_mreza_atributi_neusmerena, steps=3,
                                                                weights = E(Odrednice_mreza_atributi_neusmerena)$Frekvenca)
Odrednice_mreza_atributi_neusmerena_t.wt.3k
Odrednice_mreza_atributi_neusmerena_t.wt.3k[1:length(Odrednice_mreza_atributi_neusmerena_t.wt.3k)]

# Dodajemo vrednost modularnosti sa tezinama u listu:
modularnost_vrednost$WT_tezine <- modularity(Odrednice_mreza_atributi_neusmerena_t.wt.3k)
modularnost_vrednost # Uporedjujemo vrednosti modularnosti
# Najveca modularnost je pomocu WT sa tezinama

# Modularnost za druge algoritme

# Edge betweenness
modularity(Odrednice_mreza_atributi_neusmerena.eb<-cluster_edge_betweenness(Odrednice_mreza_atributi_neusmerena, directed = FALSE))
# The Louvaine method
modularity(Odrednice_mreza_atributi_neusmerena.lm<-cluster_louvain(Odrednice_mreza_atributi_neusmerena))
# Hijerarhijsko aglomerativno grupisanje

# Za aglumerativno hijerarhijsko  grupisanje [AGGLOMERATIVE HIERARCHICAL CLUSTERING (UNDIRECTED, UNWEIGHTED GRAPH)]
# postupak je, uz objašnjenja, u potpunosti preuzet iz sledeceg izvora:
# https://github.com/jeljov/SNALabs_at_UB, lab_4, gde se moze naci objasnjenje postupka

Odrednice_mreza_atributi_neusmerena.am = as_adjacency_matrix(Odrednice_mreza_atributi_neusmerena, sparse=FALSE)
Odrednice_mreza_atributi_neusmerena.am

Odrednice_mreza_atributi_neusmerena.sim.mat = cosine(Odrednice_mreza_atributi_neusmerena.am)
Odrednice_mreza_atributi_neusmerena.sim.mat

Odrednice_mreza_atributi_neusmerena.dist.mat = 1-Odrednice_mreza_atributi_neusmerena.sim.mat

Odrednice_mreza_atributi_neusmerena.hc = hclust(as.dist(Odrednice_mreza_atributi_neusmerena.dist.mat), method = "average")

# plot(Odrednice_mreza_atributi_neusmerena.hc)

# 2 klastera:
modularity(Odrednice_mreza_atributi_neusmerena, cutree(Odrednice_mreza_atributi_neusmerena.hc, k = 2))
# 3 klastera:
modularity(Odrednice_mreza_atributi_neusmerena, cutree(Odrednice_mreza_atributi_neusmerena.hc, k = 3))

rect.hclust(Odrednice_mreza_atributi_neusmerena.hc, k = 3, border="blue")

Odrednice_mreza_atributi_neusmerena.net.clust <- cutree(Odrednice_mreza_atributi_neusmerena.hc, k = 3)

# Vrednost modularnosti
modularity(Odrednice_mreza_atributi_neusmerena, Odrednice_mreza_atributi_neusmerena.net.clust)