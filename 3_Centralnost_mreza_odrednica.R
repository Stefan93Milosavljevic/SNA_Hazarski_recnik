##### CENTRALNOST U MREZI ODREDNICA HAZARSKOG RECNIKA

# Kod koji sledi zasnovan je prevashodno na:
# https://github.com/jeljov/SNALabs_at_UB [lab_2.R]
# https://sna.stanford.edu/lab.php?l=4
# delimicno i: https://www.markanthonyhoffman.com/social_network_analysis/

##### STRUKTURA R-SKRIPTE:
### POTREBNE BIBLIOTEKE
### UCITAVANJE MREZE
### CENTRALNOST STEPENA [CS]
### CENTRALNOST BLISKOSTI [CB]
### CENTRALNOST INTERMEDIJARNOSTI [CI]
### PAGE RANK [PR]
### Jedinstven DF za sve vrednosti centralnosti
### KORELACIJA MEDJU CENTRALNOSTIMA
### KORELACIJA CENTRALNOSTI SA DUZINOM ODREDNICA (PO BROJU TOKENA)

### POTREBNE BIBLIOTEKE:
library(igraph)
library(ggplot2)
library(tidyr)
library(corrplot)
library(dplyr)

### UCITAVANJE MREZE

Odrednice_veze<-read.csv('Lista_ivica.csv', header=TRUE, stringsAsFactors = F) # Ucitavamo podatke sa odrednicama i njihovim vezama
# View(Odrednice_veze) # Za pregled uvezenih podataka
# Primetimo da su podaci iz trece kolone (Frekvenca) automatski specifikovani kao tezine veza
Odrednice_atributi<-read.csv("Odrednice_atributi.csv", header=T, stringsAsFactors = F) # Ucitavamo podatke sa atributima

Odrednice_mreza_atributi<-graph_from_data_frame(Odrednice_veze, directed = T, vertices = Odrednice_atributi) # Na osnovu ucitanih podataka formiramo mrezu sa atributima cvorova
Odrednice_mreza_atributi
# Sledecim funkcijama proveravamo atribute u dobijenoj mrezi:
vertex_attr_names(Odrednice_mreza_atributi) # Imena atributa cvorova 
vcount(Odrednice_mreza_atributi)
edge_attr_names(Odrednice_mreza_atributi) # Imena atributa ivica (tezine)
ecount(Odrednice_mreza_atributi) # broj ivica

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


### CENTRALNOST STEPENA [CS]

CS_u<-degree(Odrednice_mreza_atributi, mode='in') # Ulazna centralnost stepena
CS_iz<-degree(Odrednice_mreza_atributi, mode='out') # Izlazna centralnost stepena

# Sa tezinama (zbir tezina susednih ivica za svaki cvor)
# Prakticno, meri frekvencu date odrednice u svim drugim odrednicama
CS_u_t<-strength(Odrednice_mreza_atributi, vids=V(Odrednice_mreza_atributi), mode="in", weights = E(Odrednice_mreza_atributi)$Frekvenca)
CS_iz_t<-strength(Odrednice_mreza_atributi, vids=V(Odrednice_mreza_atributi), mode="out", weights = E(Odrednice_mreza_atributi)$Frekvenca)

# Formiramo DF sa vrednostima CS
CS.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
           ime=V(Odrednice_mreza_atributi)$Ime, 
           CS_u,
           CS_iz,
           CS_u_t,
           CS_iz_t)

# Korelacija medju centralnostima stepena po usmerenosti i tezinama:
# (Svuda koristimo Spirmanov test, jer je ranije vec pokazano da podaci nemaju normalnu raspodelu)

korelacija_CS<- cor(CS.df[,3:6], 
                             use='complete.obs',
                             method = 'spearman')

# Vizualizacija korelacije pomocu paketa corrplot:
corrplot(corr = korelacija_CS, type = "upper", 
         diag = FALSE,
         addCoef.col = "black")

# Primeri vizualizacije
# Barplot:
barplot(sort(CS_u, decreasing = TRUE), main="Ulazna centralnost stepena", xlab="Odrednica", las=2) # Sortitano padajuce
barplot(sort(CS_iz, decreasing = TRUE), main="Izlazna centralnost stepena", xlab="Odrednica", las=2) # Sortitano padajuce

# Za vizualizaciju pomocu ggplot2 neophodno je transformisati DF sa formata "wide" u format "long":
# Koristiti funkciju gather() iz paketa tidyr

CS.df_long <- gather(data = CS.df, 
                            key = 'degree_type', 
                            value = 'degree_value', 
                            CS_u:CS_iz,
                            factor_key = TRUE)

# Kreiramo grafik:
ggplot(data = CS.df_long, 
       mapping = aes(x=id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "ID odrednice", y = "Ulazni + izlazni stepen", fill = "Tip stepena") +
  scale_x_continuous(breaks = seq(0,45,1)) +
  theme_bw()

# Sa malo sortitanja:
ggplot(data = CS.df_long, 
       mapping = aes(x=reorder(id, -degree_value), y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "ID odrednice", y = "Ulazni + izlazni stepen", fill = "Tip stepena") +
  scale_x_discrete(breaks = seq(0,45,1)) +
  theme_bw()

# Mozemo nacrtati i grafik za distribuciju stepena:
ggplot(data = CS.df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  # geom_histogram(bins = 15, position = 'dodge') +
  labs(x = "Vrednost stepena", y = "Gustina", fill = "Tip stepena") +
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,36,1)) +
  theme_bw()

# Sortiranje:
CS_sort_u<-CS.df[order(-CS.df$CS_u),]; CS_sort_u
CS_sort_u_t<-CS.df[order(-CS.df$CS_u_t),]; CS_sort_u_t
CS_sort_iz<-CS.df[order(-CS.df$CS_iz),]; CS_sort_iz
CS_sort_iz_t<-CS.df[order(-CS.df$CS_iz_t),]; CS_sort_iz_t

# Poredjenje prvih 10 odrednica:

CS_10.df<- cbind(CS_sort_u %>% select("id", "ime", "CS_u"),
                   CS_sort_u_t %>% select("id", "ime", "CS_u_t"),
                   CS_sort_iz %>% select("id", "ime", "CS_iz"),
                   CS_sort_iz_t %>% select("id", "ime", "CS_iz_t")) %>%
  head(10)
CS_10.df
# write.csv(CS_10.df, "CS_prvih_10.csv")

### CENTRALNOST BLISKOSTI [CB]

# Krenucemo od neusmerene mreze. Pre nego to izracunamo centralnost bliskosti, valja proveriti da li je mreza povezana. 
# Mreza je povezana ako postoji putanja izmedju svaka dva cvora u mrezi.
is_connected(Odrednice_mreza_atributi_neusmerena) # Jeste povezana
# Racunamo centralnost bliskosti:
CB_neusm<-closeness(Odrednice_mreza_atributi_neusmerena, normalized = TRUE)

# Neusmerena mreza sa tezinom ivica:
# [Veca tezina znaci vecu bliskosti]
CB_neusm_t<-closeness(Odrednice_mreza_atributi_neusmerena, normalized = TRUE,
                                                        weights = 1/E(Odrednice_mreza_atributi_neusmerena)$Frekvenca)
# Dobijene vrednosti upisujemo u DF:
CB_neusm.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
                  ime=V(Odrednice_mreza_atributi)$Ime, 
                  CB_neusm,
                  CB_neusm_t)

# Za vizualizaciju (poredjenje vrednosti sa tezinama i bez njih)
CB_neusm.df_long <- gather(data = CB_neusm.df, 
                     key = 'Tip_bliskosti', 
                     value = 'Vrednost_bliskosti', 
                     CB_neusm_t:CB_neusm,
                     factor_key = TRUE)
ggplot(data = CB_neusm.df_long, 
       mapping = aes(x=reorder(id, -Vrednost_bliskosti), y=Vrednost_bliskosti, fill=Tip_bliskosti)) +
  geom_col(position = 'dodge') +
  labs(x = "ID odrednice", y = "Bliskosti sa tezinama i bez tezina", fill = "Tip bliskosti") +
  scale_x_discrete(breaks = seq(0,45,1)) +
  theme_bw()

### Za nasu analizu vaznije su ulazne i izlazne vrednosti CB, s obzirom na usmerenost mreze
# STA MERI ULAZNA CENTRALNOST BLISKOSTI? 
      # Prosek broja koraka koje neko treba da preduzme da dodje do datog cvora sa svih drugih doseznih cvorova u mrezi.
# STA MERI IZLAZNA CENTRALNOST BLISKOSTI? 
      # Suprotno: prosek broja koraka od datog cvora do svih ostalih doseznih cvorova u mrezi
# Opet prvo proveravamo da li je mreza povezana
# U usmerenoj mrezi postoji slaba (weak) povezanost -- koja ne uzima u obzir usmerenost ivica, i jaka (strong) povezanost – koja uzima u obzir usmerenost ivica
is.connected(Odrednice_mreza_atributi, mode='strong') # Mreza nije povezana
# Stoga moramo vrsiti analizu nad dzinovskom komponentom, koja, naravno, jeste povezana:
is.connected(Odrednice_mreza_atributi_dz_k, mode='strong')
CB_u<-closeness(Odrednice_mreza_atributi_dz_k, mode = 'in', normalized = TRUE)
CB_iz<-closeness(Odrednice_mreza_atributi_dz_k, mode = 'out', normalized = TRUE)
# Najrelevantnije su nam vrednosti sa tezinama:          
CB_u_t<-closeness(Odrednice_mreza_atributi_dz_k, mode = 'in', normalized = TRUE,                                                                  
                                                   weights = 1/E(Odrednice_mreza_atributi_dz_k)$Frekvenca)
CB_iz_t<-closeness(Odrednice_mreza_atributi_dz_k, mode = 'out', normalized = TRUE,                                                                                 
                                                    weights = 1/E(Odrednice_mreza_atributi_dz_k)$Frekvenca)

# Dobijene vrednosti mozemo upisati u DF:
CB_usm.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi_dz_k)$name),
                  ime=V(Odrednice_mreza_atributi_dz_k)$Ime, 
                  CB_u,
                  CB_iz,
                  CB_u_t,
                  CB_iz_t)
# View(CB_usm.df)
# Posto se vrednosti CB neusmerene i usmerene mreze razlikuju po broju redova, spojicemo
# ih u jedinstven DF pomocu funkcije merge():
CB_sve.df<-merge(x = CB_neusm.df, 
                 y = CB_usm.df,
                 all = TRUE)
# View(CB_sve.df)
CB_sve.df<-CB_sve.df[, c("id", "ime", "CB_u", "CB_u_t", "CB_iz", "CB_iz_t", "CB_neusm", "CB_neusm_t")]

# Korelacija medju CB razlicitih vrednosti po usmerenosti i tezini
korelacija_CB<- cor(CB_sve.df[,3:8], 
                    use='complete.obs',
                    method = 'spearman')

# Vizualizacija korelacije pomocu paketa corrplot:
corrplot(corr = korelacija_CB, type = "upper", 
         diag = FALSE,
         addCoef.col = "black")

# Rezultate iz DF-a mozemo sortirati prema potrebi
# Najpre, neusmerena mreza:
CB_sort_neusm<-CB_sve.df[order(-CB_sve.df$CB_neusm),]; CB_sort_neusm
CB_sort_neusm_t<-CB_sve.df[order(-CB_sve.df$CB_neusm_t),]; CB_sort_neusm_t # prema vrednostima bez tezina, opadajuce

# Onda i usmerena:
CB_sort_u_t<-CB_sve.df[order(-CB_sve.df$CB_u_t),]; CB_sort_u_t # prema ulaznim vrednostima sa tezinama, opadajuce
CB_sort_u<-CB_sve.df[order(-CB_sve.df$CB_u),]; CB_sort_u # prema ulaznim vrednostima bez tezina, opadajuce
CB_sort_iz_t<-CB_sve.df[order(-CB_sve.df$CB_iz_t),]; CB_sort_iz_t # prema izlaznim vrednostima sa tezinama, opadajuce
CB_sort_iz<-CB_sve.df[order(-CB_sve.df$CB_iz),]; CB_sort_iz # prema izlaznim vrednostima sa tezinama, opadajuce

# Prvih 10 po CB
CB_10.df <- cbind(CB_sort_u %>% select(id, ime, CB_u),
                   CB_sort_u_t %>% select(id, ime, CB_u_t),
                   CB_sort_iz %>% select(id, ime, CB_iz),
                   CB_sort_iz_t %>% select(id, ime, CB_iz_t),
                   CB_sort_neusm %>% select(id, ime, CB_neusm),
                   CB_sort_neusm_t %>% select(id, ime, CB_neusm_t)) %>%
  head(10); format.data.frame(CB_10.df, digits=3)
# write.csv(CB_10.df, "CB_prvih_10.csv")

# Primeri vizualizacije pomocu barplota:
barplot(sort(CB_u, decreasing = TRUE), main="Ulazna centralnost bliskosti", xlab="Odrednica", las=2) # Sortitano padajuce
barplot(sort(CB_iz, decreasing = TRUE), main="Izlazna centralnost bliskosti", xlab="Odrednica", las=2) # Sortitano padajuce
barplot(sort(CB_neusm_t, decreasing = TRUE), main="Centralnost bliskosti neusmerene mreze \n sa tezinama", xlab="Odrednica", las=2) # Sortitano padajuce

# Odnos izmedju ulazne i izlazne bliskosti predstavicemo pomocu ggplota:
# Promena formata iz "wide" u "long"
centralnost_bliskosti.df_long <- gather(data=CB_sve.df, 
                                        key = 'degree_type', 
                                        value = 'degree_value', 
                                        CB_u,CB_iz,
                                        factor_key = TRUE)

ggplot(data = centralnost_bliskosti.df_long, 
       mapping = aes(x=id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "ID odrednice", y = "Ulazna + izlazna vrednost bliskosti", fill = "Tip bliskosti") +
  scale_x_discrete (limits = seq(1,45)) +
  theme_bw()

### CENTRALNOST INTERMEDIJARNOSTI [CI]

CI_neusm<-betweenness(Odrednice_mreza_atributi_neusmerena, normalized = TRUE)
CI_usm<-betweenness(Odrednice_mreza_atributi, directed=TRUE, normalized = TRUE)
CI_usm_t<-betweenness(Odrednice_mreza_atributi, directed=TRUE, normalized = TRUE, weights = E(Odrednice_mreza_atributi)$Frekvenca)
CI_neusm_t<-betweenness(Odrednice_mreza_atributi_neusmerena, normalized = TRUE, weights = E(Odrednice_mreza_atributi_neusmerena)$Frekvenca)

# Upisujemo u DF:
CI.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
                  ime=V(Odrednice_mreza_atributi)$Ime,
                  CI_usm,
                  CI_usm_t,
                  CI_neusm,
                  CI_neusm_t)

korelacija_CI<- cor(CI.df[,3:6], 
                    use='complete.obs',
                    method = 'spearman')

# Vizualizacija korelacije pomocu paketa corrplot:
corrplot(corr = korelacija_CI, type = "upper", 
         diag = FALSE,
         addCoef.col = "black")

# Sortiranje:
CI_sort_usm<-CI.df[order(-CI.df$CI_usm),]; CI_sort_usm # usmerena
CI_sort_neusm<-CI.df[order(-CI.df$CI_neusm),]; CI_neusm # neusmerena
CI_sort_usm_t<-CI.df[order(-CI.df$CI_usm_t),]; CI_sort_usm_t # usmerena sa tezinama
CI_sort_neusm_t<-CI.df[order(-CI.df$CI_neusm_t),]; CI_sort_neusm_t # neusmerena sa tezinama

# Prvih 10 odrednica po CI:
CI_10.df <- cbind(CI_sort_usm %>% select(id, ime, CI_usm),
                   CI_sort_usm_t %>% select(id, ime, CI_usm_t),
                   CI_sort_neusm %>% select(id, ime, CI_neusm),
                   CI_sort_neusm_t %>% select(id, ime, CI_neusm_t)) %>%
  head(10); format.data.frame(CI_10.df, digits=2)
# write.csv(CI_10.df, "CI_prvih_10.csv")

# Vizualizacija pomocu barplota:
barplot(sort(CI_usm, decreasing = TRUE), main="Intermedijarnost \n usmerena mreza", xlab="Odrednica", las=2) # Sortitano opadajuce
barplot(sort(CI_neusm, decreasing = TRUE), main="Intermedijarnost \n neusmerena mreza", xlab="Odrednica", las=2) # Sortitano opadajuce

### PAGE RANK [PR]

# Sa neusmerenom mrezom:
PR_neusm <- page_rank(Odrednice_mreza_atributi_neusmerena)$vector # Bez tezina
PR_neusm_t<-page_rank(Odrednice_mreza_atributi_neusmerena, weights = E(Odrednice_mreza_atributi_neusmerena)$Frekvenca)$vector # Sa tezinama

# Sa usmerenom mrezom:
PR_usm <- page_rank(Odrednice_mreza_atributi, directed = TRUE)$vector # Bez tezina
PR_usm_t<-page_rank(Odrednice_mreza_atributi, directed = TRUE, weights = E(Odrednice_mreza_atributi)$Frekvenca)$vector # Sa tezinama

# Upisujemo podatke u DF:
PR.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
                   ime=V(Odrednice_mreza_atributi)$Ime,
                   PR_neusm,
                   PR_neusm_t,
                   PR_usm,
                   PR_usm_t)

# Korelacija razlicitih 'varijanti' po usmerenosti i tezini
korelacija_PR<- cor(PR.df[,3:6], 
                    use='complete.obs',
                    method = 'spearman')

# Vizualizacija korelacije pomocu paketa corrplot:
corrplot(corr = korelacija_PR, type = "upper", 
         diag = FALSE,
         addCoef.col = "black")

# Soritranje:
PR_sort_usm<-PR.df[order(-PR.df$PR_usm),]; PR_sort_usm # usmerena, bez tezina, opadajuce
PR_sort_usm_t<-PR.df[order(-PR.df$PR_usm_t),]; PR_sort_usm_t # usmerenea, sa tezinama
PR_sort_neusm<-PR.df[order(-PR.df$PR_neusm),]; PR_sort_neusm # neusmerena, sa tezinama, opadajuce
PR_sort_neusm_t<-PR.df[order(-PR.df$PR_neusm_t),]; PR_sort_neusm_t # neusmerena, bez tezina, opadajuce

PR_10.df <- cbind(PR_sort_usm %>% select(id, ime, PR_usm),
                   PR_sort_usm_t %>% select(id, ime, PR_usm_t),
                   PR_sort_neusm %>% select(id, ime, PR_neusm),
                   PR_sort_neusm_t %>% select(id, ime, PR_neusm_t)) %>%
  head(10); format.data.frame(PR_10.df, digits=2)
# write.csv(PR_10.df, "PR_prvih_10.csv")

# Vizualizacija pomocu barplota:
barplot(sort(PR_usm, decreasing = TRUE), main="Page Rank \n usmerena mreža", 
        xlab="Odrednica", las=2) # Sortitano opadajuce
barplot(sort(PR_neusm, decreasing = TRUE), main="Page Rank \n neusmerena mreža", 
        xlab="Odrednica", las=2) # Sortitano opadajuce

### Jedinstven DF za sve vrednosti centralnosti

# Formiramo jedinstven DF, u koji cemo upisati gornje vrednosti centralnosti radi dalje analize
centralnost.df<-data.frame(id=as.integer(V(Odrednice_mreza_atributi)$name),
                                     ime=V(Odrednice_mreza_atributi)$Ime,
                                     CS_u,
                                     CS_u_t,
                                     CS_iz,
                                     CS_iz_t,
                                     CI_usm,
                                     CI_usm_t,
                                     CI_neusm,
                                     CI_neusm_t,
                                     PR_usm,
                                     PR_usm_t,
                                     PR_neusm,
                                     PR_neusm_t,
                                     CB_neusm,
                                     CB_neusm_t)

# Pomocu funkcije merge() dodajemo vrednosti ulazne i izlazne centralnosti bliskosti
centralnost.df<- merge(x = centralnost.df, 
                                        y = CB_usm.df,
                                        all = TRUE)
# View(centralnost.df)
# Malo cemo promeniti redosled kolona:
centralnost.df<-centralnost.df[, c("id", "ime", "CS_u", "CS_u_t", "CS_iz", "CS_iz_t", "CB_u", "CB_u_t", "CB_iz", "CB_iz_t", "CB_neusm", "CB_neusm_t", 
                                   "CI_usm", "CI_usm_t", "CI_neusm", "CI_neusm_t", "PR_usm", "PR_usm_t", "PR_neusm", "PR_neusm_t")]

### KORELACIJA MEDJU CENTRALNOSTIMA
# Da bismo odredili kako da racunamo korelaciju, moramo proveriti da li nasi podaci imaju normalnu raspodelu
apply(centralnost.df[,3:20], 2, shapiro.test)
apply(centralnost.df[,3:20], 2, qqnorm)
# Nemaju sve mere centralnosti normalnu distribuciju, pa koristimo Spirmanov test korelacije:
centralnost_korelacija<- cor(centralnost.df[,3:20], 
                              use='complete.obs', # zbog nekoliko NA vrednosti
                              method = 'spearman')
# Posto smo napred vec racunali korelaciju izmedju razlicitih 'varijanti' istih centralnosti,
# sada cemo se, radi preglednosti, zadrzati na korelaciji izmedju razlicitih mera centralnosti;
# dodatno, ogranicicemo se samo na usmerene i mreze bez tezina, jer su ti rezultati od primarnog znacaja za nas rad

centralnost_korelacija_skr<- cor(centralnost.df[c("CS_u","CS_iz","CB_u","CB_iz","CI_usm","PR_usm")], # Samo za neke varijante po usmerenosti i tezini
                              use='complete.obs', # zbog nekoliko NA vrednosti
                              method = 'spearman')

# Vizualizacija korelacije pomocu paketa corrplot:
corrplot(corr = centralnost_korelacija_skr, type = "upper", 
         diag = FALSE,
         addCoef.col = "black")
# https://rpubs.com/melike/corrplot

### KORELACIJA CENTRALNOSTI SA DUZINOM ODREDNICA (PO BROJU TOKENA)

duzina_odrednica<-read.csv('Frekvenca_tokena.csv')
# View(duzina_odrednica)
# Radi, preglednosti, formiramo i DF sa imenima odrednica
duzina_odrednica_sa_imenima<-data.frame(centralnost.df$id,
                                        centralnost.df$ime,
                                        duzina_odrednica$Tokens)
colnames(duzina_odrednica_sa_imenima)<-c("id", "ime", "broj_tokena")
duzina_odrednica_sort<-duzina_odrednica_sa_imenima[order(-duzina_odrednica_sa_imenima$broj_tokena),];duzina_odrednica_sort
# write.csv(duzina_odrednica_sort, "Duzina_odrednica_po_broju_tokena.csv")


korelacija_duzina<-cor(centralnost.df[c(3:20)], duzina_odrednica_sa_imenima$broj_tokena, 
                       use='complete.obs', # zbog nekoliko NA vrednosti
                       method = 'spearman')
format.data.frame(as.data.frame(korelacija_duzina), digits=2)
# write.csv(format.data.frame(as.data.frame(korelacija_duzina), digits=2), "Korelacija_centralnost_duzina.csv")

### POREDJENJE TROJKI ODREDNICA

# Najpre, za lakse poredjenje, dodacemo rangove u kolunu za sve odrednice prema vrednostima centralnosti:
centralnost.df$rang_CS_u<-rank(-centralnost.df$CS_u, ties.method="min")
centralnost.df$rang_CS_iz<-rank(-centralnost.df$CS_iz, ties.method="min")
centralnost.df$rang_CS_u_t<-rank(-centralnost.df$CS_u_t, ties.method="min")
centralnost.df$rang_CS_iz_t<-rank(-centralnost.df$CS_iz_t, ties.method="min")
centralnost.df$rang_CB_u<-rank(-centralnost.df$CB_u, ties.method="min")
centralnost.df$rang_CB_iz<-rank(-centralnost.df$CB_iz, ties.method="min")
centralnost.df$rang_CB_u_t<-rank(-centralnost.df$CB_u_t, ties.method="min")
centralnost.df$rang_CB_iz_t<-rank(-centralnost.df$CB_iz_t, ties.method="min")
centralnost.df$rang_CB_neusm<-rank(-centralnost.df$CB_neusm, ties.method="min")
centralnost.df$rang_CB_neusm_t<-rank(-centralnost.df$CB_neusm_t, ties.method="min")
centralnost.df$rang_CI_usm<-rank(-centralnost.df$CI_usm, ties.method="min")
centralnost.df$rang_CI_neusm<-rank(-centralnost.df$CI_neusm, ties.method="min")
centralnost.df$rang_CI_usm_t<-rank(-centralnost.df$CI_usm_t, ties.method="min")
centralnost.df$rang_CI_neusm_t<-rank(-centralnost.df$CI_neusm_t, ties.method="min")
centralnost.df$rang_PR_usm<-rank(-centralnost.df$PR_usm, ties.method="min")
centralnost.df$rang_PR_usm_t<-rank(-centralnost.df$PR_usm_t, ties.method="min")
centralnost.df$rang_PR_neusm<-rank(-centralnost.df$PR_neusm, ties.method="min")
centralnost.df$rang_PR_neusm_t<-rank(-centralnost.df$PR_neusm_t, ties.method="min")

# View(centralnost.df) # Odavde cemo izdvojiti one varijante po usmerenosti i tezini koje smo i napred mahom komentarisali:
centralnost_rang.df<-centralnost.df[c("id", "ime", "CS_u", "rang_CS_u", "CS_u_t", "rang_CS_u_t", "CS_iz", "rang_CS_iz", "CS_iz_t", "rang_CS_iz_t", 
                                     "CB_u", "rang_CB_u", "CB_u_t", "rang_CB_u_t", "CB_iz", "rang_CB_iz", "CB_iz_t", "rang_CB_iz_t", 
                                     "CB_neusm", "rang_CB_neusm", "CB_neusm_t", "rang_CB_neusm_t",
                                     "CI_usm", "rang_CI_usm", "CI_usm_t", "rang_CI_usm_t", "CI_neusm", "rang_CI_neusm", "CI_neusm_t", "rang_CI_neusm_t", 
                                     "PR_usm", "rang_PR_usm", "PR_usm_t", "rang_PR_usm_t","PR_neusm", "rang_PR_neusm", "PR_neusm_t", "rang_PR_neusm_t")]
# View(centralnost_skr.df)
trojke_id<-c("1", "17", "31", "4", "20", "34", "12", "29", "41", "13", "30", "42", "11", "21", "39", 
             "6","16", "44", "2", "23", "35", "10", "25", "45", "7", "15", "37") # id-ovi trojki koje nas zanimaju (odgovarajucim redom)
# View(centralnost_rang.df)

# CS:
centralnost_trojke.CS.df<-format.data.frame(centralnost_rang.df[trojke_id, 
                                                                (c(1:10))], digits=2); centralnost_trojke.CS.df
# write.csv(centralnost_trojke.CS.df, "Centralnost_trojke.CS.csv")

# CB:
centralnost_trojke.CB.df<-format.data.frame(centralnost_rang.df[trojke_id, 
                                                                (c(1, 2, 11:22))], digits=2); centralnost_trojke.CB.df
# write.csv(centralnost_trojke.CB.df, "Centralnost_trojke.CB.csv")

# CI:

centralnost_trojke.CI.df<-format.data.frame(centralnost_rang.df[trojke_id, 
                                                                (c(1, 2, 23:30))], digits=2); centralnost_trojke.CI.df
# write.csv(centralnost_trojke.CI.df, "Centralnost_trojke.CI.csv")

# PR:
centralnost_trojke.PR.df<-format.data.frame(centralnost_rang.df[trojke_id, 
                                                                (c(1, 2, 31:38))], digits=2); centralnost_trojke.PR.df
# write.csv(centralnost_trojke.PR.df, "Centralnost_trojke.PR.csv")
