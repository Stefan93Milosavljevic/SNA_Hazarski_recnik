## EKSTRAKCIJA VEZA ZA MREZU ODREDNICA HAZARSKOG RECNIKA MILORADA PAVICA
##################################################################
## OPIS PROBLEMA ##
# Podeliti tekst romana na odrednice (= poglavlja)
# Odrediti kako su odrednice medjusobno povezane
# Dve odrednice su u vezi ako se ime jedne pominje u tekstu druge 
# (= ako se ime jednog poglavlja pominje u drugom poglavlju)
##################################################################

##################################################################
# Glavna biblioteka koje ce biti koriscena: Quanteda
library(quanteda)
# https://quanteda.io/
# Replikacija Jockersove knjige:
# https://quanteda.io/articles/pkgdown/replication/digital-humanities.html 
# Tutorijal: https://tutorials.quanteda.io
# Trebace nam i:
library(stringi)
require(readtext)
require(stringr)
##################################################################

##################################################################
options(max.print=1000000) # za opseg prikazivanja na ekranu
##################################################################

##################################################################
# Ucitavanje teksta (tip: character)
tekst.HR<-texts(readtext("HR_1997_sa_zamenjenim_slovima.txt")) # Ucitavamo tekst Hazarskog recnika
##################################################################

##################################################################
## PODELA NA ODREDNICE ##
# Vazno: u tekstu romana rucno je iznad svakog poglavlja dodat naslov
# tipa ODREDNICA 1 ... ODREDNICA 45, kako bi svaki naziv pocinjao na 
# isti nacin; to stoga sto se podele na poglavlja uglavnom zasnivaju
# na unapred pripremljenim tekstovima sa nazivima poglavljima koji pocinju isto

# ODREDjUJEMO POCETAK I KRAJ TEKSTA KOJI NAM JE POTREBAN (BEZ METAPODATAKA)
(pocetak.v <- stri_locate_first_fixed(tekst.HR, "ODREDNICA 1")[1])
(kraj.v <- stri_locate_last_fixed(tekst.HR, "Zxuta knxiga - kraj")[1])
tekst.HR.v <- stri_sub(tekst.HR, pocetak.v, kraj.v) # Ovde je pohranjen tekst bez metapodataka, kao vektor

# TEKST ROMANA SADA TREBA PODELITI NA POGLAVLJA (= ODREDNICE), RADI DALJE ANALAIZE
pozicije_odrednica.v <- kwic(tekst.HR.v, phrase(c("ODREDNICA \\d")), valuetype = "regex")$from
head(pozicije_odrednica.v)
tail(pozicije_odrednica.v)

# TEKST SVAKOG POGLAVLJA (ODREDNICE) CUVACEMO U ZASEBNOM DOKUMENTU U OKVIRU KORPUSA
odrednice.korpus <- 
  corpus(tekst.HR.v) %>%
  corpus_segment(pattern = "ODREDNICA\\s\\d+.*\\n", valuetype = "regex")
summary(odrednice.korpus, 10) # Prikazi podatke o prvih 10
# Malo 'ciscenja': da se uklone tragovi novih redova (\n):
docvars(odrednice.korpus, "pattern") <- stringi::stri_trim_right(docvars(odrednice.korpus, "pattern"))
summary(odrednice.korpus, n = 3)
docnames(odrednice.korpus) <- docvars(odrednice.korpus, "pattern") # Preimenujemo poglavlja da se zovu kako se zovu u dokumentu
summary(odrednice.korpus)

# Frekvenca tokena po odrednicama (trebace za neke kasnije analize)
frekvenca_tokena_odrednice.df<-as.data.frame(summary(odrednice.korpus))
frekvenca_tokena_odrednice.df
# write.csv(frekvenca_tokena_odrednice.df, "Frekvenca_tokena.csv")

#TEKST ODREDNICA U KORPUSU TOKENIZUJEMO ("PRETVARAMO U TOKENE=RECI") RADI DALJE ANALIZE
odrednice.korpus.tokeni<-tokens(odrednice.korpus, remove_punct=TRUE) # tekstovi poglavlja u korpusu pretvoreni u tokene, 
# bez interpunkcije 
# NAPOMENA: Ako zelimo da pretvorimo u mala slova, iskoristicemo funkciju tokens_tolower()
# Nama ne treba pretvaranje u mala slova, jer su nam za adekvatnu identifikaciju potrebna i velika i mala slova
odrednice.korpus.tokeni[1] # Mala proba: Prikazi tokene iz prve odrednice

################################################################
## SLOZENICE ##
# Napravicemo neke slozenice, kako bismo mogli da pretrazujemo viseclane nazive

?tokens_compound()
odrednice.tokeni.korpus_sa_slozenicama<-tokens_compound(odrednice.korpus.tokeni, pattern 
                                                        = phrase(c('Veridben* ugovor* Samuel* Koen*',
                                                                   'Lov* snov*',
                                                                   'Lov* na snov*',
                                                                   'Hazarsk* polemi*',
                                                                   'Zidar* muzik*',
                                                                   'Ibn* Kor*',
                                                                   'Liber Cosri',
                                                                   'Hazarsk* cxup*',
                                                                   'Odlom* iz Basr*',
                                                                   case_insensitive=TRUE
                                                        )))
# Malo probe:
odrednice.tokeni.korpus_sa_slozenicama[2] # Prikazi tokene, ukljucujuci i slozenice, iz 2. poglavlja
tokens_select(odrednice.tokeni.korpus_sa_slozenicama[2], pattern="Avram*Brankovicx*") # Selektuj tokene "Avram Brankovicx" u razl. padežima
tokens_select(odrednice.tokeni.korpus_sa_slozenicama[35], pattern="Sablxak*")


#################################################################

## RECNIK ODREDNICA ##
# Pravimo Recnik odrednica, koji ce nam pomoci da u korpusu tokena pronadjemo nazive odrednica u svakoj odrednici
# Alterativne izraze trazimo pomocu konkatenacije, tj. c().
# Na primer: Odrednica.2=c("Brankovicx*Avram*", "Avram*Brankovicx*")
# Zahvaljujuci konstruisanim slozenicama, mozemo da trazimo viseclane nazive tipa Brankovicx*Avram* (gde * pokriva bilo koji znak, 
# ukljucujuci i donju crtu (_), koja se automatski dodaje pri slaganju tokena u viseclane izraze)

# ?dictionary()
odrednice.recnik<-dictionary(list(
  Odrednica.1.17.31='Ateh',  
  Odrednica.2="*Avram*", # Avram se u romanu odnosi samo na Avrama Brankovica, a ovaj izraz obuhvatice zapise poput "Avram", "papas-Avram";
  # ne moze se traziti prema "Brankovicx" jer ima i drugih Brankovica (Grgur, Brankovici kao porodica itd.)
  Odrednica.3.9=c("Grgur*", "Stolpnik*"), # Ne moze kao "Brankovicx" iz istog razloga kao gornja odrednica
  Odrednica.4.20.34='kagan*',
  Odrednica.5=c("Lov*snov*", "lov*snov", "Lov*na*snov", "lov*na*snov*"), 
  Odrednica.6='Metodij*',
  Odrednica.7='Sevast*',
  Odrednica.8='Skil*',
  Odrednica.10=c("SUK", "Suk", "Suka", "Suku", "Sukom", "Sukovi"), # Trazimo sve oblike registrovane u frekv. recniku Hazarskog recnika (Vasic, 1998);
  # izbegavamo reg. izraz zbog mnogih drugih reci koje pocinju istim nizom karaktera
  Odrednica.11=c("Chiril*", "Konstantin*"),
  Odrednica.12.29.41='Hazar*',
  Odrednica.13.30.42=c("Hazarsk*polemi*", "hazarsk*polemi"),
  Odrednica.14='Cyelarev*',
  Odrednica.15='Aksxan*',
  Odrednica.16="*Bekr*", # *Bekr* -- da bismo obuhvatili i "prefikse" tipa "Al-Bekri"
  Odrednica.18=c("Zidar*muzik*", "zidar*muzik*"),
  Odrednica.19='Hadrasx*',
  Odrednica.21=c("*Ibn*Kor*", "*Ibn-Kor*"),
  Odrednica.22='Ku',
  Odrednica.23='Masud*',
  Odrednica.24.38='Mokadas*',
  Odrednica.25="Muavij*",
  Odrednica.26="Sablxak*",
  Odrednica.27=c("Odlom*iz*Basre", "odlom*iz*Basre"),
  Odrednica.28='Prstomet*',
  Odrednica.32=c("Veridben*ugovor*Samuel*Koen*", "veridben*ugovor*Samuel*Koen*"),
  Odrednica.33='Daubmanus*',
  Odrednica.35='Koen*',
  Odrednica.36="Liber*Cosri",
  Odrednica.37="Efrosinij*", # Prema imenu, jer se tako cesce javlja (Efrosinija 27 : Lukarevic 8), 
  # a sem Efrosinije ima drugih Lukarevica (npr. gospar Lukarevic)
  Odrednica.39='Sangar*',
  Odrednica.40='Tibon*',
  Odrednica.43=c("Hazarsk*cxup*", "hazarsk*cxup"),
  Odrednica.44='Halev*',
  Odrednica.45="Sxulc*"))

########
# Sada cemo videti gde se u korpusu tokena po odrednicama nalaze one koje su definisane recnikom odrednica.
# Drugim recima: videcemo koje se odrednce pominju u svakoj drugoj odrednici, na osnovu cega cemo utvrditi veze za SNA analizu

# ?tokens_lookup()
recnik.tokeni<-tokens_lookup(odrednice.tokeni.korpus_sa_slozenicama, dictionary=odrednice.recnik)
recnik.tokeni # Daje spisak svih pominjanja odrednica u drugim odrednicama

# Posto je Hazarski recnik podeljen na tri dela (Crvenu, Zelenu i Zutu knjigu), postoje neke odrednice 
# koje se isto zovu (jer se javljaju u sve tri knjige), npr. Ateh - sto je ime 1, 17. i 31. odrednice, 
# pa u Recniku odrednica i stoji Odrednica.1.17.31. 
# Kada se u tekstu neke druge odrednice (recimo, Cyirilo), pominje Ateh, odn. Odrednica 1.17.31, 
# to znaci da je ta druga odrednica, npr. Cyrilo, istovremeno povezana sa 3 odrednice - Ateh u sve tri knjige, 
# odn. i sa Odrednicom 1 i sa Odrednicom 17 i sa Odrednicom 31.
# Stoga u ovde dobijenoj listi treba uraditi takve zamene da se Odrednica.1.17.31 zameni sa 3 odrednice (Odrednica.1,
# Odrednica.17 i Odrednica.31)

odrednice_visestruko_ime <- c("Odrednica.1.17.31", "Odrednica.3.9", "Odrednica.4.20.34", "Odrednica.12.29.41",
                          "Odrednica.13.30.42", "Odrednica.24.38")
# Prvo, brojima pojavljivanja odrednica/poglavlja sa visestrukim nazivima
visestruko_ime_broj <- list()
for(i in 1:length(recnik.tokeni)) {
  zamena_broj <- list(o1.17.31 = 0, o3.9 = 0, o4.20.34 = 0, o12.29.41 = 0, o13.30.42 = 0, o24.38 = 0)
  for(j in 1:length(odrednice_visestruko_ime))
    zamena_broj[[j]] <- sum(!is.na(str_match(recnik.tokeni[[i]], odrednice_visestruko_ime[j])))
  visestruko_ime_broj[[i]] <- zamena_broj
}

# Drugo, uklanjamo slozena imena
recnik.tokeni.novo <- tokens_remove(recnik.tokeni, pattern = odrednice_visestruko_ime, 
                                   valuetype = "fixed", case_insensitive = FALSE)

# Trece, na osnovu vrednosti u visestruko_ime_broj, dodajemo individualne oznake poglavlja/odrednica
odrednice_novo <- list()
for(i in 1:length(visestruko_ime_broj)) {
  odrednice_pominjanja <- recnik.tokeni.novo[[i]]
  odrednice_broj <- visestruko_ime_broj[[i]]
  if(odrednice_broj$o1.17.31 > 0) {
    cnt <- odrednice_broj$o1.17.31
    odrednice_pominjanja <- c(odrednice_pominjanja, rep("Odrednica.1", cnt), rep("Odrednica.17", cnt), rep("Odrednica.31", cnt))
  }
  if(odrednice_broj$o4.20.34 > 0) {
    cnt <- odrednice_broj$o4.20.34
    odrednice_pominjanja <- c(odrednice_pominjanja, rep("Odrednica.4", cnt), rep("Odrednica.20", cnt), rep("Odrednica.34", cnt))
  }
  if(odrednice_broj$o3.9 > 0) {
    cnt <- odrednice_broj$o3.9
    odrednice_pominjanja <- c(odrednice_pominjanja, rep("Odrednica.3", cnt), rep("Odrednica.9", cnt))
  }
  if(odrednice_broj$o12.29.41 > 0) {
    cnt <- odrednice_broj$o12.29.41
    odrednice_pominjanja <- c(odrednice_pominjanja, rep("Odrednica.12", cnt), rep("Odrednica.29", cnt), rep("Odrednica.41", cnt))
  }
  if(odrednice_broj$o13.30.42 > 0) {
    cnt <- odrednice_broj$o13.30.42
    odrednice_pominjanja <- c(odrednice_pominjanja, rep("Odrednica.13", cnt), rep("Odrednica.30", cnt), rep("Odrednica.42", cnt))
  }
  if(odrednice_broj$o24.38 > 0) {
    cnt <- odrednice_broj$o24.38
    odrednice_pominjanja <- c(odrednice_pominjanja, rep("Odrednica.24", cnt), rep("Odrednica.38", cnt))
  }
  odrednice_novo[[i]] <- odrednice_pominjanja
}

odrednice_novo


odrednica_frekvenca <- function(odrednica_id) {
  frekvenca_pomenute_odrednice <- list()
  pomenute_odrednice <- sort(unique(odrednice_novo[[odrednica_id]]))
  for(i in 1:length(pomenute_odrednice)) {
    odrednica_aktuelna <- str_split(pomenute_odrednice[i], fixed("."))[[1]][2] %>% as.integer()
    if(odrednica_aktuelna != odrednica_id)
      frekvenca_pomenute_odrednice[[pomenute_odrednice[i]]] <- sum(str_count(odrednice_novo[[odrednica_id]], 
                                                                       paste0("^", pomenute_odrednice[i], "$")))
  }
  frekvenca_pomenute_odrednice
}

odrednica_frekvenca(1)


frekvenca_svih_odrednica <- list()
for(i in 1:45) 
  frekvenca_svih_odrednica[[i]] <- odrednica_frekvenca(i)

saveRDS(frekvenca_svih_odrednica, "lista_frekvencija_pominjanja_odrednica.RData")



################### Priprema podataka za ucitavanje mreze za SNA ################### 

sve_odrednice_frekv.Rdata<-readRDS('lista_frekvencija_pominjanja_odrednica.RData') # Podaci iz snimljenog RData fajla
str(sve_odrednice_frekv.Rdata)
# View(sve_odrednice_frekv.Rdata)
# Ove podatke smesticemo najpre u DF, radi dalje analize:
library(data.table) # install.packages('data.table')
sve_odrednice_frekv.df<-rbindlist(sve_odrednice_frekv.Rdata, fill=TRUE) # Ucitavamo veze medju odrednicama sa frekvencama kao DF
View(sve_odrednice_frekv.df)

# Da bismo dobili DF 45x45 (pošto veze među nekim odrednicama izostaju), dodaćemo imena onih odrednica za koje nedostaju kolone:
imena_kolona<-c('Odrednica.18', 'Odrednica.32', 'Odrednica.27', 'Odrednica.43', 'Odrednica.45')
for (i in imena_kolona) {
  sve_odrednice_frekv.df[, i] <- NA # Pripisujemo NA vrednosti
}
# View(sve_odrednice_frekv.df)

rownames(sve_odrednice_frekv.df)<-c(1:45) # Imena redova prema rednim brojevima odrednica
# Promenićemo i imena kolona tako da se zovu isto kao redovi (1...45) [tj. uklanjamo deo 'Odrednica.']:
colnames(sve_odrednice_frekv.df) = gsub(pattern = "Odrednica.", fixed=TRUE, replacement = "", x = colnames(sve_odrednice_frekv.df))
sve_odrednice_frekv.df[is.na(sve_odrednice_frekv.df)] <- 0 # Zamenjujemo NA vrednosti sa 0
# Pravimo numeričku matricu za ucitavanje grafa:

matrica<-data.matrix(sve_odrednice_frekv.df); matrica<- matrica[, order(as.integer(colnames(matrica)))] # Sortiramo po imenima kolona (1:45)
# View(matrica)

# Na osnovu ove matrice moci ce se ucitati podaci za SNA analizu:
library(igraph)
Mreza_odrednica<-graph_from_adjacency_matrix(matrica, mode = 'directed', weighted = TRUE) # Ucitavanje usmerene mreze odrednica sa tezinama
Mreza_odrednica 
# Formiracemo i listu ivica (edgelist), upisati je u csv. Nadalje cemo koristiti taj csv kao osnov za formiranje grafa za analizu
set.seed(1)
Lista_ivica<-get.data.frame(Mreza_odrednica)
colnames(Lista_ivica)<-c("Od", "Do", "Frekvenca")
View(Lista_ivica)
write.csv(Lista_ivica, "Lista_ivica.csv", row.names = FALSE)