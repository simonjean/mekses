# mekses
MQSS

#CODE R POUR LE PROJET
#MQSS
library(haven)
AES2012INDIVIDU <- read_dta("~/Desktop/École/ENS/D3/Deuxième année/MQSS/lil-0820.dta/Stata/AES2012INDIVIDU.dta")
View(AES2012INDIVIDU)
d<-AES2012INDIVIDU

#-----Statistiques flash INSEE ----
  
# Accès à la formation
d$age1<-cut(d$age,c(18,24,34,44,54,64), labels = c("18-24 ans", "25-34 ans", "35-44 ans", "45-54 ans","55-64 ans"))


d$fed<-ifelse(d$fed==1,1,0)
mean(d$fed)

formation1<-ifelse(d$fed==1 | d$nfeguidedjt==1 | d$nfeloisir==1 | d$nfecourse==1 | d$nfeworkshop==1 |d$nfelesson==1,1,0)
mean(formation1)
prop.table(table(d$age1,formation1))


#### Statistiques de base

d$sex<- ifelse(d$sex==2,1,0)
d$sex[d$sex==1]<-"Femme"
d$sex[d$sex==0]<-"Homme"

mean(d$age)
min(d$age)
max(d$age)
quantile(d$age)


#------ Droit à la formation-----
  ### Droit individuel à la formation
d$dif<-recode1(d$dif,-2,2)
AES2012INDIVIDU$dif
prop.table(table(d$dif))

d$cif<-recode1(d$cif,-2,2)
prop.table(table(d$cif))
prop.table(table(d$cifuse,d$sex))
d$cif[d$cif=="Oui"]<-1
d$cif[d$cif=="Non"]<-0
d$cif<-as.integer(d$cif)

d$cifuse <- recode1(d$cifuse,-2,2)
prop.table(table(d$cifuse))
prop.table(table(d$cifuse,d$sex))

d$bc<-recode1(d$bc)
prop.table(table(d$bc))
prop.table(table(d$bc,d$sex))

  ### Régression de la connaissance
logit <- glm(d$cif ~ d$age + d$locsizefirm + d$csp)
###-----Recodage -----
d$dep[d$dep==01]<-"Ain"
d$dep[d$dep==02]<-"Aisne"
d$dep[d$dep==03]<-"Allier"
d$dep[d$dep==04]<-"Alpes de Haute Provence"
d$dep[d$dep==05]<-"Hautes Alpes"
d$dep[d$dep==06]<-"Alpes-Maritimes"
d$dep[d$dep==07]<-"Ardèche"
d$dep[d$dep==08]<-"Ardennes"
d$dep[d$dep==09]<-"Ariège"
d$dep[d$dep==10]<-"Aube"
d$dep[d$dep==11]<-"Aude"
d$dep[d$dep==12]<-"Aveyron"
d$dep[d$dep==13]<-"Bouches-du-Rhône"
d$dep[d$dep==14]<-"Calvados"
d$dep[d$dep==15]<-"Cantal"
d$dep[d$dep==16]<-"Charente"
d$dep[d$dep==17]<-"Charente-Maritime"
d$dep[d$dep==18]<-"Cher"
d$dep[d$dep==19]<-"Corrèze"
d$dep[d$dep=='2A']<-"Corse du Sud"
d$dep[d$dep=='2B']<-"Haute Corse"
d$dep[d$dep==21]<-"Côte d'Or"
d$dep[d$dep==22]<-"Côtes d'Armor"
d$dep[d$dep==23]<-"Creuse"
d$dep[d$dep==24]<-"Dordogne"
d$dep[d$dep==25]<-"Doubs"
d$dep[d$dep==26]<-"Drôme"
d$dep[d$dep==27]<-"Eure"
d$dep[d$dep==28]<-"Eure-et-Loir"
d$dep[d$dep==29]<-"Finistère"
d$dep[d$dep==30]<-"Gard"
d$dep[d$dep==31]<-"Haute Garonne"
d$dep[d$dep==32]<-"Gers"
d$dep[d$dep==33]<-"Gironde"
d$dep[d$dep==34]<-"Hérault"
d$dep[d$dep==35]<-"Ille-et-Vilaine"
d$dep[d$dep==36]<-"Indre"
d$dep[d$dep==37]<-"Indre-et-Loire"
d$dep[d$dep==38]<-"Isère"
d$dep[d$dep==39]<-"Jura"
d$dep[d$dep==40]<-"Landes"
d$dep[d$dep==41]<-"Loir-et-Cher"
d$dep[d$dep==42]<-"Loire"
d$dep[d$dep==43]<-"Haute-Loire"
d$dep[d$dep==44]<-"Loire-Atlantique"
d$dep[d$dep==45]<-"Loiret"
d$dep[d$dep==46]<-"Lot"
d$dep[d$dep==47]<-"Lot-et-Garonne"
d$dep[d$dep==48]<-"Lozère"
d$dep[d$dep==49]<-"Maine-et-Loire"
d$dep[d$dep==50]<-"Manche"
d$dep[d$dep==51]<-"Marne"
d$dep[d$dep==52]<-"Haute-Marne"
d$dep[d$dep==53]<-"Mayenne"
d$dep[d$dep==54]<-"Meurthe-et-Moselle"
d$dep[d$dep==55]<-"Meuse"
d$dep[d$dep==56]<-"Morbihan"
d$dep[d$dep==57]<-"Moselle"
d$dep[d$dep==58]<-"Nièvre"
d$dep[d$dep==59]<-"Nord"
d$dep[d$dep==60]<-"Oise"
d$dep[d$dep==61]<-"Orne"
d$dep[d$dep==62]<-"Pas-de-Calais"
d$dep[d$dep==63]<-"Puy-de-Dôme"
d$dep[d$dep==64]<-"Pyrrénées Atlantiques"
d$dep[d$dep==65]<-"Hautes-Pyrrénées"
d$dep[d$dep==66]<-"Pyrrénées Orientales"
d$dep[d$dep==67]<-"Bas-Rhin"
d$dep[d$dep==68]<-"Haut-Rhin"
d$dep[d$dep==69]<-"Rhône"
d$dep[d$dep==70]<-"Haute-Saône"
d$dep[d$dep==71]<-"Saône-et-Loire"
d$dep[d$dep==72]<-"Sarthe"
d$dep[d$dep==73]<-"Savoie"
d$dep[d$dep==74]<-"Haute-Savoie"
d$dep[d$dep==75]<-"Paris"
d$dep[d$dep==76]<-"Seine-Maritime"
d$dep[d$dep==77]<-"Seine-et-Marne"
d$dep[d$dep==78]<-"Yvelines"
d$dep[d$dep==79]<-"Deux-Sèvres"
d$dep[d$dep==80]<-"Somme"
d$dep[d$dep==81]<-"Tarn"
d$dep[d$dep==82]<-"Tarn-et-Garonne"
d$dep[d$dep==83]<-"Var"
d$dep[d$dep==84]<-"Vaucluse"
d$dep[d$dep==85]<-"Vendée"
d$dep[d$dep==86]<-"Vienne"
d$dep[d$dep==87]<-"Haute-Vienne"
d$dep[d$dep==88]<-"Vosges"
d$dep[d$dep==89]<-"Yonne"
d$dep[d$dep==90]<-"Territoire de Belfort"
d$dep[d$dep==91]<-"Essone"
d$dep[d$dep==92]<-"Hauts-de-Seine"
d$dep[d$dep==93]<-"Seine-Saint-Denis"
d$dep[d$dep==94]<-"Val-de-Marne"
d$dep[d$dep==95]<-"Val d'Oise"
    
    
d$locsizefirm[d$locsizefirm==-2]<-NA
d$locsizefirm<-as.integer(d$locsizefirm)
          
          
          
          
#### Formation pas finie
unique(d$difempaut)

which(colnames(d)=='difftype_01')
which(colnames(d)=='difftype_10')

for (i in seq(240,250)){
  d[[i]]<-recode1(d[[i]],-2,2)
}
for (i in seq(240, 250)){  print(prop.table(table(d[[i]],d$csp)))
}




#-----Résultats de la formation-----
source('/Users/simonjean/Desktop/École/ENS/D3/Deuxième année/MQSS/MQSS_fonctions.R')

#----- Recodage ------

which(colnames(d)=="fedoutcome_1")
which(colnames(d)=='fedoutcome_8')

for(i in seq(74,81)){
  d[[i]]<-AES2012INDIVIDU[[i]]
}
for(i in seq(74,81)){
  d[[i]]<-recode1(d[[i]],-2,2)
}

d$csp[d$csp==00] <- "Indéterminé"
d$csp[d$csp==11] <- "Agriculteur sur petite exploitation"
d$csp[d$csp==12] <- "Agriculteur sur moyenne exploitation"
d$csp[d$csp==13] <- "Agriculteur sur grande exploitation"
d$csp[d$csp==21] <- "Artisans"
d$csp[d$csp==22] <- "Commerçants et assimilés"
d$csp[d$csp==23] <- "Chef(fe)s d'entreprise de 10 salariés ou plus"
d$csp[d$csp==31] <- "Professions libérales et assmilées"
d$csp[d$csp==33] <- "Cadres de la fonction publique"
d$csp[d$csp==34] <- "Professeurs, professions scientifiques"
d$csp[d$csp==35] <- "Professions de l'information, des arts et des spectacles"
d$csp[d$csp==37] <- "Cadres administratifs et commerciaux d'entreprise"
d$csp[d$csp==38] <- "Ingénieurs et cadres techniques d'entreprise"
d$csp[d$csp==42] <- "Instituteurs et assimilés"
d$csp[d$csp==43] <- "Professions intermédiaires de la santé et du travail social"
d$csp[d$csp==44] <- "Clergés, religieux"
d$csp[d$csp==45] <- "Professions intermédiaires administratives de la fonction publique"
d$csp[d$csp==46] <- "Professions intermédiaires administratives et commerciales des entreprises"
d$csp[d$csp==47] <- "Techniciens"
d$csp[d$csp==48] <- "Contremaîtres, agents de maîtrise"
d$csp[d$csp==52] <- "Employés civils et agents de service de la fonction publique"
d$csp[d$csp==53] <- "Policiers et militaires"
d$csp[d$csp==54] <- "Employés administratifs d'entreprise"
d$csp[d$csp==55] <- "Employés de commerce"
d$csp[d$csp==56] <- "Personnels des services directs aux particuliers"
d$csp[d$csp==62] <- "Ouvriers qualifiés de type industriel"
d$csp[d$csp==63] <- "Ouvriers qualifiés de type artisanal"
d$csp[d$csp==64] <- "Chauffeurs"
d$csp[d$csp==65] <- "Ouvriers qualifiés de la manutention, du magasinage et du transport"
d$csp[d$csp==67] <- "Ouvriers non qualifiés de type industriel"
d$csp[d$csp==68] <- "Ouvriers non qualifiés de type artisanal"
d$csp[d$csp==69] <- "Ouvriers agricoles"
d$csp[is.na(d$csp)] <- "Sans objet"

csp1 <- d$csp
csp1[csp1=="Agriculteur sur petite exploitation"|csp1=="Agriculteur sur moyenne exploitation" | csp1=="Agriculteur sur grande exploitation"]<-"Agriculteurs·trices exploitant·e·s "
csp1[]
#-----Formation effectuée----
for(i in seq(74,81)){

  print(addmargins(prop.table((table(d[[i]], d$sex)))))
}
colnames(d[[74]])



### FICHIER FONCTIONS
#----FONCTIONS ----
##----Fonctions de recodage----

recode1 <- function(x,a,b){
  #Recoder les variables telles que NA=a et Non=b
  x[x==a]<-NA
  x[x==b]<-"Non"
  x[x==1]<-"Oui"
  print(x)
}

