library(readxl)
library(httr)
library(ggplot2)
library(reshape2) # melt() et dcast()

setwd("D:/Kuzulia/Docs_stat/2020_covid")

#############################
# france : liste des d�partements et r�gions

departements <- read_excel("departements-francais.xls")
names(departements)=tolower(names(departements))
names(departements)[1]="dep"
summary(departements)


#############################
# france : donn�es hospitali�res

#https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/

fr <- read.csv2("donnees-hospitalieres-covid19-2020-04-01-19h00.csv", na.strings = c("NA",""))
fr$date=as.Date(fr$jour, "%Y-%m-%d")
summary(fr)
dim(fr)

fr[is.na(fr$dep),]

#dep		D�partement	Department	1
#sexe		Sexe 	Sex 	0
#jour		Date de notification 	Date of notice	18/03/2020
#hosp		Nombre de personnes actuellement hospitalis�es	Number of people currently hospitalized	2
#rea		Nombre de personnes actuellement en r�animation ou soins intensifs	Number of people currently in resuscitation or critical care	0
#rad		Nombre cumul� de personnes retourn�es � domicile	Total amount of patient that returned home	1
#dc		Nombre cumul� de personnes d�c�d�es � l'h�pital	Total amout of deaths at the hospital

# sexe :
# 0	femmes + hommes	
# 1	hommes
# 2	femmes


#############################
# data france : merge data fr et data departements

levels(fr$dep)[1:9]=as.factor(1:9)

departements$dep%in%levels(fr$dep) # v�rif : ok

france=merge(fr, departements[,1:3])
france$sexe=as.factor(france$sexe)
france$region=as.factor(france$region)
france$departement=as.factor(france$nom)
summary(france)

#############################
# tata : subset de data france

levels(france$region)
selregion="Bretagne"
seldep=c("22","29","35","44", "56")
# seldep=c("22","29","35","44", "56", "53") # avec la mayenne

tata=droplevels(subset(france, region%in%selregion & sexe=="0"))
tata=droplevels(subset(france, dep%in%seldep & sexe=="0"))

# sort departement by total number of hosp to define a new order for the levels (alphabetical order otherwise)
sort(tapply(tata$hosp, tata$departement, max, na.rm=TRUE), decreasing = TRUE)
depNewLevels=names(sort(tapply(tata$hosp, tata$departement, max, na.rm=TRUE), decreasing = TRUE))
tata$departement=factor(tata$departement,depNewLevels)

ggplot(data=tata) + aes(y=hosp, x=date, col=departement) +theme_classic() +
	geom_point() + geom_line() + 
	ggtitle("Nombre de personnes hospitalis�es")+
	labs(y="", x="Date", col="d�partement")+ 
  	scale_colour_brewer(palette="Set1")
ggsave("hosp.png")

ggplot(data=tata) + aes(y=rea, x=date, col=departement) +theme_classic() +
	geom_point() + geom_line() + 
	ggtitle("Nombre de personnes en r�animation ou soins intensifs")+
	labs(y="", x="Date", col="d�partement")+ 
  	scale_colour_brewer(palette="Set1")
ggsave("rea.png")

ggplot(data=tata) + aes(y=dc, x=date, col=departement) +theme_classic() +
	geom_point() + geom_line() + 
	ggtitle("Nombre cumul� de personnes d�c�d�es � l'h�pital")+
	labs(y="", x="Date", col="d�partement")+ 
  	scale_colour_brewer(palette="Set1")
ggsave("deces.png")


