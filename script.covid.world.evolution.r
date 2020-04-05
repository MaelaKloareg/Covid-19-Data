# charts of total covid-19 cases & deaths evolution 
# The data is downloaded from : 
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

# these libraries are necessary

library(readxl)
library(httr)
library(ggplot2)

aujourdhui=format(Sys.Date(),"%d/%m%/%Y")
hier=format(Sys.Date()-1,"%d/%m%/%Y")
dateconfinement = as.Date("2020-03-17")


# create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#url = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-04-02.xlsx"

# download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

# read the Dataset sheet into “R”

data <- read_excel(tf)

data$dateRep=as.Date(data$dateRep)

summary(data)

############
### data manipulation and country selection

names(data)[7]="Country"

data$Country=as.factor(data$Country)

data=data[order(data$dateRep),]
data=data[order(data$Country),]

data$cumulCases = unlist(tapply(data$cases, data$Country, cumsum))
data$cumulDeaths = unlist(tapply(data$deaths, data$Country, cumsum))

data$cumulCases[data$cumulCases==0]=NA
data$cumulDeaths[data$cumulDeaths==0]=NA

subset(data, Country%in%c("France","Italy","Spain","United_Kingdom", "United_States_of_America")&dateRep>"2020-04-04")

# sort countries by total number of deaths
sort(tapply(data$cumulDeaths, data$Country, max, na.rm=TRUE), decreasing = TRUE)[1:20]

# select the 12 countries with the more deaths (for example)
#toto=droplevels(subset(data, Country%in%
#	names(sort(tapply(data$cumulDeaths, data$Country, max, na.rm=TRUE), decreasing = TRUE)[1:12])))

# my selection of countries
toto=droplevels(subset(data, 
	Country%in%c("France","Germany","Italy","Spain","United_Kingdom", 
	"Netherlands", "Switzerland", "Belgium", "United_States_of_America", "Sweden", "Portugal", "Turkey")
	&cumulDeaths>0))

# optional french translation
levels(toto$Country)=c("Belgique","France", "Allemagne","Italie", "Pays-Bas", 
	"Portugal", "Espagne", "Suisse", "Suède", "Turquie", "Royaume-Uni", "Etats-Unis")


# sort countries by total number of deaths to define a new order for the levels (alphabetical order otherwise)
sort(tapply(toto$cumulDeaths, toto$Country, max, na.rm=TRUE), decreasing = TRUE)
CountryNewLevels=names(sort(tapply(toto$cumulDeaths, toto$Country, max, na.rm=TRUE), decreasing = TRUE))
toto$Country=factor(toto$Country,CountryNewLevels)

summary(toto)

# palette
library(RColorBrewer)
display.brewer.all() 
#getPalette = colorRampPalette(brewer.pal(9, "Set1"))
#mapalette=getPalette(nlevels(toto$Country))

mapalette=c(brewer.pal(9, "Set1"),brewer.pal(3, "Accent"))

############
# Deaths
# for total number of deaths > 10

ggplot(data=subset(toto, cumulDeaths>10)) + aes(y=cumulDeaths, x=dateRep, group=Country, col=Country) +theme_classic() +
	geom_point() + 
	#  geom_smooth(method='lm', se=FALSE, colour="grey") + 
  	geom_line() + 
  	scale_y_continuous(trans = 'log10') + 
  	annotation_logticks(sides="l") + 
	ggtitle("Nombre total de morts (échelle logarithmique)")+
  	labs(y="", x="Date", col="Pays",
	caption=paste("données mises à jour le",aujourdhui))+ 
	#caption=paste("données mises à jour le",hier))+ 
  	scale_colour_manual(values=mapalette)
ggsave("1.logdeaths.png")

# without log scale
ggplot(data=subset(toto, cumulDeaths>10)) + aes(y=cumulDeaths, x=dateRep, col=Country) +
	geom_point() + geom_line() + theme_classic() +
	ggtitle("Nombre total de morts")+
	labs(y="", x="Date", col="Pays",
	caption=paste("données mises à jour le",aujourdhui))+ 
	#caption=paste("données mises à jour le",hier))+ 
  	#scale_colour_brewer(palette="Set1")
  	scale_colour_manual(values=mapalette)
ggsave("3.deaths.png")

############
# Deaths per million habitant (popData2018)

x11()
toto$cumulDeaths_per_million = toto$cumulDeaths/toto$popData2018*1000000

ggplot(data=subset(toto, cumulDeaths_per_million>1)) + aes(y=cumulDeaths_per_million, x=dateRep, col=Country) +geom_point() + geom_line() + theme_classic() +
	scale_y_continuous(trans = 'log10') + 
	annotation_logticks(sides="l") +
	ggtitle("Nombre total de morts par million d'habitant (échelle logarithmique)")+
	labs(y="", x="Date", col="Pays",
	caption=paste("données mises à jour le",aujourdhui))+ 
	#caption=paste("données mises à jour le",hier))+ 
  	#scale_colour_brewer(palette="Set1")
  	scale_colour_manual(values=mapalette)
ggsave("2.logdeaths_per_capita.png")

# nb : compared to the previous plot, cf
# https://twitter.com/JonMinton/status/1239832173450010624 : 

#rate = n/N
#log(rate) = log(n/N) = log(n) - log(N)
#So the denom only shifts the intercept, when at the moment what's of interest is the gradient and trend.

#avec n=nombre de morts et N=nombre d'habitants
#log(n/N) = log(n) - log(N)
#donc la taille de la population ne change que l'ordonnée à l'origine de la droite

# without log scale
ggplot(data=subset(toto, cumulDeaths_per_million>1)) + aes(y=cumulDeaths_per_million, x=dateRep, col=Country) +
	geom_point() + geom_line() + theme_classic() +
	ggtitle("Nombre total de morts par million d'habitant")+ 
	labs(y="", x="Date", col="Pays",
		caption=paste("données mises à jour le",aujourdhui))+
  	scale_colour_manual(values=mapalette)
ggsave("4.deaths_per_capita.png")

#############################
# daily increase, mean on 7 last days

toto$meandeaths7days = rep(NA,nrow(toto))

mon_lissage=function(pays){
tata=subset(toto,Country==pays)
for (i in tata$dateRep[8:nrow(tata)]) tata$meandeaths7days[tata$dateRep==i] = (tata$cumulDeaths[tata$dateRep==i]-tata$cumulDeaths[tata$dateRep==(i-7)])/7
return(tata$meandeaths7days)
}
mon_lissage(pays="France")

for (j in levels(toto$Country)) toto$meandeaths7days[toto$Country==j] = mon_lissage(pays=j)

ggplot(data=subset(toto, meandeaths7days>10)) + aes(y=meandeaths7days, x=dateRep, col=Country) + theme_classic() +
	#geom_point() + 
	geom_line(size=1.2) +
	scale_y_continuous(trans = 'log10') + 
	annotation_logticks(sides="l") +
	ggtitle("Augmentation quotidienne du nombre de morts (moyenne sur les 7 derniers jours)") +
	labs(subtitle ="échelle logarithmique", y="", col="Pays",caption=paste("données mises à jour le",aujourdhui))+
  	scale_colour_manual(values=mapalette)

ggplot(data=subset(toto, meandeaths7days>10)) + aes(y=meandeaths7days, x=dateRep, col=Country) + theme_classic() +
	#geom_point() + 
	geom_line(size=1.2) +
	ggtitle("Augmentation quotidienne du nombre de morts (moyenne sur les 7 derniers jours)") +
	labs(y="", col="Pays",caption=paste("données mises à jour le",aujourdhui))+
  	scale_colour_manual(values=mapalette)



#############################
# Cases

ggplot(data=subset(toto, cumulCases>100), aes(y=cumulCases, x=dateRep, col=Country)) + theme_classic() +
  geom_point() + geom_line() + 
  scale_y_continuous(trans = 'log10') +
  annotation_logticks(sides="l") + 
  labs(y="Nombre total de cas déclarés (échelle logarithmique)", x="Date", col="Pays")+ 
  scale_colour_brewer(palette="Set1")

#############################
# daily increase in cone country : geom_col plot

ggplot(data=subset(data, dateRep>"2020-03-08" & Country=="France")) + theme_classic() +
	aes(y=cases, x=dateRep) + geom_col(fill="royalblue") +
	ggtitle("Augmentation quotidienne du nombre de cas COVID-19 en France") +
	labs(y="", caption=paste("données mises à jour le",hier))+ 
	annotate("text", x = dateconfinement , y = 2500, label = "Début du confinement", angle = 90)

ggplot(data=subset(data, dateRep>"2020-03-08" & Country=="France")) + theme_classic() +
	aes(y=deaths, x=dateRep) + geom_col(fill="darkorchid") +
	ggtitle("Augmentation quotidienne du nombre de décès dus au COVID-19 en France") +
	labs(y="", caption=paste("données mises à jour le",hier))+ 
	annotate("text", x = dateconfinement , y = 120, label = "Début du confinement", angle = 90)


ggplot(data=subset(data, dateRep>"2020-03-08" & Country=="Australia")) + theme_classic() +
	aes(y=cases, x=dateRep) + geom_col(fill="royalblue") +
	labs(caption=paste("données mises à jour le",hier))

ggplot(data=subset(data, dateRep>"2020-03-08" & Country=="Italy")) + theme_classic() +
	aes(y=deaths, x=dateRep) + geom_col(fill="royalblue") +
	labs(caption=paste("données mises à jour le",hier))

ggplot(data=subset(data, dateRep>"2020-03-08" & Country=="Spain")) + theme_classic() +
	aes(y=deaths, x=dateRep) + geom_col(fill="royalblue") +
	labs(caption=paste("données mises à jour le",hier))



