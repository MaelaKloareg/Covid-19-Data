# charts of total covid-19 cases & deaths evolution 
# The data is downloaded from : 
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

# these libraries are necessary

library(readxl)
library(httr)
library(ggplot2)

aujourdhui=format(Sys.Date(),"%d/%m%/%Y")
hier=format(Sys.Date()-1,"%d/%m%/%Y")
dateconfinement = as.POSIXct(as.Date("2020-03-17"))

# create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#url = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-26.xlsx"

# download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

# read the Dataset sheet into �R�

data <- read_excel(tf)

summary(data)


###

names(data)[7]="Country"

data$Country=as.factor(data$Country)

data=data[order(data$dateRep),]
data=data[order(data$Country),]

data$cumulCases = unlist(tapply(data$cases, data$Country, cumsum))
data$cumulDeaths = unlist(tapply(data$deaths, data$Country, cumsum))

data$cumulCases[data$cumulCases==0]=NA
data$cumulDeaths[data$cumulDeaths==0]=NA

subset(data, Country%in%c("France","Italy","Spain","United_Kingdom", "United_States_of_America")&dateRep>"2020-03-25 UTC")

# sort countries by total number of deaths
sort(tapply(data$cumulDeaths, data$Country, max, na.rm=TRUE), decreasing = TRUE)[1:20]

# select the 12 countries with the more deaths (for example)
#toto=droplevels(subset(data, Country%in%
#	names(sort(tapply(data$cumulDeaths, data$Country, max, na.rm=TRUE), decreasing = TRUE)[1:12])))

# my selection of countries
toto=droplevels(subset(data, 
	Country%in%c("France","Germany","Italy","Spain","United_Kingdom", 
	"Netherlands", "Switzerland", "Belgium", "United_States_of_America")
	&dateRep>"2020-02-24 UTC"))

# optional french translation
levels(toto$Country)=c("Belgique","France", "Allemagne","Italie", "Pays-Bas", "Espagne", "Suisse", "Royaume-Uni", "Etats-Unis")
summary(toto)


###
# Deaths

# sort countries by total number of deaths to define a new order for the levels (alphabetical order otherwise)
sort(tapply(toto$cumulDeaths, toto$Country, max, na.rm=TRUE), decreasing = TRUE)
CountryNewLevels=names(sort(tapply(toto$cumulDeaths, toto$Country, max, na.rm=TRUE), decreasing = TRUE))
toto$Country=factor(toto$Country,CountryNewLevels)

ggplot(data=toto) + aes(y=cumulDeaths, x=dateRep, col=Country) +geom_point() + geom_line() + theme_classic() +
	scale_y_continuous(trans = 'log10') + 
	annotation_logticks(sides="l") + 
	labs(y="Nombre total de morts (�chelle logarithmique)", x="Date", col="Pays")+ 
  scale_colour_brewer(palette="Set1")


# same for total number of deaths > 10

ggplot(data=subset(toto, cumulDeaths>10)) + aes(y=cumulDeaths, x=dateRep, group=Country, col=Country) +theme_classic() +
  geom_point() + 
#  geom_smooth(method='lm', se=FALSE, colour="grey") + 
  geom_line() + 
  scale_y_continuous(trans = 'log10') + 
  annotation_logticks(sides="l") + 
  labs(y="Nombre total de morts (�chelle logarithmique)", x="Date", col="Pays")+ 
  scale_colour_brewer(palette="Set1")


# without log scale
ggplot(data=tutu) + aes(y=cumulDeaths, x=dateRep, col=Country) +geom_point() + geom_line() + theme_classic() +
	labs(y="Nombre total de morts", x="Date", col="Pays")+ 
  scale_colour_brewer(palette="Set1")

###
# Cases

ggplot(data=subset(toto, cumulCases>100), aes(y=cumulCases, x=dateRep, col=Country)) + theme_classic() +
  geom_point() + geom_line() + 
  scale_y_continuous(trans = 'log10') +
  annotation_logticks(sides="l") + 
  labs(y="Nombre total de cas d�clar�s (�chelle logarithmique)", x="Date", col="Pays")+ 
  scale_colour_brewer(palette="Set1")


###
# Deaths per million habitant (popData2018)

toto$cumulDeaths_per_million = toto$cumulDeaths/toto$popData2018*1000000

ggplot(data=subset(toto, cumulDeaths_per_million>1)) + aes(y=cumulDeaths_per_million, x=dateRep, col=Country) +geom_point() + geom_line() + theme_classic() +
	scale_y_continuous(trans = 'log10') + 
	annotation_logticks(sides="l") + 
	labs(y="Nombre total de morts par million d'habitant (�chelle logarithmique)", x="Date", col="Pays")+ 
  scale_colour_brewer(palette="Set1")


# nb : compared to the previous plot, cf
# https://twitter.com/JonMinton/status/1239832173450010624 : 

#rate = n/N
#log(rate) = log(n/N) = log(n) - log(N)
#So the denom only shifts the intercept, when at the moment what's of interest is the gradient and trend.


#############################
# daily increase in France

ggplot(data=subset(toto, dateRep>"2020-03-08 01:00:00 CET" & Country=="France")) + theme_classic() +
	aes(y=cases, x=dateRep) + geom_col(fill="royalblue") +
	ggtitle("Augmentation quotidienne du nombre de cas COVID-19 en France") +
	labs(y="", caption=paste("donn�es mises � jour le",hier))+ 
	annotate("text", x = dateconfinement , y = 2500, label = "D�but du confinement", angle = 90)

ggplot(data=subset(toto, dateRep>"2020-03-08 01:00:00 CET" & Country=="France")) + theme_classic() +
	aes(y=deaths, x=dateRep) + geom_col(fill="darkorchid") +
	ggtitle("Augmentation quotidienne du nombre de d�c�s dus au COVID-19 en France") +
	labs(y="", caption=paste("donn�es mises � jour le",hier))+ 
	annotate("text", x = dateconfinement , y = 120, label = "D�but du confinement", angle = 90)


