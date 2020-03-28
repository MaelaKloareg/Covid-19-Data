# charts of total covid-19 cases & deaths evolution 
# The data is downloaded from : 
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

# these libraries are necessary

library(readxl)
library(httr)
library(ggplot2)

# create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#url = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-26.xlsx"

# download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

# read the Dataset sheet into “R”

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
	"Netherlands", "Switzerland", "Belgium", "United_States_of_America",
	"Iran")
	&dateRep>"2020-02-24 UTC"))

# optional french translation
levels(toto$Country)=c("Belgique","France", "Allemagne", "Iran","Italie", "Pays-Bas", "Espagne", "Suisse", "Royaume-Uni", "Etats-Unis")
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
	labs(y="Nombre total de morts (échelle logarithmique)", x="Date", col="Pays")

# linear adjustment on log scale, for total number of deaths > 10
tutu = subset(toto, cumulDeaths>10)

ggplot(data=tutu) + aes(y=cumulDeaths, x=dateRep, group=Country, col=Country) +theme_classic() +
  geom_point() + 
#  geom_smooth(method='lm', se=FALSE, colour="grey") + 
  geom_line() + 
  scale_y_continuous(trans = 'log10') + 
  annotation_logticks(sides="l") + 
  labs(y="Nombre total de morts (échelle logarithmique)", x="Date", col="Pays")

# without log scale
ggplot(data=tutu) + aes(y=cumulDeaths, x=dateRep, col=Country) +geom_point() + geom_line() + theme_classic() +
	labs(y="Nombre total de morts", x="Date", col="Pays")
###
# Cases

ggplot(data=toto, aes(y=cumulCases, x=dateRep, col=Country)) + theme_classic() +
  geom_point() + geom_line() + 
  scale_y_continuous(trans = 'log10') +
  annotation_logticks(sides="l") + 
  labs(y="Nombre total de cas déclarés (échelle logarithmique)", x="Date", col="Pays")

###
# Deaths per million habitant (popData2018)

toto$cumulDeaths_per_million = toto$cumulDeaths/toto$popData2018*1000000

tata = subset(toto, cumulDeaths_per_million>1)
ggplot(data=tata) + aes(y=cumulDeaths_per_million, x=dateRep, col=Country) +geom_point() + geom_line() + theme_classic() +
	scale_y_continuous(trans = 'log10') + 
	annotation_logticks(sides="l") + 
	labs(y="Nombre total de morts par million d'habitant (échelle logarithmique)", x="Date", col="Pays")

# nb : compared to the previous plot, cf
# https://twitter.com/JonMinton/status/1239832173450010624 : 

#rate = n/N
#log(rate) = log(n/N) = log(n) - log(N)
#So the denom only shifts the intercept, when at the moment what's of interest is the gradient and trend.



