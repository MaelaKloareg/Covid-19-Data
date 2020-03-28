library(httr)
library(ggplot2)
library(reshape) # melt() et dcast()

setwd("D:/Kuzulia/Docs_stat/2020_covid")

#https://www.data.gouv.fr/fr/datasets/cas-confirmes-dinfection-au-covid-19-par-region/

# https://github.com/bzg/covid19-fr/blob/master/data/covid19.csv

# create the URL where the dataset is stored with automatic updates every day

url <- "https://raw.githubusercontent.com/bzg/covid19-fr/master/data/covid19.csv"

# download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

# read the Dataset sheet into “R”

france <- read.csv(tf)
summary(france)

#############################
# france

france=read.csv("covid19.france.csv")
summary(france)

france=melt(france)
france$Date=as.Date(france$Date, "%Y/%m/%d")
names(france)[2:3]=c("region","cas")
summary(france)

# sort regions by total number of case to define a new order for the levels (alphabetical order otherwise)
sort(tapply(france$cas, france$region, max, na.rm=TRUE), decreasing = TRUE)
regionNewLevels=names(sort(tapply(france$cas, france$region, max, na.rm=TRUE), decreasing = TRUE))
france$region=factor(france$region,regionNewLevels)

# log scale, for total number of cases > 11
france= subset(france, cas>11)

ggplot(data=france) + aes(y=cas, x=Date, group=region, col=region) +theme_classic() +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +  
  scale_y_continuous(trans = 'log10') + 
  annotation_logticks(sides="l") + 
  labs(y="Nombre total de cas (échelle logarithmique)", x="Date", col="Région",
	caption="données mises à jour le 25/03/2020")

# without log scale
ggplot(data=france) + aes(y=cas, x=Date, group=region, col=region) +theme_classic() +
  geom_point() + 
  geom_line() + 
  labs(y="Nombre total de cas", x="Date", col="Région",
	caption="données mises à jour le 25/03/2020")
