library(httr)
library(ggplot2)
library(reshape) # melt() et dcast()

#https://www.data.gouv.fr/fr/datasets/cas-confirmes-dinfection-au-covid-19-par-region/

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
