#### Population health project

#set working directory
setwd('~/UN Project/national global health risks')

wdi <- read.csv("API_8_DS2_en_csv_v2.csv", header=TRUE,sep="\t",stringsAsFactors = TRUE)
per.cap.gdp <- read.csv('~/UN Project/per capita GDP from UNdata.csv')

#subset 2014 values
gdp2014 <- subset(per.cap.gdp, per.cap.gdp$Year==2014)

#load population data
pop <- read.csv("population.csv",skip=4)
#load region data
region <- read.csv("Metadata_Country_API_8_DS2_en_csv_v2.csv")
colnames(region) <- c('Country.Code','Region','IncomeGroup','SpecialNotes','TableName')

#lets look at life expectancy
lifexp <- "Life expectancy at birth, total (years)"

library(ggplot2)

#Create subset of total life expectancy for each country
life.x <- subset(wdi, wdi$Indicator.Name==lifexp)

#merge with GDP & region data
mergedx <- merge(life.x, gdp2014, by.x="Country.Name",by.y="Country.or.Area")
merged1x <- merge(region, mergedx, by.x="Country.Code",by.y="Country.Code")

#Plot 2014 life expectancy versis GDP. 
ggplot(merged1x, aes(x=(Value),y=X2014)) + geom_point(aes(color=Region))

#plot 2014 life expectancy versus GDP. Converted GDP to log scale because most points were
#bunched at the lower end of GDP scale.
ggplot(merged1x, aes(x=log(Value),y=X2014)) + geom_point(aes(color=Region))

#add trend line
p1 <- ggplot(merged1x, aes(x=log(Value),y=X2014)) + geom_smooth(aes(group=1), 
                                                                method='lm',
                                                                se=FALSE,
                                                                formula= y~ log(x),
                                                                color='red') +
  geom_point(aes(color=Region)) +
  ggtitle("2014 Life Expectancy versus Log(GDP)")+
  xlab("log(GDP per Capita)")+
  ylab("Life Expectancy in 2014") 

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")  
#Label some points
(p2 <- p1 +
    geom_text(aes(label = Country.Name),
              color = "gray20",
              data = subset(merged1x, Country.Name %in% pointsToLabel)))
#Labels are overlapping, need to alter
library("ggrepel")
(p3 <- p1 + 
    geom_text_repel(aes(label = Country.Name),
                    color = "gray20",
                    data = subset(merged1x, Country.Name %in% pointsToLabel),
                    force=10))


#which has the highest life expectancy?
merged1x[which.max(merged1x$X2014),'X2014']
merged1x[80,"Country.Name"] #It's Japan
#which has the lowest?
merged1x[which.min(merged1x$X2014),'X2014']
merged1x[153,'Country.Name'] #It's Swaziland


