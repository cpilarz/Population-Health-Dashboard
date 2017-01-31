# Untitled




```r
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
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
#Create subset of total life expectancy for each country
life.x <- subset(wdi, wdi$Indicator.Name==lifexp)

#merge with GDP & region data
mergedx <- merge(life.x, gdp2014, by.x="Country.Name",by.y="Country.or.Area")
merged1x <- merge(region, mergedx, by.x="Country.Code",by.y="Country.Code")

#Plot 2014 life expectancy versis GDP. 
ggplot(merged1x, aes(x=(Value),y=X2014)) + geom_point(aes(color=Region))
```

```
## Warning: Removed 13 rows containing missing values (geom_point).
```

![](exp2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#plot 2014 life expectancy versus GDP. Converted GDP to log scale because most points were
#bunched at the lower end of GDP scale.
p1 <- ggplot(merged1x, aes(x=log(Value),y=X2014)) + geom_point(aes(color=Region))
print(p1)
```

```
## Warning: Removed 13 rows containing missing values (geom_point).
```

![](exp2_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
#add trend line
p2 <- ggplot(merged1x, aes(x=log(Value),y=X2014)) + geom_smooth(aes(group=1), 
                                                                method='lm',
                                                                se=FALSE,
                                                                formula= y~ log(x),
                                                                color='red') +
  geom_point(aes(color=Region)) +
  ggtitle("2014 Life Expectancy versus Log(GDP)")+
  xlab("log(GDP per Capita)")+
  ylab("Life Expectancy in 2014") 
print(p2)
```

```
## Warning: Removed 13 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 13 rows containing missing values (geom_point).
```

![](exp2_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
# Label some countries
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")  
#Label some points
ggplot(merged1x, aes(x=log(Value),y=X2014)) + geom_smooth(aes(group=1), 
                                                          method='lm',
                                                          se=FALSE,
                                                          formula= y~ log(x),
                                                          color='red') +
  geom_point(aes(color=Region)) +
  ggtitle("2014 Life Expectancy versus Log(GDP)")+
  xlab("log(GDP per Capita)")+
  ylab("Life Expectancy in 2014") +
    geom_text(aes(label = Country.Name),
              color = "gray20",
              data = subset(merged1x, Country.Name %in% pointsToLabel))
```

```
## Warning: Removed 13 rows containing non-finite values (stat_smooth).

## Warning: Removed 13 rows containing missing values (geom_point).
```

![](exp2_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
#Labels are overlapping, need to alter
library("ggrepel")
```

```
## Warning: package 'ggrepel' was built under R version 3.3.2
```

```r
ggplot(merged1x, aes(x=log(Value),y=X2014)) + geom_smooth(aes(group=1), 
                                                          method='lm',
                                                          se=FALSE,
                                                          formula= y~ log(x),
                                                          color='red') +
  geom_point(aes(color=Region)) +
  ggtitle("2014 Life Expectancy versus Log(GDP)")+
  xlab("log(GDP per Capita)")+
  ylab("Life Expectancy in 2014") +
    geom_text_repel(aes(label = Country.Name),
                    color = "gray20",
                    data = subset(merged1x, Country.Name %in% pointsToLabel),
                    force=10)
```

```
## Warning: Removed 13 rows containing non-finite values (stat_smooth).

## Warning: Removed 13 rows containing missing values (geom_point).
```

![](exp2_files/figure-html/unnamed-chunk-1-5.png)<!-- -->

```r
#What is the highest life expectancy? 
merged1x[which.max(merged1x$X2014),'X2014']
```

```
## [1] 83.5878
```

```r
#Where is it?
merged1x[80,"Country.Name"] #It's Japan
```

```
## [1] Japan
## 264 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe
```

```r
#What is the lowest?
merged1x[which.min(merged1x$X2014),'X2014']
```

```
## [1] 48.93473
```

```r
#Where is it?
merged1x[153,'Country.Name'] #It's Swaziland
```

```
## [1] Swaziland
## 264 Levels: Afghanistan Albania Algeria American Samoa Andorra ... Zimbabwe
```


