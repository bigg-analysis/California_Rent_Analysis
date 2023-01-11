##########################################################
#######California Rent Project Code IST 687 Team 2########
##########################################################


library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)

#Make sure to put in your own path to read the csv file

cali_file <- read.csv("C:/Users/bliun/Desktop/Cali-File.csv")
cityStats <- read_excel("More.xlsx")
colnames(cityStats)[9] <- "AvgPop"
glimpse(cali_file)


#List of Unique Cities (This is Important as each city is recorded within each year timeframe)
cityList <- unique(cali_file$city)
cityList <- sort(cityList)

#Color and Number Lists for Cities
dummyCityList <- c(0:64)
colorList <- c(1:65)
##Cleaned Info##
# ------------------------- #

#Information by Year
cali_2003 <- cali_file[cali_file$year == 2003,]
cali_2008 <- cali_file[cali_file$year == 2008,]
cali_2013 <- cali_file[cali_file$year == 2013,]
cali_2018 <- cali_file[cali_file$year == 2018,]

#Important Means
#Means By Year
meanYear <- aggregate(cali_file$price, by=list(cali_file$year), FUN=mean)
colnames(meanYear) <- c("Year", "MeanRentPrice")
meanYear$`MeanRentPrice` <- round(meanYear$`MeanRentPrice`, digits = 2)
meanYear

#Means By Neighborhood
meanNhood <- aggregate(cali_file$price, by=list(cali_file$nhood), FUN=mean)
colnames(meanNhood) <- c("Neighborhood", "MeanRentPrice")
meanNhood$MeanRentPrice <= round(meanNhood$MeanRentPrice, digits = 2)
meanNhood

#Means by County
meanCounty <- aggregate(cali_file$price, by=list(cali_file$county), FUN=mean)
colnames(meanNhood) <- c("County", "MeanRentPrice")
meanCounty

#Means by City
meanCities <- aggregate(cali_file$price, by=list(cali_file$city), FUN=mean)
colnames(meanCities) <- c("City", "MeanRentPrice")
meanCities$MeanRentPrice <- round(meanCities$MeanRentPrice, digits = 2)
meanCities

#Means by City & Year
meanCities2003 <- aggregate(cali_2003$price, by=list(cali_2003$city), FUN=mean)
colnames(meanCities2003) <- c("City", "MeanRentPrice2003")
meanCities2003$MeanRentPrice2003 <- round(meanCities2003$MeanRentPrice2003, digits = 2)
meanCities2003

meanCities2008 <- aggregate(cali_2008$price, by=list(cali_2008$city), FUN=mean)
colnames(meanCities2008) <- c("City", "MeanRentPrice2008")
meanCities2008$MeanRentPrice2008 <- round(meanCities2008$MeanRentPrice2008, digits = 2)
meanCities2008

meanCities2013 <- aggregate(cali_2013$price, by=list(cali_2013$city), FUN=mean)
colnames(meanCities2013) <- c("City", "MeanRentPrice2013")
meanCities2013$MeanRentPrice2013 <- round(meanCities2013$MeanRentPrice2013, digits = 2)
meanCities2013

meanCities2018 <- aggregate(cali_2018$price, by=list(cali_2018$city), FUN=mean)
colnames(meanCities2018) <- c("City", "MeanRentPrice2018")
meanCities2018$MeanRentPrice2018 <- round(meanCities2018$MeanRentPrice2018, digits = 2)
meanCities2018

#Means By County & Year
meanCounty2003 <- aggregate(cali_2003$price, by=list(cali_2003$county), FUN=mean)
colnames(meanNhood) <- c("County", "MeanRentPrice")
meanCounty

meanCounty2008 <- aggregate(cali_2008$price, by=list(cali_2008$county), FUN=mean)
colnames(meanNhood) <- c("County", "MeanRentPrice")
meanCounty

meanCounty2013 <- aggregate(cali_2013$price, by=list(cali_2013$county), FUN=mean)
colnames(meanNhood) <- c("County", "MeanRentPrice")
meanCounty

meanCounty2018 <- aggregate(cali_2018$price, by=list(cali_2018$county), FUN=mean)
colnames(meanNhood) <- c("County", "MeanRentPrice")
meanCounty

#Histogram of California Rent Prices across 2003 - 2018
cali_file_hist <- ggplot(cali_file) + geom_histogram(aes(x = price), fill = "white", color = "black", binwidth = 200)
cali_file_hist <- cali_file_hist + ggtitle("California Price Rent Histogram")

#Line Chart for Average Rent Price by Recorded Year
meanYear_line <- ggplot(meanYear) + aes(x = Year, y = MeanRentPrice) + geom_line(color = "blue") + geom_point(color = "black")
meanYear_line <- meanYear_line + ggtitle("Average Rent Price By Recorded Year")

#Dataframe Combining the means by year
meanCityYear <- data.frame(cityList, meanCities2003$MeanRentPrice2003, meanCities2008$MeanRentPrice2008)
meanCityYear <- data.frame(cityDF, meanCities2013$MeanRentPrice2013, meanCities2018$MeanRentPrice2018)
colnames(meanCityYear) <- c("city", "2003", "2008", "2013", "2018")

#Same data from cityDF but in different format to create a line plot
yearList <- c(2003, 2008, 2013, 2018)
yearDF <- as.data.frame(t(cityDF))
yearDF <- yearDF[-1,]
yearDF <- data.frame(yearList, yearDF)
colnames(yearDF) <- c("Year", cityList)
yearDF <- yearDF %>% mutate_if(is.character, as.numeric)
yearDF <- yearDF %>% rename_with(make.names)

cityLine <- ggplot(yearDF, color = city) + ggtitle("California Average Rent 2003 - 2018") + 
  geom_line(aes(x = Year, y = alameda), color = "#516dfc") + geom_point(aes(x = Year, y = alameda), color = "black") + 
  geom_line(aes(x = Year, y = belmont), color = "#ff406c") + geom_point(aes(x = Year, y = belmont), color = "black") +
  geom_line(aes(x = Year, y = belvedere), color = "#00b359") + geom_point(aes(x = Year, y = belvedere), color = "black") +
  geom_line(aes(x = Year, y = berkeley), color = "#b37100") + geom_point(aes(x = Year, y = berkeley), color = "black") +
  geom_line(aes(x = Year, y = brentwood), color = "#a6008d") + geom_point(aes(x = Year, y = brentwood), color = "black") +
  geom_line(aes(x = Year, y = brisbane), color = "#05e6e2") + geom_point(aes(x = Year, y = brisbane), color = "black") +
  geom_line(aes(x = Year, y = burlingame), color = "#fbff00") + geom_point(aes(x = Year, y = burlingame), color = "black") +
  geom_line(aes(x = Year, y = cambrian), color = "#b46eff") + geom_point(aes(x = Year, y = cambrian), color = "black") +
  geom_line(aes(x = Year, y = campbell), color = "#ed602d") + geom_point(aes(x = Year, y = campbell), color = "black") +
  geom_line(aes(x = Year, y = concord), color = "#44ab30") + geom_point(aes(x = Year, y = concord), color = "black") +
  geom_line(aes(x = Year, y = corralitos), color = "#df86eb") + geom_point(aes(x = Year, y = corralitos), color = "black") +
  geom_line(aes(x = Year, y = corte.madera), color = "#fc8d32") + geom_point(aes(x = Year, y = corte.madera), color = "black") + 
  geom_line(aes(x = Year, y = cupertino), color = "#a7a8b0") + geom_point(aes(x = Year, y = cupertino), color = "black") + 
  geom_line(aes(x = Year, y = daly.city), color = "#fffd87") + geom_point(aes(x = Year, y = daly.city), color = "black") + 
  geom_line(aes(x = Year, y = dublin), color = "#d494f2") + geom_point(aes(x = Year, y = dublin), color = "black") + 
  geom_line(aes(x = Year, y = el.cerrito), color = "#ff474e") + geom_point(aes(x = Year, y = el.cerrito), color = "black") + 
  geom_line(aes(x = Year, y = el.sobrante), color = "#3bbf53") + geom_point(aes(x = Year, y = el.sobrante), color = "black") + 
  geom_line(aes(x = Year, y = emeryville), color = "#cf6ff7") + geom_point(aes(x = Year, y = emeryville), color = "black") + 
  geom_line(aes(x = Year, y = fairfield), color = "#92a33b") + geom_point(aes(x = Year, y = fairfield), color = "black") + 
  geom_line(aes(x = Year, y = foster.city), color = "#9cb1ff") + geom_point(aes(x = Year, y = foster.city), color = "black") + 
  geom_line(aes(x = Year, y = gilroy), color = "#9c097e") + geom_point(aes(x = Year, y = gilroy), color = "black") + 
  geom_line(aes(x = Year, y = hayward), color = "#646eb5") + geom_point(aes(x = Year, y = hayward), color = "black") + 
  geom_line(aes(x = Year, y = healdsburg), color = "#ab2c05") + geom_point(aes(x = Year, y = healdsburg), color = "black") + 
  geom_line(aes(x = Year, y = larkspur), color = "#000000") + geom_point(aes(x = Year, y = larkspur), color = "black") + 
  geom_line(aes(x = Year, y = los.altos), color = "#647517") + geom_point(aes(x= Year, y = los.altos), color = "black") +
  geom_line(aes(x = Year, y = los.gatos), color = "#d18484") + geom_point(aes(x= Year, y = los.gatos), color = "black") + 
  geom_line(aes(x = Year, y = marin), color = "#8cb584") + geom_point(aes(x = Year, y = marin), color = "black") + 
  geom_line(aes(x = Year, y = menlo.park), color = "#2aabad") + geom_point(aes(x = Year, y = menlo.park), color = "black") + 
  geom_line(aes(x = Year, y = mill.valley), color = "#7c4e99") + geom_point(aes(x = Year, y = mill.valley), color = "black") + 
  geom_line(aes(x = Year, y = millbrae), color = "#fae48c") + geom_point(aes(x = Year, y = millbrae), color = "black") + 
  geom_line(aes(x = Year, y = milpitas), color = "#02755e") + geom_point(aes(x = Year, y = milpitas), color = "black") + 
  geom_line(aes(x = Year, y = mountain.view), color = "#804025") + geom_point(aes(x = Year, y = mountain.view), color = "black") +
  geom_line(aes(x = Year, y = napa.county), color = "#a6e0ff") + geom_point(aes(x = Year, y = napa.county), color = "black") + 
  geom_line(aes(x = Year, y = novato), color = "#b186e3") + geom_point(aes(x = Year, y = novato), color = "black") + 
  geom_line(aes(x = Year, y = oakland), color = "#dbb21d") + geom_point(aes(x = Year, y = oakland), color = "black") + 
  geom_line(aes(x = Year, y = orinda), color = "#1d7a3b") + geom_point(aes(x = Year, y = orinda), color = "black") + 
  geom_line(aes(x = Year, y = pacifica), color = "#b7a1ff") + geom_point(aes(x = Year, y = pacifica), color = "black") + 
  geom_line(aes(x = Year, y = palo.alto), color = "#7d0137") + geom_point(aes(x = Year, y = palo.alto), color = "black") +
  geom_line(aes(x = Year, y = petaluma), color = "#992f2f") + geom_point(aes(x = Year, y = petaluma), color = "black") + 
  geom_line(aes(x = Year, y = pittsburg), color = "#226e1b") + geom_point(aes(x = Year, y = pittsburg), color = "black") + 
  geom_line(aes(x = Year, y = redwood.city), color = "#628996") + geom_point(aes(x = Year, y = redwood.city), color = "black") + 
  geom_line(aes(x = Year, y = redwood.shores), color = "#fc8814") + geom_point(aes(x = Year, y = redwood.shores), color = "black") + 
  geom_line(aes(x = Year, y = richmond), color = "#688500") + geom_point(aes(x = Year, y = richmond), color = "black") +
  geom_line(aes(x = Year, y = rohnert.park), color = "#5c441f") + geom_point(aes(x = Year, y = rohnert.park), color = "black") + 
  geom_line(aes(x = Year, y = russian.river), color = "#90f5a5") + geom_point(aes(x = Year, y = russian.river), color = "black") + 
  geom_line(aes(x = Year, y = san.anselmo), color = "#d6a3ff") + geom_point(aes(x = Year, y = san.anselmo), color = "black") + 
  geom_line(aes(x = Year, y = san.bruno), color = "#f57367") + geom_point(aes(x = Year, y = san.bruno), color = "black") + 
  geom_line(aes(x = Year, y = san.francisco), color = "#d67a2f") + geom_point(aes(x = Year, y = san.francisco), color = "black") +
  geom_line(aes(x = Year, y = san.jose), color = "#b580d9") + geom_point(aes(x = Year, y = san.jose), color = "black") + 
  geom_line(aes(x = Year, y = san.leandro), color = "#85715d") + geom_point(aes(x = Year, y = san.leandro), color = "black") + 
  geom_line(aes(x = Year, y = san.mateo), color = "#a36a00") + geom_point(aes(x = Year, y = san.mateo), color = "black") + 
  geom_line(aes(x = Year, y = san.rafael), color = "#5eccc8") + geom_point(aes(x = Year, y = san.rafael), color = "black") + 
  geom_line(aes(x = Year, y = san.ramon), color = "#b155f2") + geom_point(aes(x = Year, y = san.ramon), color = "black") + 
  geom_line(aes(x = Year, y = santa.clara), color = "#e1eb5b") + geom_point(aes(x = Year, y = santa.clara), color = "black") + 
  geom_line(aes(x = Year, y = santa.cruz), color = "#206e52") + geom_point(aes(x = Year, y = santa.cruz), color = "black") + 
  geom_line(aes(x = Year, y = santa.rosa), color = "#6f6599") + geom_point(aes(x = Year, y = santa.rosa), color = "black") + 
  geom_line(aes(x = Year, y = saratoga), color = "#fc00f8") + geom_point(aes(x = Year, y = saratoga), color = "black") + 
  geom_line(aes(x = Year, y = sausalito), color = "#629c36") + geom_point(aes(x = Year, y = sausalito), color = "black") + 
  geom_line(aes(x = Year, y = sonoma), color = "#94592c") + geom_point(aes(x = Year, y = sonoma), color = "black") + 
  geom_line(aes(x = Year, y = soquel), color = "#787878") + geom_point(aes(x = Year, y = soquel), color = "black") + 
  geom_line(aes(x = Year, y = sunnyvale), color = "#2129ff") + geom_point(aes(x = Year, y = sunnyvale), color = "black") + 
  geom_line(aes(x = Year, y = union.city), color = "#b53e57") + geom_point(aes(x  = Year, y = union.city), color = "black") + 
  geom_line(aes(x = Year, y = vallejo), color = "#005719") + geom_point(aes(x = Year, y = vallejo), color = "black") + 
  geom_line(aes(x = Year, y = walnut.creek), color = "#ab5b30") + geom_point(aes(x = Year, y = walnut.creek), color = "black") + 
  geom_line(aes(x = Year, y = woodside), color = "#5eabcc") + geom_point(aes(x = Year, y = woodside), color = "black")

#Means by City & Year Boxplots
meanCities_boxplot <- ggplot(meanCitiesNew) + geom_boxplot(aes(x = overall.mean), color = "black", fill = "#9cfff2") + 
  ggtitle("California Average Rent Boxplot")
meanCities2003_boxplot <- ggplot(meanCitiesNew) + geom_boxplot(aes(x = mean.2003), color = "black", fill = "#c7ffb3") + 
  ggtitle("2003 California Average Rent Boxplot")
meanCities2008_boxplot <- ggplot(meanCitiesNew) + geom_boxplot(aes(x = mean.2008), color = "black", fill = "#fff385") +
  ggtitle("2008 California Average Rent Boxplot")
meanCities2013_boxplot <- ggplot(meanCitiesNew) + geom_boxplot(aes(x = mean.2013), color = "black", fill = "#ff9a52") + 
  ggtitle("2013 California Average Rent Boxplot")
meanCities2018_boxplot <- ggplot(meanCitiesNew) + geom_boxplot(aes(x = mean.2018), color = "black", fill = "#ffa6f2") +
  ggtitle("2018 California Average Rent Boxplot")

#Mean Rent Price by City Plot
city_plot <- ggplot(meanCities, aes(x = City, y = MeanRentPrice, color = City, label = MeanRentPrice)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ggtitle("Mean Rent Price by City") + 
  geom_text(hjust = 0.3, vjust = -0.5)
city_plot

#Average Population vs Mean Price by City
cityStats <- cbind(cityStats, meanCities$MeanRentPrice)
colnames(cityStats)[10] <- "MeanRentPrice"
cityStatsPlot <- ggplot(cityStats, aes(x = AvgPop, y = MeanRentPrice, label = City, color = City)) 
cityStatsPlot <- cityStatsPlot + geom_point() + geom_text(hjust = 0, vjust = 0)
cityStatsPlot <- cityStatsPlot + coord_cartesian() + ggtitle("AvgPop vs Mean Price by City")

#Linear Model Separation (Gets rid of extraneous columns to make linear modeling easier)
temp <- cali_file[-1:-6]
temp <- temp[-2:-11]
summary(lm(price~belmont+belvedere+berkeley+brentwood+brisbane+burlingame+cambrian+campbell+concord+
             corralitos+corte.madera+cupertino+daly.city+dublin+el.cerrito+el.sobrante+emeryville+
             fairfield+foster.city+gilroy+hayward+healdsburg+larkspur+los.altos+
             los.gatos+marin+menlo.park+mill.valley+millbrae+milpitas+mountain.view+
             napa.county+novato+oakland+orinda+pacifica+palo.alto+petaluma+pittsburg+
             redwood.city+redwood.shores+richmond+rohnert.park+russian.river+san.anselmo+
             san.bruno+san.francisco+san.jose+san.leandro+san.mateo+san.rafael+
             san.ramon+santa.clara+santa.cruz+santa.rosa+saratoga+sausalito+sonoma+soquel+sunnyvale+union.city+
             vallejo+walnut.creek+woodside,
           data=temp))

temp2003 <- cali_2003[-1:-6]
temp2003 <- temp2003[-2:-11]
summary(lm(price~belmont+belvedere+berkeley+brentwood+brisbane+burlingame+cambrian+campbell+concord+
             corralitos+corte.madera+cupertino+daly.city+dublin+el.cerrito+el.sobrante+emeryville+
             fairfield+foster.city+gilroy+hayward+healdsburg+larkspur+los.altos+
             los.gatos+marin+menlo.park+mill.valley+millbrae+milpitas+mountain.view+
             napa.county+novato+oakland+orinda+pacifica+palo.alto+petaluma+pittsburg+
             redwood.city+redwood.shores+richmond+rohnert.park+russian.river+san.anselmo+
             san.bruno+san.francisco+san.jose+san.leandro+san.mateo+san.rafael+
             san.ramon+santa.clara+santa.cruz+santa.rosa+saratoga+sausalito+sonoma+soquel+sunnyvale+union.city+
             vallejo+walnut.creek+woodside,
           data=temp2003))

temp2008 <- cali_2008[-1:-6]
temp2008 <- temp2008[-2:-11]
summary(lm(price~belmont+belvedere+berkeley+brentwood+brisbane+burlingame+cambrian+campbell+concord+
             corralitos+corte.madera+cupertino+daly.city+dublin+el.cerrito+el.sobrante+emeryville+
             fairfield+foster.city+gilroy+hayward+healdsburg+larkspur+los.altos+
             los.gatos+marin+menlo.park+mill.valley+millbrae+milpitas+mountain.view+
             napa.county+novato+oakland+orinda+pacifica+palo.alto+petaluma+pittsburg+
             redwood.city+redwood.shores+richmond+rohnert.park+russian.river+san.anselmo+
             san.bruno+san.francisco+san.jose+san.leandro+san.mateo+san.rafael+
             san.ramon+santa.clara+santa.cruz+santa.rosa+saratoga+sausalito+sonoma+soquel+sunnyvale+union.city+
             vallejo+walnut.creek+woodside,
           data=temp2008))

temp2013 <- cali_2013[-1:-6]
temp2013 <- temp2013[-2:-11]
summary(lm(price~belmont+belvedere+berkeley+brentwood+brisbane+burlingame+cambrian+campbell+concord+
             corralitos+corte.madera+cupertino+daly.city+dublin+el.cerrito+el.sobrante+emeryville+
             fairfield+foster.city+gilroy+hayward+healdsburg+larkspur+los.altos+
             los.gatos+marin+menlo.park+mill.valley+millbrae+milpitas+mountain.view+
             napa.county+novato+oakland+orinda+pacifica+palo.alto+petaluma+pittsburg+
             redwood.city+redwood.shores+richmond+rohnert.park+russian.river+san.anselmo+
             san.bruno+san.francisco+san.jose+san.leandro+san.mateo+san.rafael+
             san.ramon+santa.clara+santa.cruz+santa.rosa+saratoga+sausalito+sonoma+soquel+sunnyvale+union.city+
             vallejo+walnut.creek+woodside,
           data=temp2013))

temp2018 <- cali_2018[-1:-6]
temp2018 <- temp2018[-2:-11]
summary(lm(price~belmont+belvedere+berkeley+brentwood+brisbane+burlingame+cambrian+campbell+concord+
             corralitos+corte.madera+cupertino+daly.city+dublin+el.cerrito+el.sobrante+emeryville+
             fairfield+foster.city+gilroy+hayward+healdsburg+larkspur+los.altos+
             los.gatos+marin+menlo.park+mill.valley+millbrae+milpitas+mountain.view+
             napa.county+novato+oakland+orinda+pacifica+palo.alto+petaluma+pittsburg+
             redwood.city+redwood.shores+richmond+rohnert.park+russian.river+san.anselmo+
             san.bruno+san.francisco+san.jose+san.leandro+san.mateo+san.rafael+
             san.ramon+santa.clara+santa.cruz+santa.rosa+saratoga+sausalito+sonoma+soquel+sunnyvale+union.city+
             vallejo+walnut.creek+woodside,
           data=temp2018))