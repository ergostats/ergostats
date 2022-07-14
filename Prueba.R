
############################################################################################################ 

                                # Programming test

#By:  Andrea Díaz Sánchez

###########################################################################################################



#Set as working directory
setwd("C:/Users/FRANCISCO/Downloads/Prueba")



#Install all packages to be used

#Plot
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("gplots")

#Analysis data
install.packages("reshape")
install.packages("plm") 
#instal from RCAN library
install.packages("imputeTS")




###############################################################################
                                   #Questions
##############################################################################




#1.-
#Find the mean income going to the top 10% in each country


#To open csv file:
datos = read.csv("data_top10_income_shares.csv", sep="\t")
View(datos)
#Data has missing values (NA structures) 


#To summarize the variables and find missing values in data frame:
sum(is.na(datos$brazil))
##Have 10 missing data, 27% of missing values in the sample
sum(is.na(datos$china))
sum(is.na(datos$india))
##Have 18 missing data, 48.65% of missing alues in the sample
sum(is.na(datos$uscanada))
sum(is.na(datos$russia))
sum(is.na(datos$europe))


#To find mean in each country included missing values: 
mbrasil = mean(datos$brazil, na.rm=TRUE)
mchina = mean(datos$china)
mindia = mean(datos$india, na.rm=TRUE)
muscanada = mean(datos$uscanada)
mrussia = mean(datos$russia)
meurope = mean(datos$europe)









#2.-
#Impute missing data with the closest year of available data. 

library(imputeTS)
Newdata = na_locf(datos, option="nocb")
View(Newdata)
#Newdata is the new dataframe for the richest 10% of households with imputation data.









#3.- 
#Plot variables of each country
library(ggplot2)
library(ggpubr)


#Item a:

#Brazil
f1 = ggplot() +
     geom_line(aes(x = Newdata$Year, y = Newdata$brazil), colour = "blue")+
     xlab("Years") +
     ylab("Income(in million dollars)")
plot(f1)


#China
f2 = ggplot() +
     geom_line(aes(x = Newdata$Year, y = Newdata$china), colour = "purple")+
     xlab("Years") +
     ylab("Income(in million dollars)")
plot(f2)

#Europe
f3 = ggplot() +
     geom_line(aes(x = Newdata$Year, y = Newdata$europe), colour = "maroon")+
     xlab("Years") +
     ylab("Income(in million dollars)")
plot(f3)


#India
f4 = ggplot() +
     geom_line(aes(x = Newdata$Year, y = Newdata$india), colour = "olive drab")+
     xlab("Years") +
     ylab("Income(in million dollars)")
plot(f4)

#Russia
f5 = ggplot() +
     geom_line(aes(x = Newdata$Year, y = Newdata$russia), colour = "chocolate")+
     xlab("Years") +
     ylab("Income(in million dollars)")
plot(f5)

#US-Canada
f6 = ggplot()+
     geom_line(aes(x = Newdata$Year, y = Newdata$uscanada), colour = "turquoise")+
     xlab("Years") +
     ylab("Income(in million dollars)")
plot(f6)


#To combine all graphs into one figure:
require(ggpubr)
ggpubr = ggarrange(f1,f2,f3,f4,f5,f6, labels=c("Brazil", "China", "Europe", "India","Russia", "US-Canada"))
plot(ggpubr)




#Item b:
#Take care of the axis and titles as to make the figures comparable and the combined figure suitable for presentation.
ggplot(data = Newdata, aes(x = Year)) +
  geom_line(aes(y = brazil, colour = "Brazil")) +
  geom_line(aes(y = china, colour = "China")) +
  geom_line(aes(y = europe, colour = "Europe")) +
  geom_line(aes(y = india, colour = "India")) +
  geom_line(aes(y = russia, colour = "Russia")) +
  geom_line(aes(y = uscanada, colour = "US_Canada")) +
  labs(title= "Top 10% income shares",
       x = "Years",
       y = "Income(in million dollars)",
       colour = "Country")








#6.-
#Reshape the data to a long format 


#Colums are ordered alphabetically in the dataframe
Newdata = Newdata[,c(1,2,3,7,4,6,5)]
View(Newdata)


#The factor in this case is country
Newdata$Year = as.factor(Newdata$Year)

#To reshape the data in long format use:
library(tidyr)
data_long = gather(Newdata, country, topshares,brazil:uscanada, factor_key = TRUE)
#to order the dataframe by years, use:
data_long = data_long[order(data_long$Year),]
#to round the numbers in the variable topshares, use:
round(data_long$topshares,1)
View(data_long)


#Plot of the data in long  format
#Obtain the figure in another window
x11()
#To graph the data in long format, use:
pl1 = ggplot(data = data_long)
pl1 = pl1 + geom_bar(aes(x = Year, y = topshares, fill = country), stat = "identity")+
  labs(title = "Top 10% income shares",
       y = "Incomes")
plot(pl1)


  
#7.-
#Merge the dataset of the top 10% income shares with the dataset of bottom 50% percent income shares


#Open the second csv file
Newdata1 = read.csv("data_bottom50_income_shares_long.csv", sep=",")
View(Newdata1)
#there is missing values in second dataframe

#to find the missing values, use:
sum(is.na(Newdata1))
#there are 10 missing values in second dataframe


#To imput missing values, I changed the dataframe from long to a wide format
data_large = spread(Newdata1, country, bottomshares)
View(data_large)



#to found the variable with missing values, use:
sum(is.na(data_large$brazil))
#have 10 missing data
sum(is.na(data_large$china))
sum(is.na(data_large$india))
sum(is.na(data_large$uscanada))
sum(is.na(data_large$russia))
sum(is.na(data_large$europe))


#Impute missing data with the closest year of available data. 

data_large = na_locf(data_large, option="nocb")
View(data_large)


#to change name to variable year, use:
require(reshape)
data_large = rename(data_large, c(year="Year"))



#To change data to format long, use:
data1_long = gather(data_large, country, bottomshares, brazil:uscanada, factor_key = TRUE)
#to order the dataframe by years, use:
data1_long = data1_long[order(data1_long$Year),]
View(data1_long)
#To round the numbers in the variable bottomshares 
round(data1_long$bottomshares, 1)
View(data1_long)


#Merge the dataset of the top 10% income shares with the dataset of bottom 50% percent income shares
#data_long is the top shares in long format
#data1_long is the bottoms shares in long format
merge = data.frame(data_long$Year, data_long$country, data_long$topshares, data1_long$bottomshares)
View(merge)
merge = rename(merge, c(data_long.Year = "Year", data_long.country = "Country", data_long.topshares = "Income_ts", data1_long.bottomshares = "Income_bs" ))
View(merge)









#8.-
#(i) a regression of bottom income shares on top income shares in the latest cross section


#It use only the information of the year= 2016
transversaldata = subset(merge, Year == "2016")
#eliminate column Year
borrar = c("Year", "Country")
trdata = transversaldata[, ! (names(transversaldata) %in% borrar)]
View(trdata)                         


#correlate of variable bottom incomes:
cor(trdata$Income_ts, trdata$Income_bs)
#the coefficient correlation is -0.7030971, then relationship in the variables is negative and strong.


#To obtain the regression of bottom income shares on top income shares in the latest cross section, use:
regressor=lm(formula=trdata$Income_bs ~ trdata$Income_ts)
summary(regressor)




#(ii) a regression of bottom income shares on top income shares with time and country fixed effects.
library(plm)
fijo= plm(Income_bs ~ Income_ts, data=merge, index = c("Country","Year"), model = "within")
summary(fijo) 
aleatorio=plm(Income_bs ~ Income_ts, data=merge, index = c("Country","Year"), model = "random")
summary(aleatorio)

phtest(fijo, aleatorio)

#In the Hausman Test, the p-value is less than 5%, then run a regression with fixed effects
fijo= plm(Income_bs ~ Income_ts, data=merge, index = c("Country","Year"), model = "within")
summary(fijo) 




#9.-
#Plot on the same graph the evolution of time of top 10% shares and of bottom 50% shares, taking the average across countries.
library(gplots)
par(mfrow=c(1,2))
#Graphic of Top 10% shares
plotmeans(Income_ts ~ Country, main="Top 10% shares", data=merge )
#Graphic of Bottom 50% shares
plotmeans(Income_bs ~ Country, main="Bottom 50% shares", data=merge)

