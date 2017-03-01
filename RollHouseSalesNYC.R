# Author: Benjamin Reddy
# Taken from pages 49-50 of O'Neil and Schutt

#require(gdata)
#require(plyr) #Added by Monnie McGee
#install the gdata and plyr packages and load in to R.
library(car)
library(plyr)
library(gtools)
library(gdata)
# setwd("C:/MSDS 6306-SPRING2017/401/Live session 06")



# So, save the file as a csv and use read.csv instead
bk <- read.csv("C:/Users/JHosker/Documents/Rproj/LiveSessionAssignment6/rollingsales_brooklyn.csv",skip=4,header=TRUE)

## Check the data
head(bk)
summary(bk)
str(bk) # Very handy function!

## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.

## We create a new variable that is a "clean' version of sale.price.
## And sale.price.n is numeric, not a factor.

## Get rid of leading digits and any letter characters and convert remaining number string to numeric
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))

names(bk) <- tolower(names(bk)) # make all variable names lower case
## Get rid of leading digits, get rid of characters and convert number string to numeric
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
bk$year.built <- as.numeric(as.character(bk$year.built))

## Change building.class.category to character string from integer
bk$building.class.category <- as.character(bk$building.class.category)

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
############################################################
## Required Information and Comparisons
############################################################
building.category <- unique(bk$building.class.category)
familydwell <- bk[bk$building.class.category == building.category[1] | 
                    bk$building.class.category == building.category[2] | 
                    bk$building.class.category == building.category[3],
                    c("building.class.category","sale.price.n",
                    "gross.sqft","land.sqft", "year.built")]
familydwell <- na.omit(familydwell)
familydwell$sale.price.n <- familydwell$sale.price.n/1000


## Change label to shortened name for one, two and three family dwelling
familydwell$building.class.category[familydwell$building.class.category == 
                                      building.category[1]] <- "1 One Dwelling"
familydwell$building.class.category[familydwell$building.class.category == 
                                      building.category[2]] <- "2 Two Dwelling"
familydwell$building.class.category[familydwell$building.class.category == 
                                      building.category[3]] <- "3 Three Dwelling"

## Summary Statistics for One, Two and Three Family Dwellings Combined
cat("Summary of Sales Price ($000s) for 1,2 & 3 Family Dwellings")
summary(familydwell$sale.price.n)

## Sales Price:  Create BoxPlot for One, Two and Three Family Dwellings Combined
boxplot(familydwell$sale.price.n~familydwell$building.class.category,
        xlim=c(0,4),ylab="$ Price 000s", 
        main="Sales Price BoxPlot of One, Two and Three \n Family Dwellings Individually")

## Sales Price:  Create Histogram for One, Two and Three Family Dwellings Combined
hist(familydwell$sale.price.n, xlab="One to Three Family $ Price in 000s", ylab="Frequency",
    main="Sales Price Histogram of One, Two and Three \n Family Dwellings Combined")

## Sales Price:  Create Q-Q Plot using a normal distribution for One, Two 
##   and Three Family Dwellings Combined
qqnorm(familydwell$sale.price.n, xlab="Sales Price Theoretical Quantiles of One, Two \n and Three Family Dwellings Combined",
       ylab="Sample Quantiles in $ Price 000s",
       main="Normal Q-Q Plot of Sales Price for One, \n Two and Three Family Dwellings")

## Graph Sales Price vs. Gross Sqft for One, Two and Three Family Dwellings Combined
plot(familydwell$sale.price.n,familydwell$gross.sqft,
     xlab="Sales Price in 000s", ylab = "Gross Sqft",
     main="Sales Price vs. Gross Sqft for 1 to 3 \n Family Dwellings")

## Individual Graph Sales Price vs. Gross Sqft for One, Two and Three Family Dwellings Combined
## We filter out outliers above $10 Mill which is four samples: 
## $11.36, 13.181, 15.0 and 19.875 mill
count(familydwell$sale.price.n[familydwell$sale.price.n > 10000])
plot(familydwell$sale.price.n,familydwell$gross.sqft,
     xlab="Sales Price in 000s", ylab = "Gross Sqft",xlim=c(1,10000),
     main="Sales Price vs. Gross Sqft for 1 to 3 \n Family Dwelling (< $10Mill for sales price)")

## Individual Graph Sales Price vs. Year Built for One, Two and Three Family Dwellings Combined
plot(familydwell$year.built,familydwell$sale.price.n, xlab = "Year Built", 
     ylab="Sales Price in 000s",xlim=c(1800,2017),
     main="Year Built vs. Sales Price for 1 to 3 \n Family Dwelling")

## Individual Graph Sales Price vs. Land Sqft for One, Two and Three Family Dwellings Combined
## We filter out outliers above 15k which is two samples: 42.5 and 68.6k samples
count(familydwell$land.sqft[familydwell$land.sqft > 15000])
plot(familydwell$sale.price.n,familydwell$land.sqft,
     xlab="Sales Price in 000s", ylab = "Land Sqft", ylim=c(1,15000),
     main="Sales Price vs. Land Sqft for 1 to 3 \n Family Dwelling (< 15k land sqft)")

## Year Built:  Create BoxPlot for One, Two and Three Family Dwellings Combined
boxplot(familydwell$year.built~familydwell$building.class.category,
        xlim=c(0,4),ylab="Year Built", ylim=c(1800,2017),
        main="Year Built BoxPlot of One, Two and Three \n Family Dwellings Individually")

## Year Built:  Create Histogram for One, Two and Three Family Dwellings Combined
## bin <- c(1800,50,50,50,50,10)
## hist(as.numeric(familydwell$year.built), 
     ## xlim=c(1800,max(familydwell$year.built)),
##     xlab="Year Built for One to Three Families", ylab="Frequency",
##     main="Year Built Histogram of One, Two and Three \n Family Dwellings Combined")

## Gross Sqft:  Create BoxPlot for One, Two and Three Family Dwellings Combined
## We filter out outliers above 15k which is two samples:  20.457k sample
count(familydwell$gross.sqft[familydwell$gross.sqft > 15000])
boxplot(familydwell$gross.sqft~familydwell$building.class.category,
        xlim=c(0,4),ylab="Gross Sqft", ylim=c(1,15000),
        main="Gross Sqft BoxPlot of One, Two and Three Family \n Dwellings Individually (< 15K Gross Sqft)")

hist(as.numeric(familydwell$gross.sqft), xlim=c(1,15000), 
  xlab="Gross Sqft for One to Three Family Dwell", ylab="Frequency",
  main="Histogram of Gross Sqft for One, Two and Three \n Family Dwellings Combined")


## Land Sqft:  Create BoxPlot for One, Two and Three Family Dwellings Combined
## We filter out outliers above 15k which is two samples: 42.5 and 68.6k samples
count(familydwell$land.sqft[familydwell$land.sqft > 15000])
boxplot(familydwell$land.sqft~familydwell$building.class.category,
        xlim=c(0,4),ylab="Land Sqft", ylim=c(1,15000),
        main="Land Sqft BoxPlot of One, Two and Three \n Family Dwellings Individually")

hist(as.numeric(familydwell$land.sqft), xlim=c(1,15000), 
     xlab="Land Sqft for One to Three Family Dwell", ylab="Frequency",
     main="Histogram of Land Sqft for One, Two and Three \n Family Dwellings Combined")


#############################################################################
## Create Same Information for each individual Family Group for sales.price.n
#############################################################################

## Create Historgrams of Each Family Dwelling Individually
hist(familydwell$sale.price.n[familydwell$building.class.category == "1 One Dwelling"], 
     xlab=" $ Price in 000s", ylab="Frequency", main="Sales Price Histogram of One Family Dwellings")
hist(familydwell$sale.price.n[familydwell$building.class.category == "2 Two Dwelling"], 
     xlab=" $ Price in 000s", ylab="Frequency", main="Sales Price Histogram of Two Family Dwellings")
hist(familydwell$sale.price.n[familydwell$building.class.category == "3 Three Dwelling"], 
     xlab=" $ Price in 000s", ylab="Frequency", main="Sales Price Histogram of Three Family Dwellings")

## Create Q-Q Plot for One, Two and Three Family Dwellings Individually
qqnorm(familydwell$sale.price.n[familydwell$building.class.category == "1 One Dwelling"], 
       xlab="Sales Price Theoretical Quantiles of One Family Dwellings", 
       ylab="Sample Quantiles in $ Price 000s",
       main="Normal Q-Q Plot of Sales Price for One Family Dwellings")
qqnorm(familydwell$sale.price.n[familydwell$building.class.category == "2 Two Dwelling"], 
       xlab="Sales Price Theoretical Quantiles of Two Family Dwellings", 
       ylab="Sample Quantiles in $ Price 000s",
       main="Normal Q-Q Plot of Sales Price for Two Family Dwellings")
qqnorm(familydwell$sale.price.n[familydwell$building.class.category == "3 Three Dwelling"], 
       xlab="Sales Price Theoretical Quantiles of Three Family Dwellings", 
       ylab="Sample Quantiles in $ Price 000s", 
       main="Normal Q-Q Plot of Sales Price for Three Family Dwellings")

## remove outliers 
detach(bk)

