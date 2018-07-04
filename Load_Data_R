#TODO: Add worldbankdata "birth attended by professional health staff


# Set the working directory

setwd("C:/Users/Privat/SPL/Code/R_Working")


#Install the tidyverse package if not installed
#https://www.tidyverse.org/
if (any(grepl("tidyverse", installed.packages())) == FALSE){
  install.packages("tidyverse")
}

#Install countrycode package if not installed
#https://cran.r-project.org/web/packages/countrycode/countrycode.pdf

if (any(grepl("countrycode", installed.packages())) == FALSE){
  install.packages("countrycode")
}

library(countrycode)

library(readxl)

#Read in Cild Mortaltiy data
#Remark: Downloaded File had to be modified slightly before reading, in
#because otherwise it would have been imposible to use 

chm <- read_excel("Child_Mortality.xlsx", 
                  sheet = 10, 
                  col_names = TRUE, 
                  skip = 2)

#Rename the First three colums, which are not read in correctly by the
#read_excel function

names(chm)[1:3] <- c("WHO regions", "country", "All under 5 death")

#For a start, only death of under 5 year olds by all causes

chm = chm[, 1:3]

#Read in population data from World Bank Database csv file

population <- read.csv("population_worldbank.csv",
                    skip = 4, 
                    header = TRUE)

population.2010 <- subset(population, 
                       select = c(Country.Code, X2010))

population.2010$country <- countrycode(population.2010$Country.Code,
                                    "wb", "country.name")

population.2010$Country.Code = NULL

names(population.2010)[1] <-  "Population_2010"


#Read in GDP data from World Bank Database csv file
#Download at https://data.worldbank.org
#All Datafiles which are available for Download in the Worldbank Database can be 
#read in with this command

#TODO: hieraus noch eine Funktion basteln um beide Datensätze über die Funktion
#einlesen zu können

gdp.ppp <- read.csv("GDP_PPP_constant_2011dollar.csv",
                    skip = 4, 
                    header = TRUE)

#

gdp.ppp.2010 <- subset(gdp.ppp, 
                       select = c(Country.Code, X2010))

names(gdp.ppp.2010)[2] <-  "GDP_PPP_2010"

#Use the countrycode package, to assign the standard english country names to each
#observation. The reference point in the dataframe are the Worldbank Countrycodes

gdp.ppp.2010$country <- countrycode(gdp.ppp.2010$Country.Code,
                                    "wb", "country.name")

#Drop all observations without where 'country' is empty
#(These are for example the cumulative observations included in the worldbank
#data, like 'African countries')

gdp.ppp.2010 <- subset(gdp.ppp.2010,
                      gdp.ppp.2010$country != 0)


#Read in Standardized World Income Inequality Database

load("swiid6_2.rda")


#Create Subset of swiid_summary
#Keep Only the Observations of 2010 for Real Disposable Income Gini Coefficient

swiid.2010 <- subset(swiid_summary, 
                     swiid_summary$year == 2010,
                     select = c(country, year, gini_disp))

#Merge the three single data frames into one dataframe using the 'country' variable

final.df <- merge(gdp.ppp.2010, swiid.2010, by = "country")

final.df <- merge(final.df, chm, by = "country")

final.df <- merge(final.df, population.2010)

#Calculate death under 5 compared to total population

final.df$deathper1000population <- 
  (final.df$`All under 5 death`/final.df$Population_2010)*1000

#Correlation GDP and Mortality

corr.GDP.mortality <- cor(final.df$deathper1000population,
            final.df$GDP_PPP_2010, 
            method ="pearson")

plot(final.df$GDP_PPP_2010,
     final.df$deathper1000population,
     main  = "Total Child Mortality and Income, 2010",
     xlab  = "GDP per Capita in constant 2011 US-Dollar (PPP)",
     ylab  = "Dead children per 1000 inhabitants")

#Correlation gini coefficient and mortality

corr.gini.mortality <- cor(final.df$deathper1000population,
                          final.df$gini_disp, 
                          method ="pearson")

plot(final.df$gini_disp,
     final.df$deathper1000population,
     main  = "Total Child Mortality and Inequality, 2010",
     xlab  = "Gini Coefficent",
     ylab  = "Dead children per 1000 inhabitants")
