# Set the working directory

setwd("C:/Users/Privat/SPL/Code/R_Working")

#The stargazer package is needed to produce nice LaTex tables

if (any(grepl("stargazer", installed.packages())) == FALSE){
  install.packages("stargazer")
}

library(stargazer)

#List off all WHO regions

who.regions = final.df$region_who
who.regions = unique(who.regions)

#Create regional dummies for all six WHO regions

final.df$region_dummy_EMRO = ifelse(final.df$region_who == "EMRO",1,0)
final.df$region_dummy_EURO = ifelse(final.df$region_who == "EURO",1,0)
final.df$region_dummy_AFRO = ifelse(final.df$region_who == "AFRO",1,0)
final.df$region_dummy_AMRO = ifelse(final.df$region_who == "AMRO",1,0)
final.df$region_dummy_WPRO = ifelse(final.df$region_who == "WPRO",1,0)
final.df$region_dummy_SEARO = ifelse(final.df$region_who == "SEARO",1,0)

#Generate natural log of GDP with sapply

final.df$ln_GDP_PPP_2010 = sapply(final.df$GDP_PPP_2010, log)

#Estimate linear model with logaritmic child mortality as the dependend variable

lreg1 = lm(final.df$death_under_5_per_birth~final.df$ln_GDP_PPP_2010)

#Add the gini coefficient and the birthrate as control 
#variables

lreg2 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$gini_disp
           +final.df$birthrate_2010)

#Control for region dummies

lreg3 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$region_dummy_AFRO
           +final.df$region_dummy_AMRO
           +final.df$region_dummy_EMRO
           +final.df$region_dummy_EURO
           +final.df$region_dummy_SEARO
           +final.df$region_dummy_WPRO)

#Controlled for the gini coefficient and the birthrate,
#As well as for the region dummies

lreg4 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$gini_disp
           +final.df$birthrate_2010
           +final.df$region_dummy_AFRO
           +final.df$region_dummy_AMRO
           +final.df$region_dummy_EMRO
           +final.df$region_dummy_EURO
           +final.df$region_dummy_SEARO
           +final.df$region_dummy_WPRO)

#Create Latex table

stargazer(lreg1, lreg2, lreg3, lreg4,
          title="Child Mortality and Income", 
          out = "C:/Users/Privat/SPL/Code/R_Working/reg_table.tex",
          out.header = TRUE,
          align=TRUE, dep.var.labels=c("Children dead under 5(in percent)"),
          covariate.labels=c("log(GDP in 2010)",
                             "Gini Coefficient in 2010",
                             "Birthrate in 2010",
                             "WHO Region AFRO",
                             "WHO Region AMRO",
                             "WHO Region EMRO",
                             "WHO Region EURO",
                             "WHO Region SEARO",
                             "WHO Region WPRO"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)

setwd("/Users/grzegorzantkiewicz/Documents/R_Working")


#Install the package ggplot2


if (any(grepl("ggplot2", installed.packages())) == FALSE){
  install.packages("ggplot2")
}

library("ggplot2")

library("RColorBrewer")

final.df_Africa = subset(final.df, region_who == "AFRO")
final.df_Europe = subset(final.df, region_who == "EURO")


#Correlation between GDP per Capita and Death 
#under 5 per birth worldwide and moving average
d1 = ggplot(final.df,
            aes(x=final.df$GDP_PPP_2010,
                y=final.df$death_under_5_per_birth)) + 
  geom_point(aes(color=final.df$region_who))  + 
  geom_smooth() +
  labs(x="GDP per Capita in constant 2011 US-Dollar (PPP)",
       y ="Death under 5 per birth (in percent)",
       title = "Total Child Mortality and Income, 2010",
       col = "WHO region") 
?ggplot


d1

#Correlation between GDP per Capita and Death
#under 5 per birth in Europe and Africa


d2 = ggplot(final.df_Europe, aes(x=final.df_Europe$GDP_PPP_2010,
                                 y=final.df_Europe$death_under_5_per_birth)) +
  geom_point(color="#33CCFF") +
  theme(plot.background = element_blank()) +
  ylim(0, 7.5) + 
  xlim(0, 125000)+
  geom_smooth(method=lm) +
  labs( x="GDP per Capita in US Dollar (PPP)",
        y ="Death under 5 per birth (in %)",
        title = "Total Child Mortality and Income in Europe, 2010")
d2
?xlim

d2ver2 = d2 +
  ylim(0, 7.5) + 
  xlim(0, 90000)
d2ver2

d3 = ggplot(final.df_Africa, aes(x=final.df_Africa$GDP_PPP_2010,
                                 y=final.df_Africa$death_under_5_per_birth)) +
  geom_point(color="#FF3300") +
  geom_smooth(method=lm) +
  ylim(0, 22) +
  xlim(0, 40000) +
  labs(x="GDP per Capita in US Dollar (PPP)",
       y ="Death under 5 per birth (in %)",
       title = "Total Child Mortality and Income in Africa, 2010")

d3


#Arranging 2 graphs on the same page
if (any(grepl("gridExtra", installed.packages())) == FALSE){
  install.packages("gridExtra")
}

library("gridExtra")
grid.arrange(d2, d3, ncol = 2)


setwd("/Users/grzegorzantkiewicz/Documents/R_Working")

density1 = ggplot(final.df_Europe,
                  aes(GDP_PPP_2010,
                      fill = as.factor(final.df_Europe$region_who)))+
  geom_density(fill = "red", alpha = .3) +
  labs(x = "GDP per Capita in Europe in US-Dollar (PPP)",
       y = "Distriburion of Data",
       title = "Distribution of GDP per Capita in Europe")
density1

region = as.factor(final.df_Africa$region_who)

density2 = ggplot(final.df_Africa,
                  aes(GDP_PPP_2010,
                      fill = as.factor(final.df_Africa$region_who))) +
  geom_density(fill = "yellow", alpha = .3) +
  labs(x = "GDP per Capita in Africa in US-Dollar (PPP)",
       y = "Distriburion of Data",
       title = "Distribution of GDP per Capita in Africa")

density2
grid.arrange(density1, density2 , ncol = 2)

#Box-plot 



boxp1 = ggplot(final.df, aes(x = final.df$region_who,
                             y = final.df$GDP_PPP_2010,
                             color = final.df$region_who)) +
  geom_boxplot() +
  labs(y="GDP per Capita in US-Dollar (PPP)",
       x = "WHO region" ,
       title = "GDP by WHO regions") +
  theme(legend.position = "none")+
  coord_flip()

boxp1

#Set working directory 
setwd("/Users/grzegorzantkiewicz/Documents/R_Working")


#Probability of dying before being 5

final.df$death_probability =
  (final.df$death_under_5_total/final.df$birth_total)

d5 = ggplot(final.df, aes(x=final.df$region_who,
                          y=final.df$death_probability,
                          color = final.df$region_who)) + 
  geom_boxplot() + 
  labs(x = "WHO region",
       y = "Probability of dying in the first 5 years",
       title="Probability of dying in the first 5 years in every WHO region") +
  coord_flip() + 
  theme(legend.position = "none")

d5

if (any(grepl("rworldmap", installed.packages())) == FALSE){
  install.packages("rworldmap")
}
library("rworldmap")


mapped_data <- joinCountryData2Map(final.df, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data,
               nameColumnToPlot = "death_probability",
               mapTitle = "Probability of death under 5",
               missingCountryCol = "#FFFFFF", 
               oceanCol = "#CCFFFF")

#Set working directory 
setwd("/Users/grzegorzantkiewicz/Documents/R_Working")


#Function testing the normal distribution of the data

if (any(grepl("tseries", installed.packages())) == FALSE){
  install.packages("tseries")
  YES
}
library("tseries")


nor = function(x) {
  
  jarque.bera.test(complete.cases(x))
  
}

#Testing the normality of data for the world, Europe and Africa

nor(final.df$death_under_5_per_birth)


#Function testing the significance of correlation 
cor = function(x,y){
  cor.test(x, y)
}
#Test on significance of correlation in all countries 

cor(final.df$GDP_PPP_2010,
    final.df$death_under_5_per_birth)

#Test on significance of correlation in Europe

cor(final.df_Europe$GDP_PPP_2010, 
    final.df_Europe$death_under_5_per_birth)

#Test on significance of correlation in Africa

cor(final.df_Africa$GDP_PPP_2010,
    final.df_Africa$death_under_5_per_birth)


#Function testing the heteroskedasticity
if (any(grepl("lmtest", installed.packages())) == FALSE){
  install.packages("lmtest")
}
library("lmtest")

het = function(x, y) {
  modell = lm(y~x)
  gqtest(modell)
  
}

#Test on heteroskedasticity in the world

het(final.df$GDP_PPP_2010, final.df$death_under_5_per_birth)

#Test on heteroskedasticity in Europe

het(final.df_Europe$GDP_PPP_2010, final.df_Europe$death_under_5_per_birth)

# Set the working directory

setwd("C:/Users/Privat/SPL/Code/R_Working")

#The stargazer package is needed to produce nice LaTex tables

if (any(grepl("stargazer", installed.packages())) == FALSE){
  install.packages("stargazer")
}

library(stargazer)

#List off all WHO regions

who.regions = final.df$region_who
who.regions = unique(who.regions)

#Create regional dummies for all six WHO regions

final.df$region_dummy_EMRO = ifelse(final.df$region_who == "EMRO",1,0)
final.df$region_dummy_EURO = ifelse(final.df$region_who == "EURO",1,0)
final.df$region_dummy_AFRO = ifelse(final.df$region_who == "AFRO",1,0)
final.df$region_dummy_AMRO = ifelse(final.df$region_who == "AMRO",1,0)
final.df$region_dummy_WPRO = ifelse(final.df$region_who == "WPRO",1,0)
final.df$region_dummy_SEARO = ifelse(final.df$region_who == "SEARO",1,0)

#Generate natural log of GDP with sapply

final.df$ln_GDP_PPP_2010 = sapply(final.df$GDP_PPP_2010, log)

#Estimate linear model with logaritmic child mortality as the dependend variable

lreg1 = lm(final.df$death_under_5_per_birth~final.df$ln_GDP_PPP_2010)

#Add the gini coefficient and the birthrate as control 
#variables

lreg2 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$gini_disp
           +final.df$birthrate_2010)

#Control for region dummies

lreg3 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$region_dummy_AFRO
           +final.df$region_dummy_AMRO
           +final.df$region_dummy_EMRO
           +final.df$region_dummy_EURO
           +final.df$region_dummy_SEARO
           +final.df$region_dummy_WPRO)

#Controlled for the gini coefficient and the birthrate,
#As well as for the region dummies

lreg4 = lm(final.df$death_under_5_per_birth
           ~final.df$ln_GDP_PPP_2010
           +final.df$gini_disp
           +final.df$birthrate_2010
           +final.df$region_dummy_AFRO
           +final.df$region_dummy_AMRO
           +final.df$region_dummy_EMRO
           +final.df$region_dummy_EURO
           +final.df$region_dummy_SEARO
           +final.df$region_dummy_WPRO)

#Create Latex table

stargazer(lreg1, lreg2, lreg3, lreg4,
          title="Child Mortality and Income", 
          out = "C:/Users/Privat/SPL/Code/R_Working/reg_table.tex",
          out.header = TRUE,
          align=TRUE, dep.var.labels=c("Children dead under 5(in percent)"),
          covariate.labels=c("log(GDP in 2010)",
                             "Gini Coefficient in 2010",
                             "Birthrate in 2010",
                             "WHO Region AFRO",
                             "WHO Region AMRO",
                             "WHO Region EMRO",
                             "WHO Region EURO",
                             "WHO Region SEARO",
                             "WHO Region WPRO"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)
