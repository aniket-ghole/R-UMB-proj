-----------------------------------------------------------------
  
  title: "Project: The effect of Time and Sector on Emissions"
course: "MSIS-642 Multivariate statistics"
author: "Aniket"
student_ID: "01996654"
date: " May 10, 2022"

-----------------------------------------------------------------





library(readxl)
Carbon_Monitor <- read_excel("Carbon Monitor.xlsx")

#View data
View(Carbon_Monitor)

#head of data
head(Carbon_Monitor)

#dimension of data
dim(Carbon_Monitor)

#structure of data
str(Carbon_Monitor)


#create data frame
Carbon_Monitor_df <- data.frame(Carbon_Monitor)

#finding missing values
is.na(Carbon_Monitor)

#summary of variables
summary(Carbon_Monitor)

#Visualization of data
#import the libraries
library(ggplot2)
library(dplyr)

#creates the values and data frame
time<- as.numeric(rep(seq(1,12),each=12))
value<- runif(49,30,100)
group<- rep(LETTERS[1:6], times=6)
data1<-data.frame(time,value,group)

#plot the area stacked area chart
ggplot(Carbon_Monitor_df, aes(x=Dates, y=Emissions, fill=Sector))+geom_area()


# plot of regressions line
library(ggplot2)
ggplot(data = Carbon_Monitor, mapping = aes(x = Dates, y = Emissions)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Date", y = "Emission") +
  ggtitle(expression(atop("Scatterplot of Time vs Emissions", atop(italic("With Fitted Regression Line", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))



# plot of regressions line
library(ggplot2)
ggplot(data = Carbon_Monitor, mapping = aes(x = Sector, y = Emissions)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Sector", y = "Emission") +
  ggtitle(expression(atop("Scatterplot of Sector vs Emissions", atop(italic("With Fitted Regression Line", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))



# Regression

library(lmtest)
library(sandwich)

#the effect of time on Emissions
linRegr1 <- lm(Emissions~Dates, data=Carbon_Monitor)
coeftest(linRegr1, vcov. = vcovHC, type = "HC1")
summary(linRegr1)

vcov = vcovHC(linRegr1,type="HC1")
robust1 <- sqrt(diag(vcov))
robust1

coeftest1 <- coeftest(linRegr1, vcov. = vcov) 

#the effect of time and sector on emissions
linRegr2 <- lm(Emissions~Dates+Sector, data=Carbon_Monitor)
coeftest(linRegr2, vcov. = vcovHC, type = "HC1")
summary(linRegr2)

vcov = vcovHC(linRegr2,type="HC1")
robust2 <- sqrt(diag(vcov))
robust2

coeftest2 <- coeftest(linRegr2, vcov. = vcov) 


robustEE <- list(sqrt(diag(vcovHC(linRegr1, type = "HC1"))),
                 sqrt(diag(vcovHC(linRegr2, type = "HC1")))
                )
robustEE

library(stargazer)
stargazer(linRegr1, linRegr2, 
          type="html",
          digits = 2,
          se = robustEE,
          dep.var.labels=c("Emissions"),
          covariate.labels=c("Date", "Sector"),
          ci = T,
          out="models1.htm")



#hypothesis
#H0: There is no effect of time and Sector on Emissions
#H1: There is effect of time and Sector on Emissions

#Conclusion:

# There is effect of time and Sector on Emissions
