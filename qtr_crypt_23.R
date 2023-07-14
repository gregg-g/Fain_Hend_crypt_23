library(ggplot2)
library(magrittr)
library(tidyverse)


getwd()
setwd("~/SynologyDrive/GreggDrive/Other Stats/Fain_Hend_crypt_23")
data <- read.csv(file="crypt_data.csv", colClasses = c("integer",
                                                       "NULL",
                                                       "factor",
                                                       "NULL",
                                                       "factor"))
head(data)
str(data)
t.test(wt ~ brd, data = data)                                                   #check that weight between groups is not different

  #plot laterality by breed
p1 <- ggplot(data=data, aes(brd, fill=lat))
p1 + geom_bar(position="stack")

p2 <- ggplot(data=data, aes(lat, fill=brd))
p2 + geom_bar(position="stack")

cont_table <- table(data$brd, data$lat)
cont_table

chisq.test(cont_table)
mosaicplot(t(cont_table))

data <- data %>%                                                                #combine into 3 groups (L, R, Bilat)
  mutate(lat_group = substr(lat, 1, 1))

cont_table_2 <- table(data$brd, data$lat_group)                                 #create new contingency table with 3 groups
cont_table_2

chisq.test(cont_table_2)                                                        #check for significant differences
fisher.test(cont_table_2)
mosaicplot(t(cont_table_2))

p3 <- ggplot(data=data, aes(lat_group, fill=brd))
p3 + geom_bar(position="stack") +
  theme_bw()

  #need logistic regression model to get OR for individual laterality
t1 <- as.data.frame.table(cont_table_2)                                         #create tall data from contingency table
colnames(t1) <- c("brd", "lat", "freq")
str(t1) #check to make sure table looks correct

model1 <- glm(brd ~ lat, family=binomial, data=t1, weights=freq)                #create model and view log odds ratios
summary(model1)
confint(model1)
  #OR table
t2 <- exp(cbind(OddsRatio=coef(model1), confint(model1))) %>%                   #extract odds ratios and confidence intervals
  round(digits=4)
t2[c(2,3),]
