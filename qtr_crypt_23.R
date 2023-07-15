library(ggplot2)
library(magrittr)
library(tidyverse)
library(nnet)


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
p1 + geom_bar(position="fill")
ggsave("plot1", device="png")

p2 <- ggplot(data=data, aes(lat, fill=brd))
p2 + geom_bar(position="stack", color="black") +
  scale_fill_manual(name="Breed",
                    values=c("grey","white"),
                    labels=c("Other", "Quarterhorse")) +
  theme_classic() +
  labs(x="Laterality", y="Total Cases")
ggsave("plot2", device="png")

cont_table <- table(data$brd, data$lat)                                         #create contingency table
cont_table

chisq.test(cont_table)                                                          #are there any significant differences?
mosaicplot(t(cont_table))

data <- data %>%                                                                #combine into 3 groups (L, R, Bilat)
  mutate(lat_group = substr(lat, 1, 1))

cont_table_2 <- table(data$brd, data$lat_group)                                 #create new contingency table with 3 groups
cont_table_2

chisq.test(cont_table_2)                                                        #check for significant differences
fisher.test(cont_table_2)
mosaicplot(t(cont_table_2))

p3 <- ggplot(data=data, aes(lat_group, fill=brd))
p3 + geom_bar(position="stack", color="black") +
  scale_fill_manual(name="Breed",
                    values=c("grey","white"),
                    labels=c("Other", "Quarterhorse")) +
  theme_classic() +
  labs(x="Laterality", y="Total Cases")
ggsave("plot3", device="png")                                                   

  #need regression model to get OR for individual laterality
t1 <- as.data.frame.table(cont_table_2)                                         #create tall data from contingency table
colnames(t1) <- c("brd", "lat", "freq")
str(t1)                                                                         #check to make sure table looks correct

#model1 <- glm(brd ~ lat, family=binomial, data=t1, weights=freq)               #create logistic regression model and view log odds ratios
#summary(model1)
#confint(model1)
  #OR table
#t2 <- exp(cbind(OddsRatio=coef(model1), confint(model1))) %>%                  #extract odds ratios and confidence intervals
#  round(digits=4)
#t2[c(2,3),]

  #multinomial model
model2 <- multinom(lat ~ brd, data = t1, weights = freq)                        #more appropriate model - but not comparing L to R
summary(model2)
exp(coef(model2))
exp(confint(model2))

t1$lat <- fct_relevel(t1$lat, "R")                                              #relevel to compare L vs R

model3 <- multinom(lat ~ brd, data = t1, weights = freq)                      
summary(model3)
exp(coef(model3))
exp(confint(model3))
