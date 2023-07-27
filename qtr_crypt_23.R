library(ggplot2)
library(magrittr)
library(tidyverse)
library(nnet)

data <- read.csv(file="crypt_data.csv", colClasses = c("integer",
                                                       "NULL",
                                                       "factor",
                                                       "NULL",
                                                       "factor"))
head(data)
str(data)
     #check that weight between groups is not different
t.test(wt ~ brd, data = data)

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

     #create contingency table
cont_table <- table(data$brd, data$lat)
cont_table

     #are there any significant differences?
chisq.test(cont_table)
mosaicplot(t(cont_table))

     #combine into 3 groups (L, R, Bilat)
data_small <- data %>%
  mutate(lat_group = substr(lat, 1, 1))

     #create new contingency table with 3 groups
cont_table_2 <- table(data_small$brd, data_small$lat_group)
cont_table_2

     #check for significant differences
chisq.test(cont_table_2)
fisher.test(cont_table_2)
mosaicplot(t(cont_table_2))

p3 <- ggplot(data=data_small, aes(lat_group, fill=brd))
p3 + geom_bar(position="stack", color="black") +
  scale_fill_manual(name="Breed",
                    values=c("grey","white"),
                    labels=c("Other", "Quarterhorse")) +
  theme_classic() +
  labs(x="Laterality", y="Total Cases")
ggsave("plot3", device="png")                                                   

  #need regression model to get OR for individual laterality
     #create tall data from contingency table
t1 <- as.data.frame.table(cont_table_2)
colnames(t1) <- c("brd", "lat", "freq")
     #check to make sure table looks correct
str(t1)

     #create logistic regression model and view log odds ratios***NOT USED***
#model1 <- glm(brd ~ lat, family=binomial, data=t1, weights=freq)
#summary(model1)
#confint(model1)
  #OR table
     #extract odds ratios and confidence intervals
#t2 <- exp(cbind(OddsRatio=coef(model1), confint(model1))) %>%
#  round(digits=4)
#t2[c(2,3),]

  #multinomial model
     #more appropriate model - but not comparing L to R
model2 <- multinom(lat ~ brd, data = t1, weights = freq)
summary(model2)
exp(coef(model2))
exp(confint(model2))

     #relevel to compare L vs R
t1$lat <- fct_relevel(t1$lat, "R")

model3 <- multinom(lat ~ brd, data = t1, weights = freq)                      
summary(model3)
exp(coef(model3))
exp(confint(model3))
