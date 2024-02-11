library(ggplot2)
library(magrittr)
library(tidyverse)
library(nnet)
library(emmeans)

data <- read.csv(file="crypt_data_2.csv", colClasses = c("NULL",
                                                       "factor",
                                                       "factor",
                                                       "NULL"))
head(data)
str(data); levels(data$brd); levels(data$lat)
     

     #plot laterality by breed
p1 <- ggplot(data=data, aes(brd, fill=lat))
p1 + geom_bar(position="fill")
ggsave("plot1", device="png")

p2 <- ggplot(data=data, aes(lat, fill=brd))
p2 + geom_bar(position="stack", color="black") +
 # scale_fill_manual(name="Breed",
#                  values=c("grey","white"),
 #                   labels=c("Other", "Quarterhorse")) +
  theme_classic() +
  labs(x="Laterality", y="Total Cases")
ggsave("plot2", device="png")

     #create contingency table
cont_table <- table(data$brd, data$lat)
cont_table

     #are there any significant differences?
chisq.test(cont_table)
fisher.test(cont_table, simulate.p.value = TRUE) #there are differences in the distribution between groups
mosaicplot(t(cont_table)) #not super helpful or particularly legible


               

  #need regression model to get OR for individual laterality
     #create tall data from contingency table
t1 <- as.data.frame.table(cont_table)
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

t1$brd <- fct_relevel(t1$brd, "QH")  #relevel to compare all to QH

model2 <- multinom(lat ~ brd, data = t1, weights = freq, Hess=T, model=T)
summary(model2)
exp(coef(model2))   #calculate odds ratios and CI
exp(confint(model2))

  #!!create forest plot for multinomial data!!

   #check for significant differences between all pairs

emmout <- emmeans(model2, "brd", by="lat")
pairs(emmout)
contrast(emmout, "trt.vs.ctrl") #same results narrowing down groups to QH vs. each

     #relevel to compare L vs R

t1$lat <- fct_relevel(t1$lat, "Rt")
t2 <- filter(t1, lat != "Bilat")

model3 <- multinom(lat ~ brd, data = t2, weights = freq)                      
summary(model3)
exp(coef(model3))
exp(confint(model3))

emmout2 <- emmeans(model3, "brd", by="lat")
pairs(emmout2)
contrast(emmout2, "trt.vs.ctrl")

model4 <- glm(lat ~ brd, family=binomial, data=t2, weights=freq)
summary(model4)
exp(coef(model4))
exp(confint(model4))
emmeans(model4, "brd") %>%
  pairs()
emmeans(model4, "brd") %>% contrast("trt.vs.ctrl")
