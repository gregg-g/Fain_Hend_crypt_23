library(ggplot2)
library(magrittr)
library(tidyverse)
library(nnet)
library(emmeans)
library(forestplot)

data <- read.csv(file="crypt_data_2.csv", colClasses = c("NULL",
                                                       "factor",
                                                       "factor",
                                                       "NULL"))
head(data)
str(data); levels(data$brd); levels(data$lat)
     

     #plot laterality by breed
p1 <- ggplot(data=data, aes(brd, fill=lat))
p1 + geom_bar(position="fill") +
  labs(x='Breed',
       y='Proportion of Total',
       legend='Laterality')
ggsave("plot1", device="png")

p2 <- ggplot(data=data, aes(lat, fill=brd))
p2 + geom_bar(position="fill", color="black") +
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
exp(coef(model2))   #calculate odds ratios and CI - 
exp(confint(model2))

  #!!maybe create forest plot for multinomial data??

   #check for significant differences between all pairs

emmout <- emmeans(model2, "brd", by="lat")
pairs(emmout)
contrast(emmout, "trt.vs.ctrl") #same results narrowing down groups to QH vs. each

     #relevel to compare L vs R

t1$lat <- fct_relevel(t1$lat, "Rt")
t2 <- t1 %>% filter(lat != "Bilat", .preserve=FALSE) %>%
  droplevels()

model3 <- multinom(lat ~ brd, data = t2, weights = freq)                      
summary(model3)
odds.ratios <- enframe(exp(coef(model3)))
colnames(odds.ratios) <- c("breed", "odds.ratio")

confint <- as_tibble(exp(confint(model3)))
colnames(confint) <- c("lower_CI", "upper_CI")

OR_summary <- as_tibble(c(odds.ratios[-1,], confint[-1,]))
OR_summary

  #!!need forest plot
emmout2 <- emmeans(model3, "brd", by="lat")
pairs(emmout2)
contrast(emmout2, "trt.vs.ctrl")

  #For result comparison and validation
model4 <- glm(lat ~ brd, family=binomial, data=t2, weights=freq)
summary(model4)
exp(coef(model4))
exp(confint(model4))
emmeans(model4, "brd") %>%
  pairs()
emmeans(model4, "brd") %>% contrast("trt.vs.ctrl")

  #same results as multinomial model

fp_data <- tibble(mean = OR_summary$odds.ratio,
                     lower = OR_summary$lower_CI,
                     upper = OR_summary$upper_CI,
                     breed = c("Arabian", "Draft", "Mixed", "Pony", "Saddlebred",
                               "Thoroughbred", "Warmblood"),
                     OR = round(OR_summary$odds.ratio, 3))
fp_data %>%
  forestplot(labeltext = c(breed, OR),
             clip = c(0, 8),
             vertices = TRUE,
             zero = 1,
             xlab = "Odds Ratio (vs. Quarterhorse)") %>%
  fp_append_row(breed = "Quarterhorse",
                OR = "Referrent",
                position = 1) %>%
  fp_add_header(breed = "Breed",
                OR = "OR") %>%
  fp_add_lines(h_2 = gpar(lty = 1), h_3 = gpar(columns = 1:2)) %>%
  fp_set_zebra_style("#f9f9f9")
