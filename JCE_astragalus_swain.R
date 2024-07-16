#R script analyzing swainsonine data from various Astragalus samples.
#Jeremy S Davis, original code and Data from Matt Scott
#July 16, 2024. 
#R version 4.2.0

####load packages####
library(nlme)

#import data from supplement. 
astr.seed <- read.csv("JCE_supplementary_revised.csv")

##########STATS############
#model for variation in swainsonine concentration as a result of variety or
#abiotic variables. Site is chosen as a random variable to avoid pseudorep.
model.swainseed <- anova(lme(data= astr.seed,log1.swain~variety+elevation+lat+
                               BIO1+BIO12+BIO15+day,random=~1|site))
model.swainseed

#below are models for herbivory.
#new dataframe for just plants where seed herbivory is measured
herb.seeds <- astr.seed[which(!is.na(astr.seed$seed.number)),]

#binomial logit model for seed herbivory. anova is an analysis of deviance here.
model.logseed <- anova(glm(cbind(seed.crush, seed.nh)~(log1.swain+variety+site),
                           data=herb.seeds, family="binomial"),test="Chisq")
model.logseed


#now fruit herbivory, with a new dataframe.
herb.fruit <- astr.seed[which(!is.na(astr.seed$total.fruit)),]
#remove varieties with dehisced fruits (free escape, so no exit holes to analzye)
herb.fruit <- herb.fruit[which(herb.fruit$variety != "wilsonii"  & herb.fruit$variety != "maricopae"),]

#binomial logit model for seed herbivory. anova is an analysis of deviance here.
#first is for large fruit only.
model.fruitherb <- anova(glm(cbind(large,fruit.nh+small)~log1.swain + variety+ 
                               site,data=herb.fruit, family="binomial"),test="Chisq")
model.fruitherb

#second is for small fruit, distinct in which herbivore caused the damage. 
model.fruit.small<- anova(glm(cbind(small,total.fruit-small)~log1.swain+variety
                              +site,data=herb.fruit, family="binomial"),test="Chisq")
model.fruit.small