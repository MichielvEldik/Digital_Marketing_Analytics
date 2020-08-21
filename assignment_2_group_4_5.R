advertising <- read.csv("/Users/Michiel/Desktop/Advertising_Data.csv", sep=',', header =TRUE)

aggregate(advertising$Sale, by=list(advertising$Flyer_region, new$Firm_ad), FUN = mean)
#impact of flyer on/off on sale. 
#impact of firm_ad on/off on sale. 

#subsets
F0 <- subset(advertising, Flyer_region == 0)
F0A0 <- subset(F0, Firm_ad == 0)
F0A1 <- subset(F0, Firm_ad == 1)
F1 <- subset(advertising, Flyer_region == 1)
F1A0 <- subset(F1, Firm_ad == 0)
F1A1 <- subset(F1, Firm_ad == 1)
A0 <- subset(advertising, Ad_seen == 0)
A1 <- subset(advertising, Ad_seen == 1)
firmad0 <- subset(advertising, Firm_ad == 0)

xtabs(advertising$Sale ~ advertising$Existing_customer+advertising$Flyer_region)/xtabs(advertising$n ~ advertising$Existing_customer+advertising$Flyer_region)

#Amount subsets
adamount1 <- subset(Firmadonlyseen, Amount_ads < 6)
adamount1model <- glm(Sale ~ Amount_ads, family=binomial, data=adamount1)
summary(adamount1model)
adamount2 <- subset(Firmadonlyseen, Amount_ads > 5)
adamountmodel2 <- glm(Sale ~ Amount_ads, family=binomial, data=adamount2)
summary(adamountmodel2)
glmnormal <- glm(Sale ~ Amount_ads, family=binomial, data=advertising)
summary(glmnormal)

#misc. subsets
Firmadonly <- subset(advertising, Firm_ad == 1)
Firmadonlyseen <- subset(Firmadonly, Ad_seen == 1)
xtabs(Firmadonlyseen$n ~ Firmadonlyseen$Sale)

xtabs(Firmadonlyseen$Sale ~ Firmadonlyseen$Amount_ads)/xtabs(Firmadonlyseen$n ~ Firmadonlyseen$Amount_ads)
nieuwmodel <- glm(Sale ~ Dummyvariable1 + Dummyvariable2+Dummyvariable3, family=binomial, data=Firmadonlyseen)
summary(nieuwmodel)

Firmadonlyseen$Dummyvariable1 <- ifelse(Firmadonlyseen$Amount_ads == 1, 1, 0)
Firmadonlyseen$Dummyvariable2 <- ifelse(Firmadonlyseen$Amount_ads == 2, 1, 0)
Firmadonlyseen$Dummyvariable3 <- ifelse(Firmadonlyseen$Amount_ads == 3, 1, 0)
Firmadonlyseen$Dummyvariable4 <- ifelse(Firmadonlyseen$Amount_ads == 4, 1, 0)
Firmadonlyseen$Dummyvariable5 <- ifelse(Firmadonlyseen$Amount_ads == 5, 1, 0)
Firmadonlyseen$Dummyvariable6 <- ifelse(Firmadonlyseen$Amount_ads == 6, 1, 0)
Firmandonlyseen$Dummyvariable7 <- ifelse(Firmandonlyseen$Amount_ads == 7, 1, 0)

A1$Dummyvariable8 <- ifelse(A1$Amount_ads == 8, 1, 0)
A1$Dummyvariable9 <- ifelse(A1$Amount_ads == 9, 1, 0)
A1$Dummyvariable10 <- ifelse(A1$Amount_ads == 10, 1, 0)

advertising$adzero <- ifelse(advertising$Amount_ads == 0, 1, 0)
advertising$adone <- ifelse(advertising$Amount_ads == 1, 1, 0)
advertising$adtwo <- ifelse(advertising$Amount_ads == 2, 1, 0)
advertising$adthree <- ifelse(advertising$Amount_ads == 3, 1, 0)
advertising$adfour <- ifelse(advertising$Amount_ads == 4, 1, 0)
advertising$adfive <- ifelse(advertising$Amount_ads == 5, 1, 0)
advertising$adsix <- ifelse(advertising$Amount_ads == 6, 1, 0)
advertising$adseven <- ifelse(advertising$Amount_ads == 7, 1, 0)
advertising$adeight <-ifelse(advertising$Amount_ads == 8, 1, 0)
advertising$adnine <-ifelse(advertising$Amount_ads == 9, 1, 0)
advertising$adten <-ifelse(advertising$Amount_ads == 10, 1, 0)

lagvariable <-lag(advertising$Amount_ads,-1)

incrementalimpactmodel <- glm(Sale ~ Amount_ads, family=binomial, data=advertising)
summary(incrementalimpactmodel)
BIC(incrementalimpactmodel)

adglm <- glm(Sale ~ Dummyvariable1 + Dummyvariable2 + Dummyvariable3 + Dummyvariable4 + Dummyvariable5 + Dummyvariable6, family=binomial, data=Firmadonlyseen)
summary(adglm)

xtabs(advertising$Sale ~ advertising$adone)/xtabs(advertising$n ~ advertising$adone)

alincmodel <- glm(Sale ~ Ad_seen + Firm_ad*Flyer_region + Age + Gender + Firm_ad*Existing_customer + Flyer_region*Existing_customer + adone + adtwo + adthree + adfour, family=binomial, data = advertising)
summary(alincmodel)
xtabs(advertising$Sale ~ advertising$Amount_ads)/xtabs(advertising$n ~ advertising$Amount_ads)
1-pchisq(18280 - 17037, 19999 - 19989)
BIC(wtf)

Amountads <- subset(advertising, Amount_ads < 6)
Amountads2 <- subset(advertising, Amount_ads > 5)
amountadsmodel <- glm(Sale ~ Amount_ads, family = binomial, data = Amountads)
summary(amountadsmodel)
amountads2model <- glm(Sale ~ Amount_ads, family=binomial, data=Amountads2)
summary(amountads2model)
A0model <- glm(Sale ~ Firm_ad, family = binomial, data = A0)
summary(A0model)

xtabs(advertising$Sale ~ advertising$Gender)
xtabs(A0$Sale ~ A0$Amount_ads)/xtabs(A0$n ~ A0$Amount_ads)
xtabs(A0$Sale ~ A0$Amount_ads)
1-pchisq(2185.8 - 2184.9, 5953 - 5952)
adglm <- glm(Sale ~ Dummyvariable1to5, family=binomial, data=A1)
summary(adglm)

#Question 5. (include all exposure rates as seperate variables in one model)
amountadsmodel <- glm(Sale ~ Amount_ads, family = binomial, data = advertising)
summary(amountadsmodel)
amountads2model <- glm(Sale ~ Amount_ads, family=binomial, data=Amountads2)
summary(amountads2model)

adseenfirmad <- glm(Sale ~ Ad_seen, family=binomial, data =firmad0)
summary(adseenfirmad)

#Descriptives (proportion test)
B <- c(3497, 1503)
res <- prop.test(x=c(5000, 3497), n=c(20000, 14046), correct = FALSE)
res2 <- prop.test(x=c(3497, 14046), n=c(5000, 20000), p=NULL)
res2

#xtabs
advertising$n <- 1
xtabs(advertising$n ~ advertising$Ad_seen + advertising$Sale)
xtabs(advertising$n ~ advertising$Flyer_region + advertising$Firm_ad)

xtabs(advertising$Sale ~ advertising$Firm_ad + advertising$Flyer_region)

#3 is there a synergy?
xtabs(advertising$Sale ~ advertising$Flyer_region + advertising$Firm_ad)
xtabs(advertising$Sale ~ advertising$Flyer_region + advertising$Firm_ad)/xtabs(advertising$n ~ advertising$Flyer_region + advertising$Firm_ad)
xtabs(A1$Sale ~ A1$Flyer_region + A1$Firm_ad)/xtabs(A1$n ~ A1$Flyer_region + A1$Firm_ad)

# Yes there is a synergy because the sum of the individual impacts of Firm_ad and Flyer_region are much less than their combined effect.

# Amount of people who have seen the ad per group
xtabs(advertising$Ad_seen ~ advertising$Firm_ad)
xtabs(advertising$Sale ~ advertising$Existing_customer)/xtabs(advertising$n ~ advertising$Existing_customer)
xtabs(A1$Sale ~ A1$Existing_customer)/xtabs(A1$n ~ A1$Existing_customer)
xtabs(advertising$Sale ~ advertising$Amount_ads + advertising$Existing_customer)
xtabs(advertising$n ~ advertising$Existing_customer)
xtabs(advertising$Sale ~ advertising$Existing_customer)
xtabs(F1A1$n ~ F1A1$Existing_customer)
hist(advertising$Amount_ads)

xtabs(advertising$Sale ~ advertising$Ad_seen + advertising$Firm_ad)/xtabs(advertising$n ~ advertising$Ad_seen + advertising$Firm_ad)
xtabs(advertising$Existing_customer ~ advertising$Amount_ads)

xtabs(advertising$Sale ~ advertising$Gender)/xtabs(advertising$n ~ advertising$Gender)

# 4 logistic regression. nominal
logmodel <- glm(Sale ~ Firm_ad*Flyer_region + Gender + Ad_seen + Amount_ads + Flyer_region*Existing_customer + Firm_ad*Existing_customer, family = binomial, data = advertising)
summary(logmodel)
1-pchisq(18280-14000, 19999-19989)
AIC(logmodel)
BIC(logmodel)

logmodelly <- glm(Sale ~ Firm_ad*Flyer_region + Ad_seen*Age + Amount_ads*Existing_customer + Flyer_region*Existing_customer + Firm_ad*Existing_customer, family = binomial, data = advertising)
summary(logmodelly)
AIC(logmodelly)
BIC(logmodelly)

logmodeltje <- glm(Sale ~ Firm_ad + Flyer_region + Firm_ad*Flyer_region, family=binomial, data = advertising)
summary(logmodeltje)
1-pchisq(18280-15766, 19999-19997)
AIC(logmodeltje)
BIC(logmodeltje)

plot(advertising$Sale ~ advertising$Amount_ads)

logmolly <- glm(Sale ~ Amount_ads + Existing_customer, family=binomial, data=advertising)
summary(logmolly)

Testmodel <- glm(Sale ~ Existing_customer, family=binomial, data=advertising)
summary(Testmodel)

# Question 5. Along with Xtabs. Firm_ad variable has no significant impact in the model overall
xtabs(advertising$Sale ~ advertising$Ad_seen + advertising$Firm_ad)
xtabs(advertising$n ~ advertising$Ad_seen + advertising$Firm_ad)
xtabs(advertising$Sale ~ advertising$Ad_seen + advertising$Firm_ad)/xtabs(advertising$n ~ advertising$Ad_seen + advertising$Firm_ad)

# As can be seen, the proportions are almost identical when comparing two groups: people whom haven't seen the ad + charity and people who haven't seen the firm ad.
logmolly <- glm(Sale ~ Flyer_region*Existing_customer + Ad_seen, family=binomial, data=advertising)
summary(logmolly)

# Firm_ad would be a decent predictor of Ad_seen if they were to differ across the two Firm_ad variable outputs. (charity and Firm)

# A nonsignificant and nonsubstantial difference tells us that 
xtabs(advertising$Sale ~ advertising$Flyer_region + advertising$Firm_ad)
medol <- glm(Sale ~ Amount_ads, family=binomial, data=advertising)
summary(medol)
xv<- seq(min(advertising$Amount_ads), max(advertising$Amount_ads),1)
yv<- predict(medol, list(Amount_ads=xv),type="response")
lines(xv, yv, col = "red")
summary(medol$fitted.values)
hist(advertising$Sale, main = "Histogram")
summary(medol)
