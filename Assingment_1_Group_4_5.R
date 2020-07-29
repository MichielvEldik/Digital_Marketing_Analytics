#import data into R
search <-read.csv("/Users/Michiel/Desktop/Search_Data(1).csv")
#load library
library(ggplot2)
#summarize data to gain a general overview
summary(search)

#set-up for linear regression difference in difference analysis
did <- read.csv("/Users/Michiel/Desktop/diddata.csv", sep=';', header =TRUE)
did$DummyTreatment <- ifelse(did$Treatment.Control > 0, 1, 0)
did$DummyPrepost <- ifelse(did$Before.after > 0, 1, 0)
did$interactionvariable <- did$DummyTreatment * did$DummyPrepost
didreg <- lm(TotalTraffic ~ DummyTreatment + DummyPrepost + interactionvariable, data = did)
summary(didreg)

#variables for total traffic Google and Bing.
search$TotalTrafficGoogle <- search$SponsoredTrafficGoogle + search$OrganicTrafficGoogle
search$TotalTrafficBing <- search$SponsoredTrafficBing + search$OrganicTrafficBing
#variable for total organic traffic.
search$TotalOrganicTraffic <- search$OrganicTrafficGoogle + search$OrganicTrafficBing
#variable for total traffic.
search$TotalTraffic <- search$TotalTrafficGoogle + search$TotalTrafficBing
#variable (dummy) to distinguish between the data where SEA is on, and the data for which SEA is off.
search$SEAGoogleOn <- ifelse(search$SponsoredTrafficGoogle > 0, 1, 0)

#subsets that contain only the parts where SEA in Google is turned on vs. off
before <- subset(search, SEAGoogleOn == 1)
after <- subset(search, SEAGoogleOn == 0)

#subset search$before ex. outlier
exoutlier<- before[-c(10),]
summary(before)
summary(after)
summary(before$SponsoredTrafficGoogle)
summary(before$OrganicTrafficGoogle)
summary(after$OrganicTrafficGoogle)

#Descriptives [TotalTrafficGoogle] - BEFORE
hist(before$TotalTrafficGoogle)
hist(before$TotalTrafficGoogle, breaks = 15, main = "Breaks=15")
summary(before$TotalTrafficGoogle)
mean(before$TotalTrafficGoogle)
var(before$TotalTrafficGoogle)
sd(before$TotalTrafficGoogle)
qqnorm(before$TotalTrafficGoogle, pch = 1, frame = FALSE)
qqline(before$TotalTrafficGoogle)
shapiro.test(before$TotalTrafficGoogle)
boxplot(before$TotalTrafficGoogle)
barplot(before$TotalTrafficGoogle)

#Descriptives [TotalTrafficGoogle] - AFTER
hist(after$TotalTrafficGoogle)
hist(after$TotalTrafficGoogle, breaks = 15, main = "Total Traffic Google After")
summary(after$TotalTrafficGoogle)
var(after$TotalTrafficGoogle)
sd(after$TotalTrafficGoogle)
qqnorm(after$TotalTrafficGoogle, pch = 1, frame = FALSE)
qqline(after$TotalTrafficGoogle)
shapiro.test(after$TotalTrafficGoogle)
boxplot(after$TotalTrafficGoogle)
barplot(after$TotalTrafficGoogle)

#Descriptives [TotalTrafficBing] - BEFORE 
hist(before$TotalTrafficBing)
hist(before$TotalTrafficBing, breaks = 20, main = "Total Traffic Bing Before")
summary(before$TotalTrafficBing)
var(before$TotalTrafficBing)
sd(before$TotalTrafficBing)
qqnorm(before$TotalTrafficBing, pch = 1, frame = FALSE)
qqline(before$TotalTrafficBing)
shapiro.test(before$TotalTrafficBing)
boxplot(before$TotalTrafficBing)
barplot(before$TotalTrafficBing)

#Descriptives [TotalTrafficBing] - BEFORE _ *****_-_\\|...EX OUTLIER...|//_-_*****
hist(exoutlier$TotalTrafficBing)
hist(exoutlier$TotalTrafficBing, breaks = 15, main = "Total Traffic Bing Before ex. outlier")
summary(exoutlier$TotalTrafficBing)
var(exoutlier$TotalTrafficBing)
sd(exoutlier$TotalTrafficBing)
qqnorm(exoutlier$TotalTrafficBing, pch = 1, frame = FALSE)
qqline(exoutlier$TotalTrafficBing)
shapiro.test(exoutlier$TotalTrafficBing)
shapiro.test(exoutlier$TotalTrafficGoogle)
boxplot(exoutlier$TotalTrafficBing)
barplot(exoutlier$TotalTrafficBing)

#Descriptives [TotalTrafficBing] - AFTER
hist(after$TotalTrafficBing)
hist(after$TotalTrafficBing, breaks = 20, main = "Total Traffic Bing (after)")
summary(after$TotalTrafficBing)
var(after$TotalTrafficBing)
sd(after$TotalTrafficBing)
qqnorm(after$TotalTrafficBing, pch = 1, frame = FALSE, main = "Normal q-q Plot Total Traffic Bing (after)")
qqline(after$TotalTrafficBing)
shapiro.test(after$TotalTrafficBing)
boxplot(after$TotalTrafficBing)
barplot(after$TotalTrafficBing)

#descriptives for [Bing Total] - BEFORE + AFTER
hist(search$TotalTrafficBing)
hist(search$TotalTrafficBing, breaks = 20, main = "20 breaks")
summary(search$TotalTrafficBing)
var(search$TotalTrafficBing)
sd(search$TotalTrafficBing)
qqnorm(search$TotalTrafficBing, pch = 1, frame = FALSE)
qqline(search$TotalTrafficBing)
shapiro.test(search$TotalTrafficBing)
boxplot(search$TotalTrafficBing)
barplot(search$TotalTrafficBing)

#Descriptives for [TotalTraffic] - BEFORE
hist(before$TotalTraffic)
hist(before$TotalTraffic, breaks = 20, main = "Breaks=20")
summary(before$TotalTraffic)
var(before$TotalTraffic)
sd(before$TotalTraffic)
qqnorm(before$TotalTraffic, pch = 1, frame = FALSE)
qqline(before$TotalTraffic)
shapiro.test(before$TotalTraffic)
boxplot(before$TotalTraffic)
barplot(before$TotalTraffic)

#Descriptives for [TotalTraffic] - AFTER
hist(after$TotalTraffic)
hist(after$TotalTraffic, breaks = 20, main = "Breaks=20")
summary(after$TotalTraffic)
var(after$TotalTraffic)
sd(after$TotalTraffic)
qqnorm(after$TotalTraffic, pch = 1, frame = FALSE)
qqline(after$TotalTraffic)
shapiro.test(after$TotalTraffic)
boxplot(after$TotalTraffic)
barplot(after$TotalTraffic)

#Misc. Descriptives
boxplot(before$TotalTraffic, after$TotalTraffic, main = "TotalTraffic before/after")

#scatterplots
scatter.smooth(x=before$TotalTrafficGoogle, y=before$TotalTrafficBing, main="Google ~ Bing")
scatter.smooth(x=before$TotalTrafficBing, y=before$TotalTrafficGoogle, main="Bing ~ Google")
scatter.smooth(x=exoutlier$TotalTrafficBing, y=exoutlier$TotalTrafficGoogle)
scatter.smooth(x=before$OrganicTrafficBing, y=before$OrganicTrafficGoogle, main="Bing organic~ Google organic")

#correlation tests parametric
cor.test(before$TotalTrafficBing, before$TotalTrafficGoogle)
cor.test(before$TotalTrafficGoogle, before$TotalTrafficBing)
cor.test(before$OrganicTrafficBing, before$OrganicTrafficGoogle)
cor.test(before$OrganicTrafficGoogle, before$OrganicTrafficBing)
cor.test(before$TotalTrafficGoogle, before$TotalTrafficBing)

#paired independent samples t-tests
t.test(before$TotalTrafficBing, after$TotalTrafficBing, paired = FALSE)
t.test(exoutlier$TotalTrafficBing, after$TotalTrafficBing, paired = FALSE)
t.test(before$TotalTrafficGoogle, after$TotalTrafficGoogle, paired = FALSE)
t.test(before$TotalTraffic, after$TotalTraffic, paired = FALSE)
t.test(before$OrganicTrafficGoogle, after$OrganicTrafficGoogle, paired = FALSE)
t.test(after$OrganicTrafficGoogle, before$OrganicTrafficGoogle, paired = FALSE)