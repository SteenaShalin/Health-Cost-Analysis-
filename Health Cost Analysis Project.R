library(readxl)
a<-read_excel("C:/Users/Admin/Downloads/1555054100_hospitalcosts.xlsx")
View(a)
summary(a)

hist(a$AGE, main = "Histogram for age frequency", xlab = "Age Group", ylab = "Frequency of Patients", prob= TRUE, col = "red")
lines(density(a$AGE))
summary(as.factor(a$AGE))
x <- aggregate(TOTCHG~AGE,FUN = sum,data = a)
x
max(x)

which.max(summary(as.factor(a$APRDRG)))

diagnosiscost <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = a)
diagnosiscost
diagnosiscost[which.max(diagnosiscost$TOTCHG),]

summary(as.factor(a$RACE))
head(a)
a<-na.omit(a)
a$RACE<-as.factor(a$RACE)
b<- aov(TOTCHG ~ RACE, data = a)
b
summary(b)
summary(a$RACE)

model1 <- lm(TOTCHG ~ AGE + FEMALE, data = a)
a$FEMALE<-as.factor(a$FEMALE)
model1 <- lm(TOTCHG ~ AGE + FEMALE, data = a)
summary(model1)
summary(a$FEMALE)
head(a)



a$RACE<-as.factor(a$RACE)
model2 <- lm(TOTCHG ~ AGE + FEMALE + RACE, data = a)
summary(model2)


model3 <- lm(TOTCHG ~ ., data = a)
summary(model3)