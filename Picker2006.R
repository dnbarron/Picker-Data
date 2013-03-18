library(foreign)
library(plyr)
library(survival)
library(arm)
library(ggplot2)
setwd('C:/Users/dbarron/Documents/Picker Data/')
am2006 <- read.csv("amelia.2006.a2.csv")

dta2006 <- read.csv("dta2006.csv")
time.vars <- c("Outcome","MainSpec","Mailing_One","Mailing_Two","Mailing_Three","DATEREC")
drp <- c(1:13,15,17,18,46:53,55:59)
drp <- paste("Q",drp,sep="")
ix <- match(c(drp,time.vars),names(dta2006))

time.2006 <- dta2006[,ix]

dta2006 <- data.frame(am2006,time.2006)

mis.dat <- lapply(dta2006, function(x) data.frame(NAs=sum(is.na(x)),N=length(x),
prop.na=round(sum(is.na(x))/length(x),2)))

#dta2006 <- read.dta('Inpatient2006.dta')
# Need to convert SPSS dates to POSIXct
# SPSS dates are seconds since 14/10/1582
date1 <- as.POSIXct("1582-10-14")
# POSIXct dates seconds since 1/1/1970
date2 <- as.POSIXct("1970-01-01")

#Find difference between these dates in days
delta.date <- difftime(date2,date1)
# and convert to seconds
delta.secs <- delta.date*24*60*60

# Subtracting this from SPSS date variables gives POSIXct date
Date.rec.posix <- with(dta2006, DATEREC - delta.secs)
class(Date.rec.posix) <- "POSIXct"
# Remove the "time of day" part of the date
Date.rec <- as.Date(Date.rec.posix)

# Non-returned questionnaires don't have return date, so give them
# last date questionnaire returned as censoring time
#lvl <- levels(dta2006$Outcome)
#ix <- dta2006$Outcome == lvl[6]
#max.date <- max(Date.rec,na.rm=TRUE)
#Date.rec[ix] <- max.date

# Do the same for the date of first mailing
mail1.posix <- with(dta2006, Mailing_One - delta.secs)
class(mail1.posix) <- "POSIXct"
Mailing1 <- as.Date(mail1.posix)

# Do the same for the date of 2nd mailing
mail2.posix <- with(dta2006, Mailing_Two - delta.secs)
class(mail2.posix) <- "POSIXct"
Mailing2 <- as.Date(mail2.posix)

# Do the same for the date of 3rd mailing
mail3.posix <- with(dta2006, Mailing_Three - delta.secs)
class(mail3.posix) <- "POSIXct"
Mailing3 <- as.Date(mail3.posix)

reply.strata <- factor(ifelse(Date.rec <= Mailing2, "Mail1",
                  ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2, "Mail2",
                    ifelse(Date.rec > Mailing3, "Mail3",NA))))

(table(reply.strata, useNA="ifany"))
rep.mail <- factor(ifelse(reply.strata=="Mail2"|reply.strata=="Mail3","repeated",reply.strata))
table(rep.mail,useNA="ifany")         
levels(rep.mail) <- c("First","Repeated")

Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))
Reply.Time[Reply.Time <= 0] <- NA

# How many failed to return questionnaire:
tab1 <- xtabs(~Outcome,dta2006)
#Returned = 80694 out of 141,447 = 57%

overall <- dta2006$Q66[, drop=TRUE]
tab2 <- xtabs(~Q66,dta2006, exclude=NULL)

overall.num <- as.numeric(overall)
tab3 <- table(overall, useNA="ifany")
dta2006 <- data.frame(dta2006,Reply.Time, Overall.Sat=overall.num,Date.rec,Mailing1)
dta2006$Reply.Time.num <- as.double(dta2006$Reply.Time)
dta2006 <- transform(dta2006,LogReplyTime = log(Reply.Time.num))

overall.tri <- factor(with(dta2006, ifelse(Overall.Sat==1,1,
ifelse(Overall.Sat==2,2,
ifelse(is.na(Overall.Sat), NA, 3)))),labels=c("Excellent","Very good","Other"))

dta2006$overall.tri <- overall.tri

dta2006$emergency <- with(dta2006,factor(ifelse(Q1=="Emergency or urgent","yes","no")))
dta2006$Choice <- with(dta2006,factor(ifelse(Q10=="Yes","Yes","No")))
dta2006$Overall.Dic <- with(dta2006,factor(ifelse(Overall.Sat<4,"Good","Poor.Fair")))
dta2006$LOS <- with(dta2006,ifelse(LOS<1,1,LOS))
dta2006$LogLOS <- with(dta2006,log(LOS))
dta2006$Reply.Time <- with(dta2006,as.numeric(Reply.Time))
dta2006$Reply.Time <- with(dta2006,ifelse(Reply.Time<0,NA,Reply.Time))
dta2006$Overall.Score <- with(dta2006,ifelse(Overall.Sat==1, 100,
                                        ifelse(Overall.Sat==2, 75,
                                         ifelse(Overall.Sat==3, 50,
                                          ifelse(Overall.Sat==4, 25, 0)))))
dta2006$Dignity <- with(dta2006, factor(ifelse(Q64=="Yes, always","Always","Sometimes or no")))
dta2006 <- transform(dta2006, gender=Q69)


 write.csv(dta2006,"dta2006.csv")

################
## Start here
##############
library(foreign)
library(plyr)
library(survival)
library(arm)
library(ggplot2)
setwd('C:/Users/dbarron/Documents/Picker Data/')
am2006 <- read.csv("amelia.2006.a2.csv")
 dta2006 <- read.csv("dta2006.csv")

Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))
Reply.Time[Reply.Time <= 0] <- NA
with(dta2006, {
    Reply.Time2 <- Date.rec - Mailing2
    Reply.Time3 <- Date.rec - Mailing3
})

mail2.posix <- with(dta2006, Mailing_Two - delta.secs)
class(mail2.posix) <- "POSIXct"
Mailing2 <- as.Date(mail2.posix)
Reply.Time2 <- as.numeric(Date.rec - Mailing2)
Reply.Time3 <- as.numeric(Date.rec - Mailing3)
Reply.Time2 <- ifelse(reply.strata=="Mail2",Reply.Time2,NA)
Reply.Time3 <- ifelse(reply.strata=="Mail3",Reply.Time3,NA)
Reply.Time1 <- ifelse(reply.strata=="Mail1",Reply.Time,NA)

dta2006 <- data.frame(dta2006,reply.strata,Reply.Time2,Reply.Time3)
dta2006$Reply.Time2 <- Reply.Time2
dta2006$Reply.Time3 <- Reply.Time3
dta2006$Reply.Time1 <- Reply.Time1

g <- ggplot(data=dta2006)
g + geom_histogram(aes(x=Reply.Time1))

# Do the same for the date of 3rd mailing
mail3.posix <- with(dta2006, Mailing_Three - delta.secs)
class(mail3.posix) <- "POSIXct"
Mailing3 <- as.Date(mail3.posix)

reply.strata <- factor(ifelse(Date.rec <= Mailing2, "Mail1",
                  ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2, "Mail2",
                    ifelse(Date.rec > Mailing3, "Mail3",NA))))
with(dta2006, Mailing2-Mailing1)
(table(reply.strata, useNA="ifany"))
rep.mail <- with(dta2006,factor(ifelse(reply.strata=="Mail2"|reply.strata=="Mail3","repeated",reply.strata)))
table(rep.mail,useNA="ifany")         
levels(rep.mail) <- c("First","Repeated")
early <- factor(with(dta2006,ifelse(Reply.Time <= 14, "Early",ifelse(Reply.Time>14,"Late",NA))))
table(early)
dta2006$early <- early
dta2006$rep.mail <- rep.mail

g <- ggplot(data=dta2006, aes(x=Reply.Time.num))
g + geom_histogram(aes(y=..density..), fill="blue") + xlab("Time to reply (days)")
g + geom_boxplot(data=subset(dta2006,!is.na(overall.tri)),aes(x=overall.tri,y=Reply.Time.num)) + xlab("Overall satisfaction") + ylab("Time to reply (days)")

g + geom_histogram(aes(x=log(Reply.Time.num),y=..density..),fill="blue") + xlab("Log time to reply (days)")
# Question: are their significant differences in response times based on satisfaction

a1 <- aov(Reply.Time.num ~ factor(Q66), data=dta2006)
summary(a1)
# Yes, highly siginficant, F = 98.2, df=4,77346

l1 <- lm(Reply.Time.num ~  factor(Q66), data=dta2006)
summary(l1)
c(coef(l1)[1],coef(l1)[1]+coef(l1)[2:5])
# Shows that effects of Good, Fair and Poor very similar
# Predicted reply for Excellent = 18.7, V. Good = 20.0, 
# Good = 21.6, Fair = 21.5, Poor = 21.1

#overall.tri <- reorder(overall.tri,new.order=c("Excellent","Very good","Other"))
tab4 <- with(dta2006,table(overall.tri, useNA="ifany"))
tab4

l2 <- lm(Reply.Time.num ~ overall.tri, dta2006)
summary(l2)

b.l2 <- coef(l2)
c(b.l2[1], b.l2[1]+b.l2[2:3])
# Excellent = 18.7, V. good = 20.0, other = 21.5

# Try log time
l3 <- lm(log(Reply.Time3) ~ overall.tri, dta2006)
summary(l3)
b.l3 <- coef(l3)
exp(c(b.l3[1], b.l3[1]+b.l3[2:3]))

# Excellent = 14.1, V. good = 15.2, other = 16.3

# Response rates
rr <- with(dta2006,tapply(Overall.Sat,trustcode,function(x) sum(!is.na(x))/length(x)))
rrd <- data.frame(Trustcode=names(rr),ResponseRates=as.vector(rr))
dta2006a <- merge(dta2006,rrd,all.x=TRUE,by.x="trustcode",by.y="Trustcode")

## plots & descriptive stats
xtabs(~ reply.strata, dta2006)
xtabs(~rep.mail,dta2006)
xtabs(~early,dta2006) # early is within 14 days of first mailing

earl1 <- lmer(early ~ overall.tri + (1|trustcode), family="binomial", data=dta2006)
display(earl1)
earl2 <- glm(early ~ overall.tri +  emergency + Choice + age_group + LogLOS + gender, family="binomial", data=dta2006)
display(earl2)

rep1 <- lmer(rep.mail ~ overall.tri + (1|trustcode), family="binomial", data=dta2006)
display(rep1)
rep2 <- lmer(rep.mail ~ overall.tri +  emergency + Choice + age_group + LogLOS + gender + (1|trustcode), family="binomial", data=dta2006)
display(rep2)
b <- fixef(rep1)
b2 <- fixef(rep2)
inv.logit <- function (x, min = 0, max = 1) 
{
    p <- exp(x)/(1 + exp(x))
    p <- ifelse(is.na(p) & !is.na(x), 1, p)
    p * (max - min) + min
}
inv.logit(b2[1] + b[2])

predict(rep1,newdata=data.frame(overall.tri=c("Excellent","Very good","Other")),type="response")

RepTime <- with(dta2006,ifelse(reply.strata=="Mail1",Reply.Time1,
   ifelse(reply.strata=="Mail2",Reply.Time2,
   ifelse(reply.strata=="Mail3",Reply.Time3,NA))))
plotdata <- data.frame(Reply.Time=RepTime,Mailing=dta2006$reply.strata)
levels(plotdata$Mailing) <- c("First","Second","Third")
g <- ggplot(data=plotdata)
g + geom_histogram(aes(x=Reply.Time,y=..density..,fill=Mailing),position="dodge") + xlab("Time to reply (days)") + ylab("Density") + xlim(c(0,40))
g + geom_density(aes(x=Reply.Time,colour=Mailing)) + xlab("Time to reply (days)") + ylab("Density") + xlim(c(0,40))

lm.1 <- lmer(Reply.Time ~ overall.tri + (1|trustcode), data=dta2006)
summary(lm.1)
lm2006.1 <- lmer(Reply.Time1 ~ overall.tri + (1|trustcode), data=dta2006)
lm2006.2 <- lmer(Reply.Time2 ~ overall.tri + (1|trustcode), data=dta2006)
lm2006.3 <- lmer(Reply.Time3 ~ overall.tri + (1|trustcode), data=dta2006)
summary(lm2006.1)
summary(lm2006.2)
summary(lm2006.3)

lm2006.f <- lmer(Reply.Time ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006)
lm2006.1f <- lmer(Reply.Time1 ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006)
lm2006.2f <- lmer(Reply.Time2 ~ overall.tri +  emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006)
lm2006.3f <- lmer(Reply.Time3 ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006)
summary(lm2006.f)
summary(lm2006.1f)
summary(lm2006.2f)
summary(lm2006.3f)


lm.l1 <- lmer(log(Reply.Time) ~ overall.tri + (1|trustcode), data=dta2006)
summary(lm.l1)
lm2006 <- lm(Reply.Time ~ overall.tri + emergency + Choice + age_group + LogLOS + gender, data=dta2006)
summary(lm2006)
lm2006.l <- lm(log(Reply.Time) ~ overall.tri + emergency + Choice + age_group + LogLOS + gender, data=dta2006)
summary(lm2006.l1)

lm2006.l1 <- lmer(log(Reply.Time1) ~ overall.tri + (1|trustcode), data=dta2006)
lm2006.l2 <- lmer(log(Reply.Time2) ~ overall.tri + (1|trustcode), data=dta2006)
lm2006.l3 <- lmer(log(Reply.Time3) ~ overall.tri + (1|trustcode), data=dta2006)
summary(lm2006.l1)
summary(lm2006.l2)

g0.hat <- fixef(lm2006.l1)["(Intercept)"]
b.1.hat <- fixef(lm2006.l1)["overall.triOther"]
b.2.hat <- fixef(lm2006.l1)["overall.triVery good"]
sigma.a.hat <- sigma.hat(lm2006.l1)$sigma$trustcode
sigma.y.hat <- sigma.hat(lm2006.l1)$sigma$data
n.sims <- 1000
a.tilde <- rnorm (n.sims, g0.hat, sigma.a.hat)
y.tilde.exc <- rnorm (n.sims, a.tilde , sigma.y.hat)
y.tilde.vg <- rnorm (n.sims, a.tilde + b.2.hat , sigma.y.hat)
y.tilde.oth <- rnorm (n.sims, a.tilde + b.1.hat , sigma.y.hat)

simdata <- data.frame(Predictions=c(y.tilde.exc,y.tilde.vg,y.tilde.oth),Response=gl(3,100,labels=c("Excellent","Very good","Other")))
gg <- ggplot(data=simdata) + geom_boxplot(aes(x=Response,y=(Predictions)))
gg
lm2006.l1 <- lm(log(Reply.Time1) ~ overall.tri + emergency + Choice  + age_group + LogLOS + gender, data=dta2006)
summary(lm2006.l1)
lm2006.l2 <- lm(log(Reply.Time2) ~ overall.tri + emergency + Choice  + age_group + LogLOS + gender, data=dta2006)
summary(lm2006.l2)
lm2006.l3 <- lm(log(Reply.Time3) ~ overall.tri + emergency + Choice  + age_group + LogLOS + gender, data=dta2006)
summary(lm2006.l3)

ml2006 <- lmer(Reply.Time ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006a)
display(ml2006)

g1 <- glm(rep.mail ~ overall.tri , family=binomial, data=dta2006a)
summary(g1)
predict(g1,data.frame(overall.tri=c("Excellent","Very good","Other")),type="response")
prop.table(xtabs(~rep.mail+overall.tri),1)

g2 <- lmer(rep.mail ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006, family="binomial")
display(g2)
predict(g2,data.frame(overall.tri=c("Excellent","Very good","Other")),type="response")

prop.table(xtabs(~reply.strata + overall.tri, dta2006),2)


fit.g2 <- fitted(g2)

rp1 <- rpart(Q66 ~ Q20 + Q21 + Q22 + Q23 + Q24 + Q26 + Q27 + Q28 + Q29 + Q30 + Q31 + Q32 + Q33 + Q35 + Q36 + Q37 + Q40 + Q41 + Q45 + Q64 + Q65, data=am2006, method="anova")