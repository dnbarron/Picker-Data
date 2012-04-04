####################################################
## Analysis for time to return paper
###  Just uses 2009 data
####  David Barron, 4 April 2012
##########################################

library(foreign)
library(plyr)
library(survival)
library(arm)
library(ggplot2)

setwd('C:/Users/dbarron/Documents/Picker Data/')
#load("~/Picker Data/.RData")
dta2009 <- read.dta('Inpatient2009.dta')

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
Date.rec.posix <- with(dta2009, DATEREC - delta.secs)
class(Date.rec.posix) <- "POSIXct"
# Remove the "time of day" part of the date
Date.rec <- as.Date(Date.rec.posix)

# Do the same for the date of first mailing
mail1.posix <- with(dta2009, Mailing1 - delta.secs)
class(mail1.posix) <- "POSIXct"
Mailing1 <- as.Date(mail1.posix)

Reply.Time <- Date.rec - Mailing1

# Do the same for the date of 2nd mailing
mail2.posix <- with(dta2009, Mailing2 - delta.secs)
class(mail2.posix) <- "POSIXct"
dta2009$Mailing2 <- as.Date(mail2.posix)
#dta$Mailing2 <- as.Date(as.character(dta$Mailing2))

# Do the same for the date of 3rd mailing
mail3.posix <- with(dta2009, Mailing3 - delta.secs)
class(mail3.posix) <- "POSIXct"
dta2009$Mailing3 <- as.Date(mail3.posix)

dta2009$Reply.Time <- Reply.Time


overall <- dta2009$Q74[, drop=TRUE]
overall.ord <- as.ordered(overall)
overall.num <- as.numeric(overall)
table(overall, useNA="ifany")
dta2009 <- data.frame(dta2009, Overall.Sat=overall.num)


ddply(dta2009,"Trustcode",summarize,max(Date.rec,na.rm=TRUE))

max.date <- max(dta2009$DATEREC,na.rm=TRUE)
#  Replace missing date received with last recorded date
outcome.levels <- levels(dta2009$Outcome)

new.outcome <- with(dta2009,ifelse(Outcome==outcome.levels[1],"Returned",
                              ifelse(Outcome==outcome.levels[6],"Not returned",NA)))
new.Date.rec <- ifelse(new.outcome=="Not returned",max.date[[1]],dta2009$DATEREC)
table(new.outcome)

reply.strata <- with(dta2009,factor(ifelse(Date.rec <= Mailing2, "Mail1",
                                           ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2, "Mail2",
                                                  ifelse(Date.rec > Mailing3, "Mail3",NA)))))

dta2009$Reply.Time2 <- Date.rec - dta2009$Mailing2
dta2009$Reply.Time3 <- Date.rec - dta2009$Mailing3         

dta2009$Reply.Time2 <- ifelse(reply.strata=="Mail2",dta2009$Reply.Time2,NA)
dta2009$Reply.Time3 <- ifelse(reply.strata=="Mail3",dta2009$Reply.Time3,NA)
dta2009$Reply.Time1 <- ifelse(reply.strata=="Mail1",dta2009$Reply.Time,NA)

# End here

vars.used <- c("Outcome.x","censored","age_group","RECONVER","LOS.x",
               "LogLOS","Reply.Time1","Reply.Time2","Reply.Time3",
               "reply.strata","Choice","emergency","Mailing1",
               "Mailing2","Mailing3","overall.tri","Trustcode.x","early")

dta2009.used <- subset(dta2009,select=vars.used)



############### current end

cc <- complete.cases(dta2009.used[,-c(7,8,9)])
ss <- ggplot(data=dta2009.a,aes(x=RECONVER))
ss +  geom_histogram(fill="lightblue") + xlab("Single score") + ylab("Count")

time.vars <- c("Outcome","MainSpec","Mailing1","Mailing2","Mailing3","DATEREC")
kp <- c("Trustcode","Record","LOS","DisSpec","Outcome","Q1","Q64","Q66","Q69","Q70","Q71")

ix <- match(c(kp,time.vars),names(dta))




# Check this is correct variable ****************************************
xtabs(~Q74, dta)
overall <- dta$Q74[, drop=TRUE]
overall.num <- as.numeric(overall)
table(overall, useNA="ifany")
dta <- data.frame(dta,Reply.Time, Overall.Sat=overall.num)
rm(Reply.Time)


mis.dat <- lapply(dta2006, function(x) data.frame(NAs=sum(is.na(x)),N=length(x),
prop.na=round(sum(is.na(x))/length(x),2)))




#dta2009$Mailing3 <- as.Date(as.character(dta2009$Mailing3))
#dta2009$Date.rec <- as.Date(as.character(dta2009$Date.rec))

reply.strata <- with(dta2009,factor(ifelse(Date.rec <= Mailing2, "Mail1",
                  ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2, "Mail2",
                    ifelse(Date.rec > Mailing3, "Mail3",NA)))))

(table(reply.strata, useNA="ifany"))
rep.mail <- factor(ifelse(reply.strata=="Mail2"|reply.strata=="Mail3","repeated",reply.strata))
table(rep.mail,useNA="ifany")
levels(rep.mail) <- c("First","Repeated")

Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))
Reply.Time[Reply.Time <= 0] <- NA

# How many failed to return questionnaire:
tab1 <- xtabs(~Outcome,dta)
#Returned = 69348 out of 137360 = 50%

overall <- dta$Q74[, drop=TRUE]
tab2 <- xtabs(~Q74,dta, exclude=NULL)

overall.num <- as.numeric(overall)
tab3 <- table(overall, useNA="ifany")
dta <- data.frame(dta,Reply.Time, Overall.Sat=overall.num,Date.rec,Mailing1)

dta$Reply.Time.num <- as.double(dta$Reply.Time)
dta <- transform(dta,LogReplyTime = log(Reply.Time.num))

overall.tri <- factor(with(dta, ifelse(Overall.Sat==1,1,
ifelse(Overall.Sat==2,2,
ifelse(is.na(Overall.Sat), NA, 3)))),labels=c("Excellent","Very good","Other"))

dta$overall.tri <- overall.tri

dta2009 <- dta
dta2009$emergency <- with(dta2009,factor(ifelse(Q1=="Emergency or urgent","yes","no")))
dta2009$Choice <- with(dta2009,factor(ifelse(Q10=="Yes","Yes","No")))
dta2009$Overall.Dic <- with(dta2009,factor(ifelse(Overall.Sat<4,"Good","Poor.Fair")))
dta2009$LOS <- with(dta2009,ifelse(LOS<1,1,LOS))
dta2009$LogLOS <- with(dta2009,log(LOS))
dta2009$Reply.Time <- with(dta2009,as.numeric(Reply.Time))
dta2009$Reply.Time <- with(dta2009,ifelse(Reply.Time<0,NA,Reply.Time))

dta2009$Overall.Score <- with(dta2009,ifelse(Overall.Sat==1, 100,
                                        ifelse(Overall.Sat==2, 75,
                                         ifelse(Overall.Sat==3, 50,
                                          ifelse(Overall.Sat==4, 25, 0)))))

#dta2009$Dignity <- with(dta2009, factor(ifelse(Q64=="Yes, always","Always","Sometimes or no")))
dta2009 <- transform(dta2009, gender=all_gender)


 write.csv(dta2009,"dta2009.csv")
dta2009 <- read.csv("dta2009.csv")
rm(dta)
dta2009$new.outcome <- new.outcome
dta2009.ss <- read.dta("inpatient2009.subset.dta")
dta2009.ss$pid <- with(dta2009.ss,paste(Trustcode,Record,sep="."))
dta2009$pid <- with(dta2009,paste(Trustcode,Record,sep="."))
dta2009.a <- merge(dta2009,dta2009.ss,by="pid")
rm(dta2009.ss)
reply.strata <- with(dta2009.a,factor(ifelse(Date.rec <= Mailing2, "Mail1",
                  ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2, "Mail2",
                    ifelse(Date.rec > Mailing3, "Mail3",NA)))))
dta2009.a$reply.strata <- reply.strata


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
 dta2009 <- read.csv("dta2009.csv")

Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))
Reply.Time[Reply.Time <= 0] <- NA


    dta2009$Reply.Time2 <- Date.rec - dta2009$Mailing2
    dta2009$Reply.Time3 <- Date.rec - dta2009$Mailing3         
    dta2009$Reply.Time2 <- ifelse(reply.strata=="Mail2",dta2009$Reply.Time2,NA)
    dta2009$Reply.Time3 <- ifelse(reply.strata=="Mail3",dta2009$Reply.Time3,NA)
    dta2009$Reply.Time1 = ifelse(reply.strata=="Mail1",dta2009$Reply.Time,NA

mail2.posix <- with(dta2006, Mailing_Two - delta.secs)
class(mail2.posix) <- "POSIXct"
Mailing2 <- as.Date(mail2.posix)
Reply.Time2 <- as.numeric(Date.rec - Mailing2)
Reply.Time3 <- as.numeric(Date.rec - Mailing3)
Reply.Time2 <- ifelse(reply.strata=="Mail2",Reply.Time2,NA)
Reply.Time3 <- ifelse(reply.strata=="Mail3",Reply.Time3,NA)
Reply.Time1 <- ifelse(reply.strata=="Mail1",Reply.Time,NA)

dta2009$censored <- with(dta2009, ifelse(new.outcome=="Returned",1,
                                    ifelse(new.outcome=="Not returned",0,NA)))
surv2009 <- Surv(as.double(dta2009.a$Reply.Time), dta2009.a$censored)

dta2009 <- data.frame(dta2009,reply.strata)
write.csv(dta2009,"dta2009.csv")

dta2006$Reply.Time2 <- Reply.Time2
dta2006$Reply.Time3 <- Reply.Time3
dta2006$Reply.Time1 <- Reply.Time1

g <- ggplot(data=dta2006)
g + geom_histogram(aes(x=Reply.Time1))

# Do the same for the date of 3rd mailing
mail3.posix <- with(dta2006, Mailing_Three - delta.secs)
class(mail3.posix) <- "POSIXct"
Mailing3 <- as.Date(mail3.posix)

reply.strata <- with(dta2009,factor(ifelse(Date.rec <= Mailing2, "Mail1",
                  ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2, "Mail2",
                    ifelse(Date.rec > Mailing3, "Mail3",NA)))))

with(dta2006, Mailing2-Mailing1)
(table(reply.strata, useNA="ifany"))

ddply(dta2009.a,"reply.strata",summarise,mean(RECONVER,na.rm=TRUE))
summarize(dta2009.a,mean(RECONVER,na.rm=TRUE))

library(descr)
with(dta2009.a,CrossTable(censored,age_group))
with(dta2009.a,CrossTable(new.outcome,age_group))


xtabs(~new.outcome + age_group,dta2009.a)
summary(xtabs(~censored + age_group,dta2009.a))

rep.mail <- with(dta2009.a,factor(ifelse(reply.strata=="Mail2"|reply.strata=="Mail3","repeated",reply.strata)))
table(rep.mail,useNA="ifany")
levels(rep.mail) <- c("First","Repeated")
dta2009.a$rep.mail <- rep.mail

early <- factor(with(dta2009.a,ifelse(Reply.Time <= 14, "Early",ifelse(Reply.Time>14,"Late",NA))))
table(early)
dta2009.a$early <- early
dta2009$rep.mail <- rep.mail
dta2009.a$Reply.time.num <- as.numeric(dta2009.a$Reply.Time)
g <- ggplot(data=dta2009.a, aes(x=as.numeric(Reply.Time)))
g + geom_histogram(aes(y=..density..), fill="blue") + xlab("Time to reply (days)")
g + geom_boxplot(data=subset(dta2009.a,!is.na(overall.tri)),aes(x=overall.tri,y=Reply.Time.num)) + xlab("Overall satisfaction") 
+ ylab("Time to reply (days)")

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

odds.ratio <- function(fit){
    b <- fixef(fit)
    se <- sqrt(diag(vcov(fit)))
    z <- b/se
    or <- exp(b)
    se.or <- abs(or/z)
    list(odds.ratio=or,stand.err=se.or)
}
library(descr)
with(dta2009.a,CrossTable(age_group,rep.mail,expected=FALSE,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE,chisq=TRUE))
with(dta2009.a,CrossTable(all_gender,rep.mail,expected=FALSE,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE,chisq=TRUE))
with(dta2009.a,CrossTable(emergency,rep.mail,expected=FALSE,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE,chisq=TRUE))
with(dta2009.a,{
    cat(mean(Reply.Time1,na.rm=TRUE),
    mean(Reply.Time2,na.rm=TRUE),
    mean(Reply.Time3,na.rm=TRUE),sep="\n")
})
t.test(LOS.x ~ rep.mail,dta2009.a)
summary(xtabs(~age_group+early,dta2009.a))
summary(aov(LOS.x ~ overall.tri,data=dta2009.a))
summary(xtabs(~age_group+rep.mail,dta2009.a))

earl1 <- lmer(early ~ RECONVER + (1|Trustcode.x), family="binomial", data=dta2009.a)
earl1a <- glm(early ~ overall.tri + cluster(Trustcode), family="binomial", data=dta2009)
display(earl1,digits=3)
display(earl1a)

earl2 <- glm(early ~ overall.tri +  emergency + Choice + age_group + LogLOS + gender, family="binomial", data=dta2006)
display(earl2)

rep1 <- lmer(rep.mail ~ overall.tri + (1|Trustcode), family="binomial", data=dta2009)
display(rep1,digits=3)
rep2 <- lmer(rep.mail ~ overall.tri +  emergency + age_group + LogLOS + gender + (1|Trustcode), family="binomial", data=dta2009)
display(rep2,digits=3)
b <- fixef(rep1)
b2 <- fixef(rep2)
inv.logit <- function (x, min = 0, max = 1)
{
    p <- exp(x)/(1 + exp(x))
    p <- ifelse(is.na(p) & !is.na(x), 1, p)
    p * (max - min) + min
}
inv.logit(b[1]+b[3])

predict(rep1,newdata=data.frame(overall.tri=c("Excellent","Very good","Other")),type="response")

RepTime <- with(dta2009,ifelse(reply.strata=="Mail1",Reply.Time1,
   ifelse(reply.strata=="Mail2",Reply.Time2,
   ifelse(reply.strata=="Mail3",Reply.Time3,NA))))
plotdata <- data.frame(Reply.Time=RepTime,Mailing=dta2009$reply.strata)
levels(plotdata$Mailing) <- c("First","Second","Third")
g <- ggplot(data=plotdata)
 g + geom_histogram(aes(x=Reply.Time,y=..density..,fill=Mailing),position="dodge") + xlab("Time to reply (days)") + 
 ylab("Density") + xlim(c(0,40))

dta2009$overall.tri <- factor(dta2009$overall.tri, levels=c("Excellent","Very good","Other"),ordered=FALSE)
vns <- c("Reply.Time","age_group","LOS","emergency","Choice")
mean(dta2009[,vns],na.rm=TRUE)
xtabs(~age_group+reply.strata,dta2009)
xtabs(~gender+reply.strata,dta2009)
xtabs(~emergency+reply.strata,dta2009)
xtabs(~Choice+reply.strata,dta2009)

lm.b <- lmer(Reply.Time ~ 1 + (1|Trustcode), data=dta2009)
display(lm.b,digits=3)
lm.1 <- lmer(Reply.Time ~ overall.tri + (1|Trustcode.x), data=dta2009.a)
summary(lm.1)
lm2009.1 <- lmer(Reply.Time1 ~ overall.tri + (1|Trustcode), data=dta2009)
lm2009.2 <- lmer(Reply.Time2 ~ overall.tri + (1|Trustcode), data=dta2009)
lm2009.3 <- lmer(Reply.Time3 ~ overall.tri + (1|Trustcode), data=dta2009)
display(lm2009.1,digits=3)
display(lm2009.2,digits=3)
display(lm2009.3,digits=3)


### Full regressions
lm2009.f <- lmer(Reply.Time ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|Trustcode), data=dta2009)
lm2009.1f <- lmer(Reply.Time1 ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|Trustcode), data=dta2009)
lm2009.2f <- lmer(Reply.Time2 ~ overall.tri +  emergency + Choice + age_group + LogLOS + gender + (1|Trustcode), data=dta2009)
lm2009.3f <- lmer(Reply.Time3 ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|Trustcode), data=dta2009)
display(lm2009.f, digits=3)
display(lm2009.1f, digits=3)
display(lm2009.2f, digits=3)
display(lm2009.3f, digits=3)

tmp <- data.frame(Reply.Time=dta2009$Reply.Time,censored=dta2009$censored)
ix <- complete.cases(tmp)
tmp <- tmp[ix,]
# Hazard rate plots
library(muhaz)
mufit <- muhaz(tmp$Reply.Time,tmp$censored,max.time=70,bw.method="g")
plot(mufit,xlim=c(0,60),ylim=c(0,.1),xlab="Time to reply (days)")
pefit <- pehaz(tmp$Reply.Time,tmp$censored,max.time=70,width=1)
plot(pefit)
summary(pefit)
fit <- survfit(surv2009 ~ 1, data=dta2009,type="fh")
library(epiR)
fit2 <- epi.insthaz(fit)
hp <- ggplot(data=fit2,aes(x=time,y=est)) + geom_line() + geom_point() + xlab("Time to reply (days)") + ylab("Hazard rate")
hp + scale_x_continuous(limits=c(0,70)) + scale_y_continuous(limits=c(0,.15))
plot(fit2$time,fit2$est,type="b",xlim=c(0,70),ylim=c(0,.13),pch=16,xlab="Time to reply (days)",ylab="Estiamted hazard rate")
lines(fit2$time,fit2$lower)
lines(fit2$time,fit2$upper)
plot(fit)
plot(fit, col=c("green","blue","red"),xlim=c(0,60),xlab="Time to reply (days)")
legend("topright",legend=levels(dta2009$overall.tri),lty=1,col=c("green","blue","red"))

cox2009.1 <- coxph(surv2009 ~ overall.tri + cluster(Trustcode), data=dta2009, x=TRUE)
summary(cox2009.1)
zph2009.1 <- cox.zph(cox2009.1)
plot(zph2009.1)
coxhat <- predict(cox2009.1,type="expected")#,newdata=data.frame(overall.tri=levels(dta2009$overall.tri)))
by(coxhat,cox2009.1$x[,1],mean)

fit.ph <- survfit(cox2009.1, newdata=data.frame(overall.tri=levels(dta2009$overall.tri)))
plot(fit.ph,col=c("green","blue","red"),xlim=c(0,60),xlab="Time to reply (days)")
legend("topright",legend=levels(dta2009$overall.tri),lty=1,col=c("green","blue","red"))

cox2009 <- coxph(surv2009 ~ overall.tri + emergency + Choice + age_group + log(LOS) + all_gender + cluster(Trustcode), data=dta2009)
summary(cox2009)
fit.ph.f <- survfit(cox2009)
plot(fit.ph.f)
legend("topright",legend=levels(dta2009$overall.tri),lty=1,col=c("green","blue","red"))

exp2009.0 <- survreg(surv2009 ~ cluster(Trustcode), data=dta2009)
summary(exp2009.0)

exp2009.1 <- survreg(surv2009 ~ overall.tri + cluster(Trustcode), data=dta2009)
summary(exp2009.1)
predict(exp2009.1,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))
weib2009.ss <- survreg(Surv(Reply.Time,censored) ~ RECONVER + cluster(Trustcode.x), data=dta2009.a)
summary(weib2009.ss)
exp2009.11 <- survreg(Surv(Reply.Time1,censored) ~ overall.tri + cluster(Trustcode), data=dta2009)
summary(exp2009.1)
predict(exp2009.11,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))

exp2009.2 <- survreg(Surv(Reply.Time2,censored) ~ overall.tri + cluster(Trustcode), data=dta2009)
summary(exp2009.2)
predict(exp2009.2,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))

exp2009.3 <- survreg(Surv(Reply.Time3,censored) ~ overall.tri + cluster(Trustcode), data=dta2009)
summary(exp2009.3)
predict(exp2009.3,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))



exp2009 <- survreg(surv2009 ~ overall.tri + emergency  + age_group + log(LOS) + all_gender + cluster(Trustcode), data=dta2009)
summary(exp2009)

exp2009.f1 <- survreg(Surv(Reply.Time1,censored) ~ overall.tri + emergency  + age_group + log(LOS) + all_gender + cluster(Trustcode), data=dta2009)
summary(exp2009.f1)
exp2009.f2 <- survreg(Surv(Reply.Time2,censored) ~ overall.tri + emergency   + age_group + log(LOS) + all_gender + cluster(Trustcode), data=dta2009)
summary(exp2009.f2)
exp2009.f3 <- survreg(Surv(Reply.Time3,censored) ~ overall.tri + emergency   + age_group + log(LOS) + all_gender + cluster(Trustcode), data=dta2009)
summary(exp2009.f3)


predict(exp2009,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))


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

simdata <- data.frame(Predictions=c(y.tilde.exc,y.tilde.vg,y.tilde.oth),Response=gl(3,100,labels=c("Excellent","Very 
good","Other")))
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

g2 <- lmer(rep.mail ~ overall.tri + emergency + Choice + age_group + LogLOS + gender + (1|trustcode), data=dta2006, 
family="binomial")
display(g2)
predict(g2,data.frame(overall.tri=c("Excellent","Very good","Other")),type="response")

prop.table(xtabs(~reply.strata + overall.tri, dta2006),2)


fit.g2 <- fitted(g2)

rp1 <- rpart(Q66 ~ Q20 + Q21 + Q22 + Q23 + Q24 + Q26 + Q27 + Q28 + Q29 + Q30 + Q31 + Q32 + Q33 + Q35 + Q36 + Q37 + Q40 + Q41 + 
Q45 + Q64 + Q65, data=am2006, method="anova") 

# Descriptive stats
xtabs(~gender+Outcome,dta2006)
reminder1 <- with(dta2009,Mailing2-Mailing1)
hist(as.numeric(reminder1))
reminder2 <- with(dta2009,Mailing3-Mailing1)
hist(as.numeric(reminder2))