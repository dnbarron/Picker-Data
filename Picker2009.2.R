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

#setwd('C:/Users/dbarron/Documents/Picker Data/')
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
dta2009$Date.rec <- Date.rec
rm(Date.rec)

# Do the same for the date of first mailing
mail1.posix <- with(dta2009, Mailing1 - delta.secs)
class(mail1.posix) <- "POSIXct"
dta2009$Mailing1 <- as.Date(mail1.posix)


# Do the same for the date of 2nd mailing
mail2.posix <- with(dta2009, Mailing2 - delta.secs)
class(mail2.posix) <- "POSIXct"
dta2009$Mailing2 <- as.Date(mail2.posix)
#dta$Mailing2 <- as.Date(as.character(dta$Mailing2))

# Do the same for the date of 3rd mailing
mail3.posix <- with(dta2009, Mailing3 - delta.secs)
class(mail3.posix) <- "POSIXct"
dta2009$Mailing3 <- as.Date(mail3.posix)

#dta2009$Reply.Time <- as.numeric(Reply.Time)

overall <- dta2009$Q74[, drop=TRUE]
overall.ord <- as.ordered(overall)
overall.num <- as.numeric(overall)
#table(overall, useNA="ifany")
dta2009 <- data.frame(dta2009, Overall.Sat=overall.num)


#ddply(dta2009,"Trustcode",summarize,max(Date.rec,na.rm=TRUE))
# Should I use different max.date for each trust?

max.date <- max(dta2009$Date.rec,na.rm=TRUE)
#  Replace missing date received with last recorded date
outcome.levels <- levels(dta2009$Outcome)

# Not retured reason unknown or opted out or not returned unknown reason counted
# as not returned. All other non-returns treated as missing
## Gives response rate of 52%

dta2009$new.outcome <- with(dta2009,ifelse(Outcome==outcome.levels[1],"Returned",
                              ifelse(Outcome==outcome.levels[6]|Outcome==outcome.levels[4],"Not returned",NA)))
tmp <- table(new.outcome)
tmp/sum(tmp)

# Create received date as max.date for censoring purposes
new.Date.rec <- ifelse(new.outcome=="Not returned",max.date,dta2009$Date.rec)
dta2009$new.Date.rec <- as.Date(new.Date.rec,date2)

## List some data
dta2009[1:50,c("Date.rec","new.Date.rec","new.outcome","Outcome","Overall.Sat")]

# Query: why are some cases with valid rec.Date coded as Not Returned?
# These were returned blank questionnaire

dta2009$reply.strata <- with(dta2009,factor(ifelse(Date.rec <= Mailing2&Outcome=="Returned useable questionnaire", "Mail1",
      ifelse(Date.rec <= Mailing3 & Date.rec > Mailing2&Outcome=="Returned useable questionnaire", "Mail2",
            ifelse(Date.rec > Mailing3&Outcome=="Returned useable questionnaire", "Mail3",NA)))))
xtabs(~reply.strata,dta2009)
# Check that replies actually received later than mailings.
# If not, code as NA

Reply.Time <- with(dta2009,ifelse(new.Date.rec > Mailing1,new.Date.rec - Mailing1,NA))
Reply.Time2 <- with(dta2009,ifelse(new.Date.rec > Mailing2,new.Date.rec - Mailing2,NA))
Reply.Time3 <- with(dta2009,ifelse(new.Date.rec > Mailing3,new.Date.rec - Mailing3,NA))

dta2009$Reply.Time2 <- ifelse(dta2009$reply.strata=="Mail2",Reply.Time2,NA)
dta2009$Reply.Time3 <- ifelse(dta2009$reply.strata=="Mail3",Reply.Time3,NA)
dta2009$Reply.Time1 <- ifelse(dta2009$reply.strata=="Mail1",Reply.Time,NA)
dta2009$Reply.Time <- Reply.Time
rm(Reply.Time)
dta2009$censored <- with(dta2009, ifelse(new.outcome=="Returned",1,
                                         ifelse(new.outcome=="Not returned",0,NA)))
dta2009$LogLOS <- with(dta2009,log(LOS))
dta2009$Choice <- with(dta2009,factor(ifelse(Q10=="Yes","Yes","No")))
dta2009$emergency <- with(dta2009,factor(ifelse(Q1=="Emergency or urgent","yes","no")))
dta2009$overall.tri <- factor(with(dta2009, ifelse(Overall.Sat==1,1,
                                      ifelse(Overall.Sat==2,2,
                                             ifelse(is.na(Overall.Sat), NA, 3)))),labels=c("Excellent","Very good","Other"))
dta2009$Trustcode <- factor(dta2009$Trustcode)

# Remove unnecessary "Missing data" level
dta2009$all_gender <- dta2009$all_gender[, drop=TRUE]

# Dichotomous outcome variables 
# early defined as less than 15 days
###############################
dta2009$early <- factor(with(dta2009,ifelse(Reply.Time <= 14, "Early",ifelse(Reply.Time>14,"Late",NA))))
dta2009$reply.strata2 <- factor(with(dta2009,ifelse(reply.strata=="Mail1","First",
                                     ifelse(reply.strata=="Mail2"|reply.strata=="Mail3","Later",NA))))

dta2009$Late = factor(ifelse(dta2009$Reply.Time > 21,"Late","Early"))
dta2009$Late1 = factor(ifelse(dta2009$Reply.Time1 > 21,"Late","Early"))
dta2009$Late2 = factor(ifelse(dta2009$Reply.Time2 > 21,"Late","Early"))
dta2009$Late3 = factor(ifelse(dta2009$Reply.Time3 > 21,"Late","Early"))

xtabs(~Late,dta2009)
# vars.used <- c("Outcome","censored","age_group","LOS",
#                "LogLOS","Reply.Time1","Reply.Time2","Reply.Time3",
#                "reply.strata","Choice","emergency","Mailing1",
#                "Mailing2","Mailing3","overall.tri","Trustcode","early")
# 
# 
# dta2009.used <- subset(dta2009,select=vars.used)

write.csv(dta2009,"dta2009.csv")
dta2009 <- read.csv("dta2009.csv")
# Tidy up workspace
rm(Date.rec.posix,Mailing1,RepTime,Reply.Time2,Reply.Time3,date1,date2,delta.date)
rm(delta.secs,g,mail1.posix,mail2.posix,max.date,new.Date.rec,new.outcome)
rm(mail3.posix,outcome.levels,overall,overall.num,overall.ord,reply.strata2)


##### Data checks

with(dta2009,table(Outcome,overall,useNA="ifany"))
with(dta2009,table(censored,overall.tri,useNA="ifany"))
# Shows 2202 cases recorded as "Returned useable questionnaire
# but NA on Q74


##### Are any returned but no return date recorded?

with(dta2009,table(reply.strata,overall.tri,useNA="ifany"))

# Yes, some valid responses without reply times.
# 191 +        157  +      127 = 475 in total



#####################
## Descriptive stats
########################
tab.ov <- xtabs(~overall,dta2009)
rbind(tab.ov,tab.ov/sum(tab.ov)*100)

(tmp <- xtabs(~reply.strata,dta2009))
tmp/sum(tmp)

with(dta2009, {
     cat("Time 1",median(Reply.Time1,na.rm=TRUE),"\n")
     cat("Time 2",median(Reply.Time2,na.rm=TRUE),"\n")
     cat("Time 3",median(Reply.Time3,na.rm=TRUE))
})

library(descr)
with(dta2009,CrossTable(Outcome,missing.include=TRUE))
# In complete dataset, 69348 returned questionnaires
# 55152 not returned, reason  unknown
# 8713 returned blank, opted out
# So overall response rate = 69348/(69348+55152+8713) = .52
with(dta2009,CrossTable(reply.strata))

#  43756 returned before first reminder sent
# 43756/(43756+55152+8713) = .406

with(dta2009,CrossTable(reply.strata,Outcome))
with(dta2009,CrossTable(reply.strata,overall.tri, missing.include=F))
with(dta2009,CrossTable(reply.strata2,overall.tri, missing.include=F))
with(dta2009,CrossTable(reply.strata2,age_group, missing.include=F))
with(dta2009,CrossTable(reply.strata2,all_gender, missing.include=F))
with(dta2009,CrossTable(reply.strata2,emergency, missing.include=F))
t.test(LOS~reply.strata2,dta2009,na.rm=TRUE)
with(dta2009,CrossTable(new.outcome,age_group, missing.include=F))

with(dta2009,by(Reply.Time,overall.tri,mean,na.rm=TRUE))


##########
## Plots
#################################


RepTime <- with(dta2009,ifelse(reply.strata=="Mail1",Reply.Time1,
                               ifelse(reply.strata=="Mail2",Reply.Time2,
                                      ifelse(reply.strata=="Mail3",Reply.Time3,NA))))
plotdata <- data.frame(Reply.Time=RepTime,Mailing=dta2009$reply.strata)
levels(plotdata$Mailing) <- c("First","Second","Third")
g <- ggplot(data=plotdata)
g + geom_histogram(aes(x=Reply.Time,y=..density..,fill=Mailing),position="dodge") + xlab("Time to reply (days)") + 
  ylab("Density") + xlim(c(0,40))

# Could use density plot?
# g + geom_density(aes(x=Reply.Time,colour=Mailing))

# Inverse survival (KM) plot 

surv2009 <- Surv(as.double(dta2009$Reply.Time), dta2009$censored)
surv1 <- Surv(as.double(dta2009$Reply.Time1),dta2009$censored)
surv2 <- Surv(as.double(dta2009$Reply.Time2),dta2009$censored)
surv3 <- Surv(as.double(dta2009$Reply.Time3),dta2009$censored)

km.fit <- survfit(surv2009~overall.tri,dta2009)
summary(km.fit)
st1 <- km.fit$surv[1:111]
st2 <- km.fit$surv[112:220]
st3 <- km.fit$surv[221:327]
y1 <- log((1-st1)/st1)
y2 <- log((1-st2)/st2)
y3 <- log((1-st3)/st3)

x1 <- log(km.fit$time[1:111])
x2 <- log(km.fit$time[112:220])
x3 <- log(km.fit$time[221:327])

plot(x1,y1)

km.fit1 <- survfit(surv1~1,dta2009)
km.fit2 <- survfit(surv2~1,dta2009)
km.fit3 <- survfit(surv3~1,dta2009)

km.dta <- data.frame(Time=km.fit$time, ResponseRate=1-km.fit$surv)
km.dta1 <- data.frame(Time=km.fit1$time, ResponseRate1=1-km.fit1$surv)
km.dta2 <- data.frame(Time=km.fit2$time, ResponseRate2=1-km.fit2$surv)
km.dta3 <- data.frame(Time=km.fit3$time, ResponseRate3=1-km.fit3$surv)
km.dta.a1 <- merge(km.dta,km.dta1,by="Time",all=TRUE)
km.dta.a12 <- merge(km.dta.a1,km.dta2,by="Time",all=TRUE)
km.dta.a123 <- merge(km.dta.a12,km.dta3,by="Time",all=TRUE)

g <- ggplot(km.dta.a123,aes(x=Time,y=ResponseRate)) + geom_line() + 
  geom_vline(xintercept=c(21,42),colour="salmon") + theme_bw() +
  labs(x="Time since first mailing (days)", y="Response rate") + xlim(0,80)

g + geom_line(aes(y=ResponseRate1),colour="red") + 
  geom_line(aes(y=ResponseRate2),colour="blue") +
  geom_line(aes(y=ResponseRate3),colour="green") 
  
## Inverse survival plot by Trust
km.fit.tc <- by(dta2009,dta2009$Trustcode,function(x) survfit(Surv(Reply.Time,censored)~1,data=x))

pdf("ResponseRates.pdf")
for (i in 1:162){
  km.dta <- data.frame(Time=km.fit.tc[[i]]$time, ResponseRate=1-km.fit.tc[[i]]$surv)
  print(ggplot(km.dta,aes(x=Time,y=ResponseRate)) + geom_line() + 
    geom_vline(xintercept=c(21,42),colour="salmon") + theme_bw() +
    labs(x="Time since first mailing (days)", y="Response rate")  + opts(title=i))
}
dev.off()
tcs <- levels(dta2009$Trustcode)
tcs[41]
subset(dta2009,subset=Trustcode==tcs[41],select=c("Reply.Time","Mailing1","Date.rec"))
# Hazard rate plot, but not useful so didn't use
tmp <- data.frame(Reply.Time=dta2009$Reply.Time,censored=dta2009$censored)
ix <- complete.cases(tmp)
tmp <- tmp[ix,]
mufit <- muhaz(tmp$Reply.Time,tmp$censored,max.time=70,bw.method="l")
plot(mufit)
###################################################################
#### Regressions
################################################

# Is this worth doing? All censored cases will have missing values on 
# overall.tri

surv2009 <- Surv(as.double(dta2009$Reply.Time), dta2009$censored)
plot(surv2009)
# All replies
mh1 <- with(dta2009,muhaz(Reply.Time,censored,bw.method="global"))

l1 <- survreg(surv2009 ~ overall.tri + cluster(Trustcode), data=dta2009,dist="loglogistic")
plot(l1)
summary(l1)
predict(l1,data.frame(overall.tri=c("Excellent","Very good","Other"),Late="Early",Trustcode="5QT"),se.fit=FALSE,type="response")

w1.c <- coxph(surv2009 ~ overall.tri + cluster(Trustcode), data=dta2009)
summary(w1.c)
predict(w1.c,data.frame(overall.tri=c("Excellent","Very good","Other")),se.fit=FALSE,type="risk")
w1.c.fit <- survfit(w1.c,data.frame(overall.tri=c("Excellent","Very good","Other")),se.fit=FALSE)
plot(w1.c.fi)
## Could use linear regression?
m1 <- lm(log(Reply.Time) ~ overall.tri , data=dta2009)
display(m1)
exp(fixef(m1))
exp(predict(m1,newdata=data.frame(overall.tri=c("Excellent","Very good","Other"))))
# First mailing
l.first <- survreg(Surv(as.double(dta2009$Reply.Time1),censored) ~ overall.tri + cluster(Trustcode), data=dta2009, dist="loglogistic")
summary(l.first)

# Second mailing
l.second <- survreg(Surv(as.double(dta2009$Reply.Time2),censored) ~ overall.tri  + cluster(Trustcode), data=dta2009, dist="loglogistic")
summary(l.second)

# Third mailing
l.third <- survreg(Surv(as.double(dta2009$Reply.Time3),censored) ~ overall.tri  + cluster(Trustcode), data=dta2009, dist="loglogistic")
summary(l.third)

####
## Predicted response times in table 4
####
predict(l1,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))
predict(l.first,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))
predict(l.second,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))
predict(l.third,newdata=data.frame(overall.tri=levels(dta2009$overall.tri),Trustcode=levels(dta2009$Trustcode)[1]))

### Full models (table 5)
####################################

# All replies
l.all <- update(l1, . ~ . + emergency + age_group + LogLOS + all_gender)
summary(l.all)
t<-seq(0.1,60,by=.1)
lambda <- exp(-l.all$icoef[1])
rho <- l.all$scale
hl <- lambda*rho*(lambda*t)^rho / (1 + (lambda*t)^(rho+1) )
plot(t,hl,type="l")

##################################
### Function to plot baseline hazard function from log-logistic regression
##################################################
hazplot <- function(m,plot=TRUE,xlim=NULL,length=100){
  ttmp <- exp(m$y[,1]) # Time
  
  if(is.null(xlim)){
    tmin <- min(ttmp)
    tmax <- max(ttmp)
  }
  else {
    tmin <- xlim[1]
    tmax <- xlim[2]
  }
  t <- seq(tmin,tmax,length.out=length)
  lambda <- exp(-m$icoef[1])
  rho <- m$scale
  h <- lambda*rho*(lambda*t)^rho / (1 + (lambda*t)^(rho+1) )
  invisible(h)
  if (plot) plot(t,h,type="l",xlab="Time",ylab="Hazard function")
}

hazplot(l.all,xlim=c(0,60))
# First mailing
l.first.all <- update(l.first, . ~ . + emergency + age_group + LogLOS + all_gender)
summary(l.first.all)
hazplot(l.first.all,xlim=c(0,60))
# Second mailing
l.second.all <- update(l.second, . ~ . + emergency + age_group + LogLOS + all_gender)
summary(l.second.all)
hazplot(l.second.all,xlim=c(0,60))

# Third mailing
l.third.all <- update(l.third, . ~ . + emergency + age_group + LogLOS + all_gender)
summary(l.third.all)
  hazplot(l.third.all,xlim=c(0,60))

### Exclude Trusts with odd data
lvs <- levels(dta2009$Trustcode)
ix <- with(dta2009,Trustcode==lvs[41]|Trustcode==lvs[65])

w.all.ss <- update(w1, . ~ . + emergency + age_group + LogLOS + all_gender, subset=!ix)
summary(w.all.ss)
## Results very similar, so use full dataset
nd <- data.frame(overall.tri=levels(dta2009$overall.tri),emergency="yes",age_group="36-50",
                 LogLOS=1.16,all_gender="Male",Trustcode="5QT")
predict(l.all,nd)

predict(l.first.all,nd)

predict(l.second.all,nd)

predict(l.third.all,nd)

##############
# Regressions using dichotomous outcome (table 6)
############################################
early.reg <- lmer(early ~ overall.tri + (1|Trustcode), family=binomial, data=dta2009)
display(early.reg)
exp(fixef(early.reg))

first.reg <- lmer(reply.strata2 ~ overall.tri + (1|Trustcode), family=binomial, data=dta2009)
display(first.reg, digits=3)
exp(fixef(first.reg))


first.reg.full <- update(first.reg, .~. + emergency + age_group + LogLOS + all_gender)
display(first.reg.full, digits=3)
exp(fixef(first.reg.full))
exp(.035)

inv.logit <- function (x, min = 0, max = 1)
{
  p <- exp(x)/(1 + exp(x))
  p <- ifelse(is.na(p) & !is.na(x), 1, p)
  p * (max - min) + min
}

##### Predicted probs (table 7)
b <- fixef(first.reg)
inv.logit(b[1])
inv.logit(b[1] + b[2])
inv.logit(b[1]+b[3])


###############
# Regressions using time as explanatory variable
###################
dta2009$overall.tri <- ordered(dta2009$overall.tri, levels=c('Other','Very good','Excellent'))

p1 <- polr(overall.tri ~ Reply.Time + emergency + age_group + LogLOS + all_gender, data=dta2009,Hess=TRUE)
e1 <- effect('Reply.Time',p1)
plot(e1)

p2 <- polr(overall.tri ~ Reply.Time1 + emergency + age_group + LogLOS + all_gender, data=dta2009)
p3 <- polr(overall.tri ~ Reply.Time2 + emergency + age_group + LogLOS + all_gender, data=dta2009)
p4 <- polr(overall.tri ~ Reply.Time3 + emergency + age_group + LogLOS + all_gender, data=dta2009)
e4 <- effect('Reply.Time3',p4)
plot(e4)

library(ordinal)
mp1 <- clmm(overall.tri ~ Reply.Time + emergency + age_group + LogLOS + all_gender + (1|Trustcode), data=dta2009)

############### current end

p1 <- polr(overall.tri ~ reply.strata + emergency + age_group + LogLOS + all_gender , data=dta2009, Hess=TRUE)
e1 <- effect('reply.strata',p1, xlevels=list(emergency='yes',age_group='36-50',LogLOS=1.16,all_gender='Male'))
               
newd <- data.frame(reply.strata=c('Mail1','Mail2','Mail3'),emergency='yes',age_group='36-50',LogLOS=1.16,all_gender='Male')
pred.p1 <- predict(p1,newd,'p')
predict(p1,newd)


#####
### cleanliness of ward
#####
clean <- ordered(dta2009$Q22, levels=c('Very clean','Fairly clean','Not very clean','Not at all clean'))
dta2009$clean.tri <- ordered(with(dta2009, ifelse(clean=='Very clean',3,
                                                   ifelse(clean=='Fairly clean',2,
                                                          ifelse(is.na(clean), NA, 1)))),labels=rev(c("Very clean","Fairly clean","Other")))

doc.conf <- ordered(dta2009$Q32, levels=rev(c('Yes, always','Yes, sometimes','No')))
nurse.conf <- ordered(dta2009$Q36, levels=rev(c('Yes, always','Yes, sometimes','No')))
call.but <- ordered(dta2009$Q49, levels=c('I never got help when I used the call button ','More than 5 minutes','3-5 minutes','1-2 minutes','0 minutes/ right away'))
table(call.but)

dta2009 <- data.frame(dta2009,doc.conf,nurse.conf,call.but)

summary(xtabs(~clean.tri + early, dta2009))

p1.c <- polr(clean.tri ~ Reply.Time + emergency + age_group + LogLOS + all_gender, data=dta2009)
predict(p1.c,newd,'p')

p1.cd <- polr(clean.tri ~ reply.strata + emergency + age_group + LogLOS + all_gender, data=dta2009, Hess=TRUE)
summary(p1.cd)
e1 <- effect('reply.strata',p1)
predict(p1.cd,newd,'p')

#e1 <- effect('Reply.Time',p1)
#plot(e1)

p1.doc <- polr(doc.conf ~ reply.strata + emergency + age_group + LogLOS + all_gender, data=dta2009, Hess=TRUE)
summary(p1.doc)
predict(p1.doc,newd,'p')

p1.nurse <- polr(nurse.conf ~ reply.strata + emergency + age_group + LogLOS + all_gender, data=dta2009, Hess=TRUE)
summary(p1.nurse)
predict(p1.nurse,newd,'p')

p1.call <- polr(call.but ~ reply.strata + emergency + age_group + LogLOS + all_gender, data=dta2009, Hess=TRUE)

predict(p1.call,newd,'p')

library(stargazer)
stargazer(p1,p1.cd,p1.doc,p1.nurse,p1.call, covariate.labels=c('Second mailing','Third mailing','Emergency admission','Age 36--50','Age 51--65','Age 66+','log(length of stay)','Male'), dep.var.labels=c('Overall','Cleanliness','Doctors','Nurses','Call button'),model.numbers=FALSE,omit=c('Trustcode*'))