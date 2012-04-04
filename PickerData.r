library(foreign)
library(arm)
setwd('C:/Users/dbarron/Documents/Picker Data/')

dta2006 <- read.dta('Inpatient2006.dta')
dta2007 <- read.dta('Inpatient2007.dta')
dta2008 <- read.dta('Inpatient2008.dta')
dta2009 <- read.dta('Inpatient2009.dta')

summary(dta2006)
mis.dat <- lapply(dta2006, function(x) data.frame(NAs=sum(is.na(x)),N=length(x),
  prop.na=round(sum(is.na(x))/length(x),2)))

library(plyr)


misprop <- function(x) data.frame(NAs=sum(is.na(x)),N=length(x),prop.na=round(sum(is.na(x))/length(x),2))
mis.dat2 <- lapply(dta2006,function(x) tapply(x, dta2006$trustcode, function(y) round(sum(is.na(x))/length(x),2)))


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

# Do the same for the date of first mailing
mail1.posix <- with(dta2006, Mailing_One - delta.secs)
class(mail1.posix) <- "POSIXct"
Mailing1 <- as.Date(mail1.posix)

Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))

xtabs(~Q66, dta2006)
overall <- dta2006$Q66[, drop=TRUE]
overall.num <- as.numeric(overall)
table(overall, useNA="ifany")
dta2006 <- data.frame(dta2006,Reply.Time, Overall.Sat=overall.num)
rm(Reply.Time)
lm2006 <- lm(Reply.Time ~ Overall.Sat, data=dta2006, subset = Reply.Time > 0 )

lm2006.a <- lm(Reply.Time ~ factor(Overall.Sat) + age_group + LOS + gender, data=dta2006, subset=Reply.Time > 0)


library(arm)
ml1 <- lmer(Reply.Time ~ Overall.Sat + age_group + LOS + gender  + (1|trustcode), data=dta2006, subset=Reply.Time > 0)
display(ml1)
###############
## 2007
################

Date.rec.posix <- with(dta2007, DATEREC - delta.secs)
class(Date.rec.posix) <- "POSIXct"
# Remove the "time of day" part of the date
Date.rec <- as.Date(Date.rec.posix)

# Do the same for the date of first mailing
mail1.posix <- with(dta2007, Mailing_One - delta.secs)
class(mail1.posix) <- "POSIXct"
Mailing1 <- as.Date(mail1.posix)


Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))

overall <- dta2007$Q68[, drop=TRUE]
overall.ord <- as.ordered(overall)
overall.num <- as.numeric(overall)
table(overall, useNA="ifany")
dta2007 <- data.frame(dta2007,Reply.Time, Overall.Sat=overall.num)

lm2007 <- lm(Overall.Sat ~ Reply.Time, data=dta2007, subset = Reply.Time > 0 )
lm2007.a <- lm(Overall.Sat ~ Reply.Time + age_group + LOS + gender, data=dta2007, subset=Reply.Time > 0)

ol2007 <- polr(overall.ord ~ Reply.Time, subset=Reply.Time>0, Hess=TRUE)

#############
## 2008
################

Date.rec.posix <- with(dta2008, DATEREC - delta.secs)
class(Date.rec.posix) <- "POSIXct"
# Remove the "time of day" part of the date
Date.rec <- as.Date(Date.rec.posix)

# Do the same for the date of first mailing
mail1.posix <- with(dta2008, Mailing_One - delta.secs)
class(mail1.posix) <- "POSIXct"
Mailing1 <- as.Date(mail1.posix)


Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))

overall <- dta2008$Q71[, drop=TRUE]
overall.ord <- as.ordered(overall)
overall.num <- as.numeric(overall)
table(overall, useNA="ifany")
dta2008 <- data.frame(dta2008,Reply.Time, Overall.Sat=overall.num)

lm2008 <- lm(Overall.Sat ~ Reply.Time, data = dta2008, subset = Reply.Time > 0 )
lm2008.a <- lm(Overall.Sat ~ Reply.Time + age_group + LOS + gender, data=dta2008, subset=Reply.Time > 0)


########################
###2009
########################

Date.rec.posix <- with(dta2009, DATEREC - delta.secs)
class(Date.rec.posix) <- "POSIXct"
# Remove the "time of day" part of the date
Date.rec <- as.Date(Date.rec.posix)

# Do the same for the date of first mailing
mail1.posix <- with(dta2009, Mailing1 - delta.secs)
class(mail1.posix) <- "POSIXct"
Mailing1 <- as.Date(mail1.posix)


Reply.Time <- Date.rec - Mailing1
hist(as.double(Reply.Time))

overall <- dta2009$Q74[, drop=TRUE]
overall.ord <- as.ordered(overall)
overall.num <- as.numeric(overall)
table(overall, useNA="ifany")
dta2009 <- data.frame(dta2009,Reply.Time, Overall.Sat=overall.num)

dta2009$emergency <- with(dta2009,factor(ifelse(Q1=="Emergency or urgent","yes","no")))
dta2009$Choice <- with(dta2009,factor(ifelse(Q6=="No, but I would have liked a choice","No","Other")))
dta2009$Overall.Dic <- with(dta2009,factor(ifelse(Overall.Sat<4,"Good","Poor.Fair")))
dta2009$LogLOS <- with(dta2009,log(LOS))
dta2009$Reply.Time <- with(dta2009,as.numeric(Reply.Time))
dta2009$Reply.Time <- with(dta2009,ifelse(Reply.Time<0,NA,Reply.Time))
dta2009$Overall.Score <- with(dta2009,ifelse(Overall.Sat==1, 100,
                                        ifelse(Overall.Sat==2, 75,
                                         ifelse(Overall.Sat==3, 50,
                                          ifelse(Overall.Sat==4, 25, 0)))))
dta2009$Dignity <- with(dta2009, factor(ifelse(Q72=="Yes, always","Always","Sometimes or no")))

dta2009$censored <- with(dta2009, ifelse(Outcome=="Returned useable questionnaire",1,0))
#surv2009 <- Surv(dta2009$Reply.Time, dta2009$censored)

# Response rates
rr <- with(dta2009,tapply(Overall.Sat,Trustcode,function(x) sum(!is.na(x))/length(x)))
rrd <- data.frame(Trustcode=names(rr),ResponseRates=as.vector(rr))

lm2009 <- lm(Overall.Sat ~ Reply.Time, data=dta2009, weights=ResponseRates, subset = Reply.Time > 0 )

lm2009.a <- lm(Reply.Time ~ factor(Overall.Score) + emergency + Choice + age_group + LogLOS + all_gender, data=dta2009)
display(lm2009.a)
pred.dta <- data.frame(Overall.Score=c(0,25,50,75,100),emergency='yes',Choice='Other',age_group='36-50',LogLOS=1,all_gender='Female')
predict(lm2009.a,pred.dta)

ml2009.a <- lmer(Reply.Time ~ factor(Overall.Sat) + age_group + log(LOS) + all_gender + (1|Trustcode), data=dta2009)
display(ml2009.a)

lg2009.a <- glm(Dignity ~ Reply.Time + emergency + Choice + age_group + log(LOS) + all_gender, data=dta2009, subset=Reply.Time > 0, family=binomial)

#cox2009 <- coxph(surv2009 ~ Overall.Sat + emergency + Choice + age_group + log(LOS) + all_gender, data=dta2009)

dta2009m <- merge(dta2009,rrd,all.x=TRUE)

ol2009a <- polr(as.ordered(Overall.Sat) ~ Reply.Time + age_group + LOS + all_gender, data=dta2009, subset=Reply.Time > 0)

lm2009.RT <- lm(log(Reply.Time) ~ Overall.Sat + emergency + Choice + age_group + LogLOS + all_gender, data=dta2009)
ml2009.RT <- lmer(Reply.Time ~ Overall.Sat + emergency + Choice + age_group + LogLOS + all_gender + (1|Trustcode), data=dta2009)

lm2009.RTa <- lm(Reply.Time ~ Overall.Dic + emergency + Choice + age_group + LogLOS + all_gender, data=dta2009)

library(ggplot2)

g <- ggplot(data=dta2009)
g + geom_point(aes(x=LogLOS,y=Reply.Time), position="jitter")

g + geom_boxplot(aes(y=Reply.Time,x=Overall.Sat))

g + geom_histogram(aes(x=Reply.Time,y=..density..)) + geom_density(aes(x=Reply.Time))

dta.sub <- subset(dta2006,select=c("Q66","Overall.Sat","Reply.Time","age_group","LOS","gender"))
ix <- complete.cases(dta.sub)
rp1 <- rpart(Q66 ~ Reply.Time + age_group + LOS + gender, data = dta.sub, subset=ix)
