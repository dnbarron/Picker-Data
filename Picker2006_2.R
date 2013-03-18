library(foreign)
library(plyr)
library(survival)
library(arm)
library(ggplot2)

dta2006 <- read.dta('Inpatient2006.dta')

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
dta2006$Date.rec <- Date.rec
rm(Date.rec)

mail1.posix <- with(dta2006, Mailing_One - delta.secs)
class(mail1.posix) <- "POSIXct"
dta2006$Mailing1 <- as.Date(mail1.posix)


# Do the same for the date of 2nd mailing
mail2.posix <- with(dta2006, Mailing_Two - delta.secs)
class(mail2.posix) <- "POSIXct"
dta2006$Mailing2 <- as.Date(mail2.posix)
#dta$Mailing2 <- as.Date(as.character(dta$Mailing2))

# Do the same for the date of 3rd mailing
mail3.posix <- with(dta2006, Mailing_Three - delta.secs)
class(mail3.posix) <- "POSIXct"
dta2006$Mailing3 <- as.Date(mail3.posix)

overall06 <- dta2006$Q66[, drop=TRUE]
overall.ord <- as.ordered(overall06)
overall.num <- as.numeric(overall06)
