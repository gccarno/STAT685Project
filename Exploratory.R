#Greg Carnovale
#greg.carnovale@tamu.edu
#STAT 685
#Exploratory.R

library(plyr)
library(dplyr)


# Read in Data from csv files
path <- "E:/STAT685/Data/"
dfy19ea1 <- read.csv(file=paste(path, 'dfy19ea1.dat',sep=''))
dfy18ea1 <- read.csv(file=paste(path, 'dfy18ea1.dat',sep=''))
dfy17ea1 <- read.csv(file=paste(path, 'dfy17ea1.dat',sep=''))
dfy16ea1 <- read.csv(file=paste(path, 'dfy16ea1.dat',sep=''))
dfy15ea1 <- read.csv(file=paste(path, 'dfy15ea1_dat.csv',sep=''))
dfy14ea1 <- read.csv(file=paste(path, 'dfy14ea1.dat',sep=''))
dfy13ea1 <- read.csv(file=paste(path, 'dfy13ea1.csv',sep=''))
dfy12ea1 <- read.csv(file=paste(path, 'dfy12ea1.csv',sep=''))

#read demographic data
dem17 <- read.csv(file=paste(path, 'district2017.dat',sep=''),na.strings=c("",'.',"NA"))
dem16 <- read.csv(file=paste(path, 'district2016.dat',sep=''),na.strings=c("",'.',"NA"))
dem15 <- read.csv(file=paste(path, 'district2015.dat',sep=''),na.strings=c("",'.',"NA"))
dem14 <- read.csv(file=paste(path, 'district2014.dat',sep=''),na.strings=c("",'.',"NA"))
dem13 <- read.csv(file=paste(path, 'district2013.dat',sep=''),na.strings=c("",'.',"NA"))

#add demographics
#do not have demographics for 19 and 18
dfy17ea1 <- merge(dfy17ea1, dem17, by='DISTRICT')
dfy16ea1 <- merge(dfy16ea1, dem16, by='DISTRICT')
dfy15ea1 <- merge(dfy15ea1, dem15, by='DISTRICT')
dfy14ea1 <- merge(dfy14ea1, dem14, by='DISTRICT')
dfy13ea1 <- merge(dfy13ea1, dem13, by='DISTRICT')

#convert column to factor for easier binding

#upcase some names in dfy19ea1
names(dfy19ea1)[1:5] <- toupper(names(dfy19ea1[,1:5]))

#concatenate by year and district?
all_years <- bind_rows(dfy19ea1,dfy18ea1,dfy17ea1,dfy16ea1,dfy15ea1,dfy14ea1,dfy13ea1,dfy12ea1)

#rm some because memory issues?

#write all years to csv
write.csv(all_years, file=paste(path, 'all_years_and_dem.csv',sep=''))

#easier looping?
sets <- list('dfy19ea1'= dfy19ea1, 
             'dfy18ea1'=dfy18ea1, 
             'dfy17ea1'=dfy17ea1, 
             'dfy16ea1'=dfy15ea1, 
             'dfy15ea1'=dfy14ea1,
             'dfy14ea1'=dfy13ea1, 
             'dfy13ea1'=dfy12ea1)
rm_50_na <- function(dat){
  return(dat[, -which(colMeans(is.na(dat)) > 0.5)])
}
#remove columns with greater than 50% missing values
sets <- lapply(sets, rm_50_na)

#by gender
cols <- grep("a1_sex.*_rs", names(all_years), value=T)
#boxplot(dfy19ea1[c('a1_all_rs','a1_sexm_rs','a1_sexf_rs','a1_sexf_rs')] )
boxplot(a1_all_rs ~ YEAR, data=all_years)
boxplot(a1_all_rsrs + a1_sexm_ ~ YEAR, data=all_years)
#by ethnicity
cols <- grep("a1_eth.*_rs", names(dfy19ea1), value=T)
boxplot(dfy19ea1[, cols])
#by race

#races over time
unique(all_years$year)
