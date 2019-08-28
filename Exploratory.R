#Greg Carnovale
#greg.carnovale@tamu.edu
#STAT 685
#Exploratory.R

library(data.table)

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

#concatenate by year and district?

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
boxplot(dfy19ea1[c('a1_all_rs','a1_sexm_rs','a1_sexf_rs','a1_sexf_rs')])
#by ethnicity
cols <- grep("a1_eth.*_rs", names(dfy19ea1), value=T)
boxplot(dfy19ea1[, cols])
#by race


