#Greg Carnovale
#greg.carnovale@tamu.edu
#STAT 685
#Exploratory.R

library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)

# Read in Data from csv files
path <- "E:/STAT685/Data/"
dfy19ea1 <- read.csv(file=paste(path, 'dfy19ea1.dat',sep=''),na.strings=c("",'.',"NA"))
dfy18ea1 <- read.csv(file=paste(path, 'dfy18ea1.dat',sep=''),na.strings=c("",'.',"NA"))
dfy17ea1 <- read.csv(file=paste(path, 'dfy17ea1.dat',sep=''),na.strings=c("",'.',"NA"))
dfy16ea1 <- read.csv(file=paste(path, 'dfy16ea1.dat',sep=''),na.strings=c("",'.',"NA"))
dfy15ea1 <- read.csv(file=paste(path, 'dfy15ea1_dat.csv',sep=''),na.strings=c("",'.',"NA"))
dfy14ea1 <- read.csv(file=paste(path, 'dfy14ea1.dat',sep=''),na.strings=c("",'.',"NA"))
dfy13ea1 <- read.csv(file=paste(path, 'dfy13ea1.csv',sep=''),na.strings=c("",'.',"NA"))
dfy12ea1 <- read.csv(file=paste(path, 'dfy12ea1.csv',sep=''),na.strings=c("",'.',"NA"))

#read demographic data
dem17 <- read.csv(file=paste(path, 'district2017.dat',sep=''),na.strings=c("",'.',"NA"))
dem16 <- read.csv(file=paste(path, 'district2016.dat',sep=''),na.strings=c("",'.',"NA"))
dem15 <- read.csv(file=paste(path, 'district2015.dat',sep=''),na.strings=c("",'.',"NA"))
dem14 <- read.csv(file=paste(path, 'district2014.dat',sep=''),na.strings=c("",'.',"NA"))
dem13 <- read.csv(file=paste(path, 'district2013.dat',sep=''),na.strings=c("",'.',"NA"))
dem12 <- read.csv(file=paste(path, 'district2012.dat',sep=''),na.strings=c("",'.',"NA"))

#read grade 7 data - only 2 years prior
#dfy19e7 <- read.csv(file=paste(path, 'dfy19e7.dat',sep=''),na.strings=c("",'.',"NA"))
#dfy18e7 <- read.csv(file=paste(path, 'dfy18e7.dat',sep=''),na.strings=c("",'.',"NA"))
dfy17e7 <- read.csv(file=paste(path, 'dfy17e7.dat',sep=''),na.strings=c("",'.',"NA"))
dfy16e7 <- read.csv(file=paste(path, 'dfy16e7.dat',sep=''),na.strings=c("",'.',"NA"))
dfy15e7 <- read.csv(file=paste(path, 'dfy15e7_dat.csv',sep=''),na.strings=c("",'.',"NA"))
dfy14e7 <- read.csv(file=paste(path, 'dfy14e7.dat',sep=''),na.strings=c("",'.',"NA"))
dfy13e7 <- read.csv(file=paste(path, 'dfy13e7.csv',sep=''),na.strings=c("",'.',"NA"))
dfy12e7 <- read.csv(file=paste(path, 'dfy12e7.csv',sep=''),na.strings=c("",'.',"NA"))

#add demographics
#do not have demographics for 19 and 18
dfy17ea1 <- merge(dfy17ea1, dem17, by='DISTRICT')
dfy16ea1 <- merge(dfy16ea1, dem16, by='DISTRICT')
dfy15ea1 <- merge(dfy15ea1, dem15, by='DISTRICT')
dfy14ea1 <- merge(dfy14ea1, dem14, by='DISTRICT')
dfy13ea1 <- merge(dfy13ea1, dem13, by='DISTRICT')
dfy12ea1 <- merge(dfy12ea1, dem12, by='DISTRICT')

#convert column to factor for easier binding

#upcase some names in dfy19ea1
names(dfy19ea1)[1:5] <- toupper(names(dfy19ea1[,1:5]))

#add prior year scores
dfy19ea1 <- merge(dfy19ea1,dfy17e7[,c('DISTRICT','m_all_rs','r_all_rs','w_all_rs')],by='DISTRICT')
dfy18ea1 <- merge(dfy18ea1,dfy16e7[,c('DISTRICT','m_all_rs','r_all_rs','w_all_rs')],by='DISTRICT')
dfy17ea1 <- merge(dfy17ea1,dfy15e7[,c('DISTRICT','m_all_rs','r_all_rs','w_all_rs')],by='DISTRICT')
dfy16ea1 <- merge(dfy16ea1,dfy14e7[,c('DISTRICT','m_all_rs','r_all_rs','w_all_rs')],by='DISTRICT')
dfy15ea1 <- merge(dfy15ea1,dfy13e7[,c('DISTRICT','m_all_rs','r_all_rs','w_all_rs')],by='DISTRICT')
dfy14ea1 <- merge(dfy14ea1,dfy12e7[,c('DISTRICT','m_all_rs','r_all_rs','w_all_rs')],by='DISTRICT')

#concatenate by year and district?
all_years <- bind_rows(dfy19ea1,dfy18ea1,dfy17ea1,dfy16ea1,dfy15ea1,dfy14ea1,dfy13ea1,dfy12ea1)

# number tested
all_years$PASSED <- apply(select(all_years, a1_all_meetsgl_nm, a1_all_approgl_nm, a1_all_mastrgl_nm), 1, sum)

hist(all_years[all_years$YEAR==19,'PASSED']-all_years[all_years$YEAR==19,'a1_all_d'])
sum(na.omit(all_years[all_years$YEAR==19,'PASSED'] < all_years[all_years$YEAR==19,'a1_all_d']))/length(all_years[all_years$YEAR==19,'a1_all_d'])

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
boxplot(a1_all_rsrs + a1_sexm_rs ~ YEAR, data=all_years)
#by ethnicity
cols <- grep("a1_eth.*_rs", names(dfy19ea1), value=T)
boxplot(dfy19ea1[, cols])
#by race

#violin plots
p <- ggplot(all_years, aes(x=as.factor(YEAR), y=a1_all_rs)) + 
  geom_violin()
p

#races over time
unique(all_years$year)

## plot prior years vs test results, only 14 and later are available
## all scores, 7th grade math, writing, reading
## big correlation between 7th grade math, writing, reading
plot(all_years[all_years$YEAR >= 14,c('a1_all_rs','m_all_rs','r_all_rs','w_all_rs')])

## plot vs select demographic data, only 17 and before are available
## taxable value per pupil,number of students per teacher,%economically disadvantaged
plot(all_years[all_years$YEAR <= 17,c('a1_all_rs','DPFVTOTK','DPSTKIDR','DPETECOP')])
#more demos
# total pop, number tested
plot(all_years[all_years$YEAR <= 17,c('a1_all_rs','DPETALLC','a1_all_d')])
#races
#simliar result to test score boxplots
plot(all_years[all_years$YEAR <= 17,c('a1_all_rs','DPETBLAP','DPETHISP','DPETWHIP'
                                      ,'DPETINDP','DPETASIP','DPETPCIP','DPETTWOP')])
#language, econ status, iq
#econ down # English learning no effect,outliers # special ed like e^-x
#bilingual maybe a little up, outliers
#vocational - downard trend, outliers
#gifted - upward trend
plot(all_years[all_years$YEAR <= 17,c('a1_all_rs','DPETECOP','DPETLEPP','DPETSPEP'
                                      ,'DPETBILP','DPETVOCP','DPETGIFP')])

# attendance, dropout, 4yr grad, 5 yr, 6 yr, grad cnt, 
# annual RHSP/DAP,FHSP-E etc.
#attendance outliers, but upward trend
# dropout outliers, but downard
# grad rate upward, but outliers
# grad cnt and RHSP no effect, decreasing variance
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DA0AT16R',
                  'DA0912DR16R',
                  'DAGC4X16R',
                  'DAGC5X15R',
                  'DAGC6X14R',
                  'DA0GR16N',
                  'DA0GS16N'
               )])
# STAAR VARIABLES
# Upward correlation for scores, but also correlated to each other
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DA00A001S17R',
                  'DA00AR01S17R',
                  'DA00AW01S17R',
                  'DA00AM01S17R',
                  'DA00AC01S17R',
                  'DA00AS01S17R',
                  'DB00A001S17R',
                  'DH00A001S17R',
                  'DW00A001S17R',
                  'DI00A001S17R',
                  'D300A001S17R',
                  'D400A001S17R',
                  'D200A001S17R',
                  'DE00A001S17R'
               )])

# college variables
# college at/above good
# SAT/ACT good corr., but also w/ each other
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DA0CT16R',
                  'DA0CC16R',
                  'DA0CSA16R',
                  'DA0CAA16R'
               )])
#teachers total
# no corr
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DPSATOFC',
                  'DPSTTOFC'
               )])

# staff
# no correlation
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DPSCTOFP',
                  'DPSSTOFP',
                  'DPSUTOFP',
                  'DPSTTOFP',
                  'DPSETOFP',
                  'DPSXTOFP',
                  'DPSAMIFP'
               )])

#salary
# upward trend
# last is teachers
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DPSCTOSA',
                  'DPSSTOSA',
                  'DPSUTOSA',
                  'DPSTTOSA'
               )])

#teacher info
#significant factors
# DPST05FP
# DPSTADFP
# DPSTURNR - teacher turnover rate
# DPSBLFP
# DPSTHIFP
# DPSTWHFP
# DPSTREFP
# DPSTSPFP
plot(all_years[all_years$YEAR <= 17
               ,c('a1_all_rs','DPSAKIDR',
                  'DPSTKIDR',
                  'DPST05FP',
                  'DPSTEXPA',
                  'DPSTADFP',
                  'DPSTURNR',
                  'DPSTBLFP',
                  'DPSTHIFP',
                  'DPSTWHFP',
                  'DPSTO2FP',
                  'DPSTREFP',
                  'DPSTSPFP',
                  'DPSTCOFP',
                  'DPSTBIFP',
                  'DPSTVOFP',
                  'DPSTGOFP'
               )])

#total
# mostly missing values
# no trends
foo <- all_years[all_years$YEAR <= 17
                 ,c('a1_all_rs','DPFVTOTK',
                    'DPFTADPR',
                    'DPFRAALLT',
                    'DPFRAALLK',
                    'DPFRASTAP',
                    'DZRVLOCP',
                    'DPFRAFEDP',
                    'DPFUNAB1T',
                    'DPFUNA4T',
                    'DPFEAALLT',
                    'DPFEAOPFT',
                    'DPFEAOPFK',
                    'DPFEAINST',
                    'DPFEAINSK'
                 )]
foo2 <- melt(foo, 'a1_all_rs')
p1 <- ggplot(foo2, aes(value, a1_all_rs)) +  geom_point() + facet_grid(.~variable)
plot(p1)

#revenue
# negative correlation with federal and state
# positive with local
foo <- all_years[all_years$YEAR <= 17
                 ,c('a1_all_rs','DPFRASTAP',
                    'DZRVLOCP',
                    'DPFRAFEDP'
                 )]
foo2 <- melt(foo, 'a1_all_rs')
p1 <- ggplot(foo2, aes(value, a1_all_rs)) +  geom_point() + facet_grid(.~variable)
plot(p1)


#expenditures pct 
# some missing values removed
# DPFPAREGP - regular 
# DPFPACOMP - accelerated
foo <- all_years[all_years$YEAR <= 17
                 ,c('a1_all_rs','DPFEAINSP',
                    'DZEXADMP',
                    'DZEXADSP',
                    'DZEXPLAP',
                    'DZEXOTHP',
                    'DPFPAREGP',
                    'DPFPASPEP',
                    'DPFPACOMP',
                    'DPFPABILP',
                    'DPFPAVOCP',
                    'DPFPAGIFP',
                    'DPFPAATHP',
                    'DPFPAHSAP',
                    'DPFPREKP',
                    'DPFPAOTHP'
                 )]
foo2 <- melt(foo, 'a1_all_rs')
p1 <- ggplot(foo2, aes(value, a1_all_rs)) +  geom_point() + facet_grid(.~variable)
plot(p1)

#boxplot by community type
ggplot(data=all_years[all_years$YEAR <= 17,],aes(x=COMMTYPE,y=a1_all_rs))+geom_boxplot()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))
#district size
# more outliers for smaller size
ggplot(data=all_years[all_years$YEAR <= 17,],aes(x=DISTSIZE,y=a1_all_rs))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#property wealth
#A LOT of categories, narrow?
#small correlation between increase in wealth and scores
#really poor is really bad scores
ggplot(data=all_years[all_years$YEAR <= 17,],aes(x=PROPWLTH,y=a1_all_rs))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

all_years$PROPWLTHNUM<-as.numeric(gsub(",","",str_extract(all_years$PROPWLTH, "[0-9]{3},[0-9]{3}|[0-9]{1},[0-9]{3},[0-9]{3}")))
#plot(all_years$PROPWLTHNUM, all_years$a1_all_rs)
plot(all_years[all_years$YEAR <= 17 & all_years$PROPWLTHNUM < 1000000,c('a1_all_rs','PROPWLTHNUM')])

#tax rate
# need to recode, may be correlation between higher taxes and better scores
ggplot(data=all_years[all_years$YEAR <= 17,],aes(x=TAXRATE,y=a1_all_rs))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
all_years$TAXRATENUM<-as.numeric(str_extract(all_years$TAXRATE, "[0-9]{1}.[0-9]{4}"))
plot(all_years[all_years$YEAR <= 17,c('a1_all_rs','TAXRATENUM')])


#district size is a range, many nas, check layout across years
#tax rate and propwlth are ranges like district, need to translate

## run linear correlation for all columns against a1_all_rs
DISTSIZE
COMMTYPE
PROPWLTH
TAXRATE

# create new variables
# a1_all_d - total tested

#percentage at risk; Not risk no, no info, at risk yes
#a1_atr[nvy]_d 
all_years$PCT_AT_RSK <- all_years$a1_atry_d/all_years$a1_all_d

# percentage bilingual info
# a1_bil[2345nvy]; bil early exit, bil late exit, dual lang/two way
# dual lang/ one way, not billingual, no info, billing codes 2,3,4,5
all_years$PCT_BILING <- all_years$a1_bily_d/all_years$a1_all_d

# percentage, economic codes
# a1_eco[129nvy]; free meals, reduced price, other econ, not econ
# no econ info, yes econ 1-2-9
all_years$PCT_ECON_DIS <- all_years$a1_ecoy_d/all_years$a1_all_d

# percentage bilingual or esl
# a1_esbi[nvy]_d; not bilingual , no info on both, either bil or esl
all_years$PCT_ESBI <- all_years$a1_bily_d/all_years$a1_all_d

# esl content based percentage
# a1_esl[23nvy]_d; esl content based, esl pull-out, Not ESL,
# no info, esl codes 2 3
all_years$PCT_ESL <- all_years$a1_esly_d/all_years$a1_all_d

# percentage races
# a1_eth[2abhipvw]_d; 2+, asian, black, hisp, Nat.Am., PacificIs.,
# no info, white
all_years$PCT_asian <- all_years$a1_etha_d/all_years$a1_all_d
all_years$PCT_black <- all_years$a1_ethb_d/all_years$a1_all_d
all_years$PCT_hisp <- all_years$a1_ethh_d/all_years$a1_all_d
all_years$PCT_white <- all_years$a1_ethw_d/all_years$a1_all_d

# percentage gifted
# a1_gif[nvy]_d; No, no info, yes
all_years$PCT_GIF <- all_years$a1_gify_d/all_years$a1_all_d

#percentage LEP (limited english proficiency)
#a1_lep[0cfrstv]_d; other, current, 1st yr, 4th year, 2nd year
# 3rd year, no info
all_years$PCT_LEP0 <- all_years$a1_lep0_d/all_years$a1_all_d

# percentage migrant
# a1_mig[nvy]_d; not, no info, yes
all_years$PCT_MIG <- all_years$a1_migy_d/all_years$a1_all_d

#percentage sexes
# a1_sex[fmv]_d female, male, no info
all_years$PCT_F <- all_years$a1_sexf_d/all_years$a1_all_d

# percentage special ed
# a1_spe[nvy]_d; not, no info, yes
all_years$PCT_SPE <- all_years$a1_spey_d/all_years$a1_all_d

#Title I
# a1_ti[10/16/17/18/19/nvy]_d; non-participant (not previous)
#, schoolwide, targeted
# non-particpant (previous), homeless particpant non title I
# not title-I codes 0 8, no info, title I codes 6 7 9
all_years$PCT_TIT1 <- all_years$a1_ti1y_d/all_years$a1_all_d

#vocational
# a1_voc[12nvy]_d; state-approved, district courses, not tech, no info
# codes 1 2
all_years$PCT_VOC <- all_years$a1_vocy_d/all_years$a1_all_d


# transform some scale variables from the demographic data
# what about outliers?
# what about invalid scale?

