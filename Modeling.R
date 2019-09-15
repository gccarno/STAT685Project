## Modeling

library(MASS)
library(ISLR) #should be using leaps?
library(leaps)

## Variables to consider
## 
## Response variables
## a1_all_rs
##
## YR
## m_all_rs, 
## DA0AT16R, DA0912DR16R, DAGC4X16R, DA0GR16N
## DA0CT16R, DA0CC16R, DA0CSA16R
## DPSTTOSA
## DPST05FP, DPSTADFP, DPSTURNR, DPSBLFP, DPSTHIFP, DPSTWHFP, DPSTREFP, DPSTSPFP
## DPFRASTAP, DZRVLOCP, DPFRAFEDP
## DPFPAREGP, DPFPACOMP
## PROPWLTHNUM, TAXRATENUM
##
## PCT_AT_RSK, PCT_BILING, PCT_ECON_DIS, PCT_ESBI, PCT_ESL, 
## PCT_asian, PCT_black, PCT_hisp, PCT_white
## PCT_GIF, PCT_LEP0, PCT_MIG, PCT_F, PCT_SPE, PCT_TIT1, PCT_VOC
##
##
# may only be able to use years 14, 15, 16, 17
# what about cross validation?

## only valid years 
sub_all_years <- all_years[all_years$YEAR > 13 & all_years$YEAR < 18,]

##Run PCA


## does using the more expanded data set with less predictors lead to a
## better prediction accuracy?

##boxcox
ones <- rep(1, length(sub_all_years$a1_all_rs))
a <- boxcox(lm(sub_all_years$a1_all_rs ~ ones),lambda = seq(-4, 2, 1/10))
max_x_for_y<- function(BC){
  with(BC, x[which.max(y)])
}
max_x_for_y(a)

shapiro.test(sub_all_years$a1_all_rs**-1.63)
qqnorm(sub_all_years$a1_all_rs**-1.63)

##
hist(sub_all_years$a1_all_rs)
d1 <- density(na.omit(sub_all_years$a1_all_rs))
plot(d1)

hist(sub_all_years$a1_all_rs**-1.64)
d1 <- density(na.omit(sub_all_years$a1_all_rs**-1.64))
plot(d1)

# train test split
## 70% of the sample size
smp_size <- floor(0.70 * nrow(sub_all_years))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(sub_all_years)), size = smp_size)

train <- sub_all_years[train_ind, ]
test <- sub_all_years[-train_ind, ]

## Impute, investigage missing?
## Too many NAs for DA0AT16R, DA0912DR16R        
## DAGC4X16R         ,DA0GR16N     , DA0CT16R         ,DA0CC16R       
## PCT_ESBI caused singularities?
vars_to_use <- c('YEAR','m_all_rs','DPSTTOSA', 'a1_all_rs', 'a1_all_d'
                 ,'DPST05FP','DPSTADFP','DPSTURNR','DPSTBLFP','DPSTHIFP'
                 ,'DPSTWHFP','DPSTREFP','DPSTSPFP','DPFRASTAP','DZRVLOCP'
                 ,'DPFRAFEDP','DPFPAREGP','DPFPACOMP','PROPWLTHNUM','TAXRATENUM'
                 ,'PCT_AT_RSK','PCT_BILING','PCT_ECON_DIS','PCT_ESL', 
                 'PCT_asian','PCT_black','PCT_hisp','PCT_white','PCT_GIF','PCT_LEP0'
                 ,'PCT_MIG','PCT_F','PCT_SPE','PCT_TIT1','PCT_VOC')
for (m_var in vars_to_use){
  print(m_var)
  summary(train[, m_var])
}
summary(train[ ,vars_to_use])

##Principal Components

## Weighted Linear Regression
## SLR
train_na_rm <- na.omit(train[, vars_to_use])
test_na_rm <- na.omit(test[, vars_to_use])
m0 <- lm(a1_all_rs ~ .,data=train_na_rm)
summary(m0)
par(mfrow=c(2,2))
plot(m0)

#predict
m0.p <- predict(m0,test_na_rm)

#MSE
mean((m0.p-test_na_rm$a1_all_rs)**2)


## remove na from training
## adding weights showed large improvement
m1 <- lm(a1_all_rs ~ .,data=train_na_rm, weights = a1_all_d)

summary(m1)
par(mfrow=c(2,2))
plot(m1)

#predict
m1.p <- predict(m1,test_na_rm)

#MSEPE
mean((m1.p-test_na_rm$a1_all_rs)**2)

## remove na from training
## adding weights showed large improvement
m3 <- lm(a1_all_rs**-1.636 ~ .,data=train_na_rm, weights = a1_all_d)
m4 <- lm(a1_all_rs**-1.636 ~ .,data=train_na_rm)

boxcox(m4)

summary(m3)
par(mfrow=c(2,2))
plot(m3)
summary(m4)
par(mfrow=c(2,2))
plot(m4)

#predict
m3.p <- predict(m3,test_na_rm)
m4.p <- predict(m4,test_na_rm)

#MSEPE
mean((m0.p-test_na_rm$a1_all_rs)**2)
mean((m1.p-test_na_rm$a1_all_rs)**2)
mean((m3.p**(-1/1.636)-test_na_rm$a1_all_rs)**2)
mean((m4.p**(-1/1.636)-test_na_rm$a1_all_rs)**2)

#large matrix
##singluarities produced by some variables
singulars <- c('a1_all_d','a1_sexv_d','a1_ethv_d','a1_eco9_d','a1_ecov_d',
  'a1_til8_d','a1_ti9_d','a1_tilv_d','a1_migv_d',
  'a1_lepv_d','a1_bil5_d','a1_bilv_d','a1_eslv_d',
  'a1_esbiy_d','a1_esbiv_d','a1_esbiy_d','a1_esbiv_d',
  'a1_spev_d','a1_gifv_d','a1_atrv_d','a1_vocv_d','a1_all_docs_r',
  'PCT_ESBI')

#other variables to remove
c('REGION')
# train test split, using a column
## 70% of the sample size
smp_size <- floor(0.70 * nrow(val_years_n_vars2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(numeric_cols)), size = smp_size)

train <- numeric_cols[train_ind, ]
test <- numeric_cols[-train_ind, ]

#extract values where it is na
m0 <- lm(a1_all_rs ~ .,data=train)
sing_val_coef <- which(is.na(m0$coefficients))
summary(m0)
par(mfrow=c(2,2))
plot(m0)

#predict
m0.p <- predict(m0,test_na_rm)

#MSE
mean((m0.p-test_na_rm$a1_all_rs)**2)



m0.num <- lm(a1_all_rs ~ .,data=train[,!is.na(m0$coefficients)&!(colnames(train)='a1_all_d')])
summary(m0.num)
sing_val_coef2<-c(names(sing_val_coef),'a1_all_d','REGION.x','REGION.y',
                  'a1_all_docs_n','a1_all_abs_n','a1_all_oth_n',
                  'a1_all_abs_r','a1_all_oth_r')
#should I include absent and other numbers tested
#removing some of the absent variables
#
# run again without the bad coefficients
m1.num <- lm(a1_all_rs ~ .,data=train[,-which(names(train) %in% sing_val_coef2)])
summary(m1.num)
par(mfrow=c(2,2))
plot(m1.num)

## getting perfect corelation, investigate

## need cross validation

##STEPWISE
## preliminary tests showed boxcox was effective
## keeping weights for more valid model
##cannot do weighted with leaps?
regfit.best=regsubsets(m3,data=train_na_rm)
regfit.best=regsubsets(a1_all_rs**-1.636~.,data=train_na_rm)
train[,!is.na(m0$coefficients)]
regfit.full=regsubsets(a1_all_rs**-1.636~.,data=train[,!is.na(m0$coefficients)]
                       ,nvmax=70,weights=train$a1_all_d,method=c('forward'))
reg.summary=(summary(regfit.full))
plot(reg.summary$bic ,xlab =" Number of Variables ",
     ylab=" Adjusted RSq",type="l")
which.min (reg.summary$bic )
par(mfrow=c(1,1))
plot(regfit.full)

regfit.full=regsubsets(a1_all_rs**-1.636~.
                       ,data=train[,-which(names(train) %in% sing_val_coef2)]
                       ,nvmax=70,weights=train$a1_all_d,method=c('forward'))
reg.summary=(summary(regfit.full))
plot(reg.summary$bic ,xlab =" Number of Variables ",
     ylab=" Adjusted RSq",type="l")
which.min (reg.summary$bic )
par(mfrow=c(1,1))
plot(regfit.full)

## Unweighted

## Lasso

## Random forest

## need to do validation of the data


