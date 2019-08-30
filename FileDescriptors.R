#Concatenate File Descriptors

library(readxl)

# Read in Data from csv files
path <- "E:/STAT685/Data/"
#on the test data
fy19 <- read_excel(paste(path, 'fy19_varlist_a1.xlsx',sep=''))
fy18 <- read_excel(paste(path, 'fy18_varlist_a1.xls',sep=''))
fy17 <- read_excel(paste(path, 'fy17_varlist_a1.xls',sep=''))
fy16 <- read_excel(paste(path, 'fy16_varlist_a1.xls',sep=''))
fy15 <- read_excel(paste(path, 'fy1415_varlist_a1.xls',sep=''))
fy14 <- read_excel(paste(path, 'fy1314_varlist_a1.xls',sep=''))
fy13 <- read_excel(paste(path, 'fy1213_varlist_a1.xls',sep=''))

#reanme columns
names(fy19) <- c('Variable','Format_19', 'Desc19')
names(fy18) <- c('Variable','Format_18', 'Desc18')
names(fy17) <- c('Variable','Format_17', 'Desc17')
names(fy16) <- c('Variable','Format_16', 'Desc16')
names(fy15) <- c('Variable','Format_15', 'Desc15')
names(fy14) <- c('Variable','Format_14', 'Desc14')
names(fy13) <- c('Variable','Format_13', 'Desc13')

#join data by file descriptor
desc_set <- merge(fy19, fy18, by='Variable',all=TRUE)
desc_set <- merge(desc_set, fy17, by='Variable',all=TRUE)
desc_set <- merge(desc_set, fy16, by='Variable',all=TRUE)
desc_set <- merge(desc_set, fy15, by='Variable',all=TRUE)
desc_set <- merge(desc_set, fy14, by='Variable',all=TRUE)
desc_set <- merge(desc_set, fy13, by='Variable',all=TRUE)


#print to excel again
write.csv(desc_set, file=paste(path, 'desc_set.csv', sep=''))
