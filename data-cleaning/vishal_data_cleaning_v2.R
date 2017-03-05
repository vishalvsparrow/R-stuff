rm(list=ls(all.names = TRUE)) #removes the hidden objects too which start with .
gc()
setwd('C:\\Users\\vishal\\Google Drive\\CSUF\\ISDS-574\\HW1')

raw_data = read.csv('ToyotaCorolla_new.csv',header=TRUE,na.strings = '')

#I now select the variables which I think would be good predictors of price, and elimnate the rest
#For this I plot the heatmap and see which entries to delete

source('imageMatrix.R')
myImagePlot(is.na(raw_data))

#Set the threshold to 30%

cols_to_delete = c()
threshold = 0.3

for (col.index in 1:ncol(raw_data))
{
  is.false = table(is.na(raw_data[,col.index]))["FALSE"]
  is.true = table(is.na(raw_data[,col.index]))["TRUE"]
  #print (colnames(raw_data)[col.index])
  #print (table(is.na(raw_data[,col.index])))
  print("----------")
  
  if(is.na(is.false) || is.na(is.true))
  {
    next
  }
  
  else
  {
    no.false = table(is.na(raw_data[,col.index]))[["FALSE"]]
    no.true = table(is.na(raw_data[,col.index]))[["TRUE"]]
    
    if(no.true/no.false > threshold)
    {
      print (colnames(raw_data)[col.index])
      cols_to_delete <- append(cols_to_delete,col.index)
      
    }
    
  }
}

#I find that the features Fuel Type, Radio Cassette, and KM have about 30% missing values.
#Therefore, I must eliminate them

new_data = raw_data[,- cols_to_delete]

#I see that there seem to be rows with missing values which are more than 30%

no.rows = nrow(new_data)
no.cols = ncol(new_data)

row.index = rep(NA, no.rows)
row.index_delete = rep(NA, no.rows)

#I will repeat this for-lopp (a tad modified) below. I should start making functions in R
  
for(i in 1:no.rows)
{
  no.missing_rows = 0
  
  row.index[i] = (sum(is.na(new_data[i,])) == 0)
  
  no.missing_rows = sum(is.na(new_data[i,]))
  
  if(no.missing_rows/no.cols < threshold)
  {
    row.index_delete[i]  = TRUE
    
  }
  else
    row.index_delete[i] = FALSE
  
}

table(row.index_delete)

new_data = new_data[row.index_delete,]

dev.off() #In case R throws a graphics error

myImagePlot(is.na(new_data))

#------------- SOME PART OF THE CODE BELOW IS MANUAL (FILE SPECIFIC) EXAMPLE: DUMMY VARIABLES ------#

#Summary for the Color categorical variable
table(new_data$Color)

#Creating dummy variables for colors
new_data$black = new_data$blue = new_data$green = new_data$grey = new_data$red = new_data$other.color = 0 

new_data$black[which(new_data$Color == 'Black')] = 1
new_data$blue[which(new_data$Color == 'Blue')] = 1
new_data$green[which(new_data$Color == 'Green')] = 1
new_data$grey[which(new_data$Color == 'Grey')] = 1
new_data$red[which(new_data$Color == 'Red')] = 1
new_data$other.color[which(new_data$Color %in% c('Beige', 'Silver', 'Violet', 'White', 'Yellow'))] = 1

#Delete the Colors column

new_data <- new_data[,-which(colnames(new_data)=="Color")]

#summary for Color categorical variables

table(new_data$black)
table(new_data$red)
table(new_data$other.color)
table(new_data$Gears)

#Check the number of missing values in each column

na_count <-sapply(new_data, function(y) sum(length(which(is.na(y))))>0)
na_count <- new_data[na_count]

#View(na_count)

#In order to check if the variables are categorical, we will count the number of unique values contained
#If this value is greater than 11, we will assume the variable to be continious, else categorical
threshold.continious = 9
continious.col_index <- c()

for(i in 1:length(na_count))
{
  if(length(unique((na_count[[i]])))> threshold.continious)
  {
    continious.col_index <- append(continious.col_index,i)
    temp_1 = NULL
    temp_1 = na_count[[i]]
    temp_1[is.na(temp_1)] = round(mean(temp_1, na.rm = TRUE),2) 
    na_count[[i]] = temp_1
  }
}

#View(na_count)

#Merging the two data frames to make a non-missing new_data dataframe

for(i in 1:length(new_data))
{
  for(j in 1:length(na_count))
  {
    if((colnames(new_data[i]) == colnames(na_count[j])))
      {
         new_data[i] = na_count[j]
      }
  }
  
}

########################### REPEATED CODE -- DO NOT SKIP #############################
#Finally, deleting any row with a missing value 
row.index = row.index_delete = rep(NA, nrow(new_data))

#View(new_data)

for(i in 1:nrow(new_data))
{
  no.missing_rows = 0
  
  row.index[i] = (sum(is.na(new_data[i,])) == 0)
  
  no.missing_rows = sum(is.na(new_data[i,]))
  
  #if(no.missing_rows/no.cols < threshold)
  #{
    #row.index_delete[i]  = TRUE
    
  #}
  #else
   # row.index_delete[i] = FALSE
  
  if(no.missing_rows > 0)
    row.index_delete[i]  = TRUE
  else
    row.index_delete[i] = FALSE
    
}

new_data = new_data[!(row.index_delete),]
#Just for sake
na_count = na_count[!(row.index_delete),]
############################# END OF REPEATED CODE#####################

#View(new_data)

#Now, we will remove all the continious variables from the na_count data
#This way, we are left with only categorical variables in na_count
na_count <- na_count[,-continious.col_index]
continious.col_index = NULL #Safety Check
View(na_count)
#Now that I think about it, na_count has no use (but it may have in future). Silly me


######################OUTLIER CODE####################
#I will now plot histograms for a few continious variables

par(mfrow=c(1, 2))
hist(new_data$Weight)
boxplot(new_data$Weight)

hist(new_data$CC)
boxplot(new_data$CC)

#Function for detecting outliers
detect_outliers <- function(my_data){

  q1 = quantile(my_data,0.25,na.rm=TRUE)[["25%"]]
  q3 = quantile(my_data,0.75,na.rm=TRUE)[["75%"]]
  
  IQR = q3-q1
  upper.limit = q3+(1.5*IQR)
  lower.limit = q1-(1.5*IQR)
  
  temp_data = rep(NA, length(my_data))
  
  for(j in 1:length(my_data))
  {
    if(my_data[j] < lower.limit || my_data[j] > upper.limit)
    {
      temp_data[j] = FALSE
    }
    else
      temp_data[j] = TRUE
  }
  return(temp_data)
}

#Remove outliers for Weight and CC
new_data <- new_data[detect_outliers(new_data$Weight),]
new_data <- new_data[detect_outliers(new_data$CC),]

boxplot(new_data$Weight)
boxplot(new_data$CC)

#Hence, the new Weight feature is free of outliers
#######################END OF OUTLIER CODE#############################

#From na_count, I see that there are only Doors and Gears which need dummy variables
#Creating dummy variables for Gears & Doors 

#For Gears
new_data$Gears.fifth = new_data$Gears.others = 0

new_data$Gears.fifth[which(new_data$Gears == 5)] = 1

new_data$Gears.others[which(new_data$Gears %in% c(1,2,3,4,6))] = 1

#For Doors
new_data$Doors.five = new_data$Doors.three = new_data$Doors.others = 0

new_data$Doors.five[which(new_data$Doors == 5)] = 1

new_data$Doors.three[which(new_data$Doors == 3)] = 1

new_data$Doors.others[which(new_data$Doors %in% c(1,2,4))] = 1

#Delete the Gears and Doors columns
new_data <- new_data[,-which(colnames(new_data)=="Gears")]
new_data <- new_data[,-which(colnames(new_data)=="Doors")]

#Renaming the rows so that they are nicely in order
rownames(new_data) <- NULL

#myImagePlot not working --- ISSUE on 3/4/2017
source('imageMatrix.R')
myImagePlot(is.na(new_data))

#-----------ISSUE PARTLY RESOLVED-----------
#The heatmap will show you that there are still some missing values. 
#This is because I have only removed the rows with 30% or more missing values
#and not treated the missing values yet. This is one of the questions I have to ask

#UPDATE March 3rd 2017
#It seems like it is finally the time to treat the missing examples.
#Done it in the code above #Update March 4th 2017
#However, cannot get heatmap to work
#-----------ISSUE END------------

write.csv(new_data,file = "vishal_cleaned_with_R_1.csv")
