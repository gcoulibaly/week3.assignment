week3.assignment
================
#1Function that returns the number of missing value, enter as NA
vector<-function(x)
{
  sum (is.na(x))
}

vector(c(1,2,3,NA, NA))

#2) Function that takes a data frame and return the number of missing values
df<- function(x, y,z)
{
 df<-data.frame(x, y,z)
 sum(is.na(df))
}
df(c(1,2,3,NA), c(1,2,3,NA),c(1,2,3,NA))

#3) Function that returns the minimum, maximum, mean, median, first quartle, third quartile, standard deviation, number of missing values
vector1<-function(x)
{
 
  print(sum(is.na(x)))               #number of missing values
  print(mean(x, na.rm=T))            #mean
  print(min(x, na.rm=T))              #minimum
  print(max(x, na.rm=T))              #maximum
  print(median(x, na.rm=T))         #median
  print(sd(x, na.rm=T))              #standart deviation
  
                     
}

vector1(c(1,2,3,4,NA))
  

#4) Function that returns number of distinct elements, mode, frequency of the mode, number of missiing values
vector2<-function(x)
{
  print(length(unique(x)))
  print(names(which(table(x)==max(table(x)))))
  print(max(table(x)))
  print(sum(is.na(x)))
}
vector2(c(1,2,3,3,3,3,3,4,5,3,6,NA))

#5) Function that return the number of: TRUE values, FALSE values; 
#proportion = #of Trues divide by # of total elements include the missing values,
#number of misssing values

vector5<-function(x)
{
  print(sum(x,na.rm=TRUE))
  print(sum(!is.na(x))- sum(x,na.rm=TRUE))
  print((sum(x,na.rm=TRUE))/(length(x)))        #this proportion includes the number of missing values
  print((sum(x,na.rm=TRUE))/(sum(!is.na(x))))    #this proportion excludes the number of missing values
  print(sum(is.na(x)))
}

vector5(c(TRUE,TRUE,TRUE,FALSE, FALSE, NA, NA, NA, NA))



#6) Functions that takes input as data frame and returns a summary

vector6<-function(vector1,vector2, vector5)
{ 
    tapply(vector1$mean, vector2$max(tabale(x)), vector$sum(!is.na(x)))
}

vector6(c(2,3,4),c(2,3,2),c(2,3,NA))

