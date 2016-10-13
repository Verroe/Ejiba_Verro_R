#Verro Ejiba
#Homework 5
#First download package that has diamond data frame
install.packages("ggplot2")
library("ggplot2")
#1 Print to the console all methods and attributes associated with a dataframe
df = data.frame(diamonds)
attributes(df) #Gets the attributes of the dataframe
str(df) #structure of data frame
#Determine the number of columns in a dataframe
ncol(df)
#summary(diamonds)
#2 Determine how many rows are in a dataframe
nrow(df)
#3 
#Extract the column names from a dataframe
colnames(df)
#print the names of the column names (one per line) to the console
for (i in 1:ncol(df)){
  print(colnames(df[i]))
}
#4 Determine the type of each column (numeric, factor, logical, etc)
coltypes <- c(sapply(df, class))
coltypes
#5 Code that will loop through any dataframe and calculate the mean of every numeric column
for (i in (1:ncol(df))){
  m <- df[,i]
  if ( is.numeric(m) == T){
    print(colnames(df)[i])#Label the output with the name of the column
    print(mean(m)) 
  }
}
#6 Loop through any dataframe and create frequency table for every factor column.
for (i in (1:ncol(df))){
  f <- df[, i] #extract the column of diamonds
  if (is.factor(f) == T){
    print(colnames(df)[i]) #Label the output with the name of the column
    print(table(f)) #print the frequency
  }
}

#7
for (j in my){
  print(which(is.na(j)))
}
