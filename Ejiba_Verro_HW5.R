#Verro Ejiba
#Homework 5
#Group C
#First download package that has diamond data frame
install.packages("ggplot2")
library("ggplot2")
#1 Print to the console all methods and attributes associated with a dataframe
df <- data.frame(diamonds)
methods(class = data.frame)
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
#omit the missing data then computes the mean of the columns with numeric variables 
print(lapply(na.omit(df)[, sapply(na.omit(df), is.numeric)], mean))

#6 Loop through any dataframe and create frequency table for every factor column.
#extract the columns that have factor variable and creates its frequency table
print(lapply(df[, sapply(df, is.factor)], table))

#7
#prints the total number of rows containing mmissing value in each column 
print(sum(apply(is.na(df), 1, any)))
#Get the percentage of rows containing NA in any columns by the dividing the sum above by the total number of rows then multiply the result by 100
100*(sum(apply(is.na(df), 1, any))/nrow(df))
#8 
f <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
  #and their corresponding Pearson correlation coefficient in the second column.
  #Parameters: A dataframe
  #Returns: A dataframe with pair of column names and its correlation using pearson method
  
  name_pairs <- cbind() 
  correlations <- cbind()
  #remove NA in the data
  ndata <- na.omit(dataframe)
  mtx <- ndata[sapply(ndata, is.numeric)], method = "pearson"
  # for (i in (1:ncol(dataframe))){
  #   name1 = names(diamonds)[i]  #getting the first column name
  #   
  #   for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one
  #     if  (is.na(names(diamonds)[j]) == FALSE){
  #       if (names(diamonds)[j] != names(diamonds)[i]){
  #         name2 =names(diamonds)[j]}   #getting the second column name
  #       name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
  #       
  #       if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the correlation for numeric columns
  #         if (is.numeric(dataframe[,name2]) == TRUE){
  #           correlations<- rbind(correlations, cor(dataframe[,name1],dataframe[,name2], method="pearson"))}#calculating the pearson correlations
  #         else{
  #           correlations<- rbind(correlations, NA)  #for non-numeric columns, the correletion is NA to avoid errors
  #         }}
  #       else{
  #         correlations<- rbind(correlations, NA)
  #       }
  #       
  #     }
  #   }
  #   
  # }
  # 
  # output = data.frame(name_pairs,correlations) #combining two columns
  return(output)
}


print(f(diamonds))

