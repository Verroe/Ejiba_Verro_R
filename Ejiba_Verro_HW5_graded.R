#Verro Ejiba
#Homework 5
#First download package that has diamond data frame
install.packages("ggplot2")
library("ggplot2")
#1 Print to the console all methods and attributes associated with a dataframe
df = data.frame(diamonds)
attributes(df) #Gets the attributes of the dataframe
str(df) #structure of data frame
#Stuart- methods(class = 'data.frame') will list all the methods associated with data frames
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
#Stuart- It would be best for these to be written as functions 
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

#7 still needs review
#Stuart- a good start, but I think you want to store the misVal's and then to aggregate those values in
#a seperate for loop, currently you are reassigning a value to misVal each iteration of the loop
for (j in (1:nrow(df))){
  r <- df[j,]
  misVal <- c(which(is.na(r)))
  i = 1:length(misVal)
  for (val in misVal) {
    #print(val[i])
    print(sum(val[i]))
  } 
}

#8 
f <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
  #and their corresponding Pearson correlation coefficient in the second column.
  #Parameters: A dataframe
  #Returns: A dataframe
  
  name_pairs <- cbind() 
  correlations <- cbind()
  
  for (i in (1:ncol(dataframe))){
    name1 = names(diamonds)[i]  #getting the first column name
    
    for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one
      if  (is.na(names(diamonds)[j]) == FALSE){
        if (names(diamonds)[j] != names(diamonds)[i]){
          name2 =names(diamonds)[j]}   #getting the second column name
        name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
        
        if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the correlation for numeric columns
          if (is.numeric(dataframe[,name2]) == TRUE){
            correlations<- rbind(correlations, cor(dataframe[,name1],dataframe[,name2], method="pearson"))}#calculating the pearson correlations
          else{
            correlations<- rbind(correlations, NA)  #for non-numeric columns, the correletion is NA to avoid errors
          }}
        else{
          correlations<- rbind(correlations, NA)
        }
        
      }
    }
    
  }
  
  output = data.frame(name_pairs,correlations) #combining two columns
  return(output)
}


print(f(diamonds))

#Stuart- Good job, you can omit non-numeric columns in to clean your table output
