#Verro Ejiba
#Homework 5
#1 Print to the console all methods and attributes associated with a dataframe
data("diamonds")
attributes(diamonds)
#methods(diamonds)
str(diamonds) 
#check methods again
#Determine the number of columns in a dataframe
length(diamonds)
summary(diamonds)
#diamonds[1:20,]
#2 Determine how many rows are in a dataframe
length(row(diamonds))
#3 
#Extract the column names from a dataframe
names <- colnames(diamonds)
#print the names of the column names (one per line) to the console
for (i in 1:length(names)){
  print(names[i])
}
#4 Determine the type of each column (numeric, factor, logical, etc)
coltypes <- c(sapply(diamonds, class))
#colTypes <- c(typeof(diamonds))
#5 Code that will loop through any dataframe and calculate the mean of every numeric column
for (i in diamonds){
  if ( is.numeric(i) == T){
    print(mean(i))
  }
}
#print(coltypes[i])


#Label the output with the name of the column
