#Verro Ejiba
#Homework 6
#Group C

#Stuart- Your code is good, but it needs more documentation.

#Get data from package
install.packages("ggplot2")
library("ggplot2")
data("diamonds")
#2 Scatter plot of weight("Carat") and Price using Color(the "color" column in the diamonds dataframe )
ggplot(diamonds, aes(carat,price)) + geom_point(aes(colour = factor(color))) + xlab("Weight") + ylab("Price") + labs(title ="Diamonds - Weight to Price by Color")

#3 Natural log transformation price
ggplot(diamonds, aes(log(carat),log(price))) + geom_point(aes(colour = factor(color))) + xlab("Weight") + ylab("Price") + labs(title ="Diamonds - Weight to Price Linear")

#4 Remove the linear trend by creating a linear model and using the transformed on x-axis and the residuals on the y-axis
res = resid(lm(log(price) ~ log(carat), data = diamonds))
ggplot(diamonds, aes(log(carat),res)) + geom_point(aes(colour = factor(color))) + xlab("Weight") + ylab("Price Residuals") + labs(title ="Diamonds - Weight to Price Linear")

#5
install.packages("grid")
price_plot <- ggplot(diamonds, aes(price,..density.., colour = factor(color))) + geom_histogram(binwidth = 25) + theme(axis.title.x= element_blank(), axis.title.y= element_blank(), legend.position = "none")
carat_plot <- ggplot(diamonds, aes(carat,..density.., colour = factor(color))) + geom_histogram(binwidth = 0.085) + theme(axis.title.x= element_blank(), axis.title.y= element_blank(), legend.position = "none")
ggplot(diamonds, aes(log(carat),res)) + geom_point(aes(colour = factor(color))) + xlab("Weight") + ylab("Price Residuals") + labs(title ="Diamonds - Weight to Price Linear") + theme(legend.position ="top")
vp1 <- viewport(width = 0.4, height = 0.2, x=0.80, y =0.77)
vp2 <- viewport(width = 0.5, height = 0.3, x=.32, y=.2)
print(price_plot, vp = vp2)
print(carat_plot, vp = vp1)