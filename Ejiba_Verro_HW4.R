##Verro Ejiba
#Homework4
#1Create vectors:
#a (1,2,3, ..., 19,20)
vec1 <- c(1:20) 
#b (20, 19, ..., 2,1)
vec2 <- c(20:1) 
#c (1,2,3, ..., 19, 20, 19, 18, ..., 2,1)
vec3 <- c(vec2,vec1)
#d (4,6,3) and assign it ot the name tmp
tmp <- c(4,6,3)
#e tmp with 10 occurences of 4
rep(tmp,10)
#f tmp with 11 occurrences of 4,6,3
rep_len(tmp, 31)
#g temp with 10 occurrences of 4, 20 of 6, and 30 of 3
rep(tmp, c(10,20,30))

#2 create a vector of the values of e^x *cos(x) at x = 3,3.1, 3.2, ..., 5.9,6
f <- function(x) (exp(x)*cos(x))
x <- seq(3,6, .1)
rbind(x, f(x))

#3
#a create the following vector (0.1^3 0.2^1, 0.1^6 0.2^4, 0.1^9 0.2^7, ... , 0.1^36 0.2 ^34 )
x = c(0.1) #assigns value for x
y = c(0.2) #assigns value for y
#computes the power of eaach 0.1 
n = 12 #length
i = seq(1,n,1)
tx <- c(i*3)
#computes the power of each 0.2 
ty <- c()
for (j in 1:n){
  if (j == 1) {
    ty[1] = j
  }
  else {
    ty <- c(ty[1],ty[-j]+3)
  }
}
pwx = tx[1:n] #vector with powers of x
pwy = ty[1:n] #vector with powers of y
fun <- function(pwx, pwy) (x^pwx)*(y^pwy)
cbind(pwx,pwy, fun(pwx,pwy))
#b create the following vector (2, 2^2/2, 2^3/3, ..., 2^25/25)
len = 25 #length of the vector 
p = 1:len 
f <- function(p) (2^p)/p
cbind(p, f(p))
#4 Calculate the sum of the following 2 function 
#a i = 1:100 function -> (i^3 + 4i^2)
f <- function(i) (i^3 +4*i^2) #Define function
i = 1:100 
total = sum(f(i)) #Find the sum
print(total)
#b i=1:25 function = ((2^i)/i + (3^i)/i^2)
f2 <- function(i) ((2^i)/i + (3^i)/i^2)
i = 1:25 
total2 = sum(f2(i)) #Do summation
print(total2)
#5 Use the function paste to create the following character vectors of length 30:
#a ("label 1", "label 2", ..., "label 30"). Note that there is a single space between label and the number following.
labs <- paste(c("label"), 1:30, sep=" ") 
print(labs)
#b ("fn1", "fn2", ..., "fn30"). In this case, there is no space between fn and the number following.
fns <- paste(c("fn"), 1:30, sep="")
print(fns)
#6 Execute the following lines which create two vectors of random integersn wich are chosen with replacement from integers 0,1, ..., 999 both vectrs have length 250
set.seed(50)
xVec <- sample(0:999, 250, replace=T) #picks 250 random nnumbers between 1 and 999 
yVec <- sample(0:999, 250, replace=T)
#a Create the vector (y2 - x1, ..., yn - xn-1)
i = seq(2,length(yVec), 1) #creates a vector that selects elements at position 2,3,...,n respectively
j = seq(1,length(xVec)-1, 1) #creats a vector that selects 1,2,...,n-1 respectively
newVec <- c(yVec[i] - xVec[j]) #A vector with (y2 - x1, ..., yn - xn-1)
print(newVec)
#b Create the vector (sin(y1)/ cos(x2), sin(y2)/cos(x3), ..., sin(yn-1)/cos(xn))
k = seq(2,length(xVec), 1) #Create the x vector from 2 to len of x
l = seq(1, length(yVec),1)
trig <- c(sin(yVec[k])/cos(xVec[l]))
print(trig)
#c Create the vector (x_1+2x_2 - x_3, ..., x_n-2 + 2x_n-1 - x_n)
f1 = seq(1,length(xVec),3) #length is not a multiple of 3 need to find other alternatives
f2 = seq(2,length(xVec),3)
f3 = seq(3,length(xVec),3)
res <- c(xVec[f1]+2*xVec[f2]-xVec[f3])
#d Calculate sum i = 1:-1 of the function (e^x * i+1) / (x_i + 10)
i = 1: length(xVec)-1
fun <- function(i) (exp(-xVec[i+1]))
s = sum(fun(i))/(xVec[i]+10)
#7 Use the xVec and yVec above and functions sort, order, mean, sqrt, sum, and abs
#a. Choose values in yVec that are >600 and #b The index position in yVec of the values that are >600

for (j in 1:length(yVec)) {
    if (yVec[j] > 600 ) {
      index <- c(j)
      values <- c(yVec[j])
      valY <- cbind(index, values)
      print(valY)
    }
}
#c find the values where yVec = xVec at the same index 
for (i in 1:length(xVec)){
  if(xVec[i] >600){
    x_index <- c(i)
    x_values <- c(xVec[i])
    valX <- cbind(x_index, x_values)
    print(valX)
  }
  if (index == x_index && x_values == values){
    corr <- cbind(x_index, x_values)
    print(corr)
  }
}
#d create the vector (|x_1 - mean(x)|^1/2, ..., |x_n - mean(x)|^1/2) 
m_x <- mean(xVec)
n = seq(1,length(xVec),1)
sam <- c(sqrt(abs(xVec[n] - m_x)))
print(sam)
#e How many values in yVec are within 200 of the maximum value of the terms in yVec?
print(length((max(yVec) - 200): max(yVec)))
#f How many numbers in xVec are divisible by 2? 
print(length(which(c(xVec[n] %% 2 == 0))))
#g Sort numbers in the vector xVec in the order of increasing values in yVec
#Pick out the elements in yVec at the index 1,4,7,10, 13, ...)

for (j in 1:length(yVec)){
  if (j == 1) {
    yv[1] = j
  }
  else {
    yv <- c(yv[1],yv[-j]+3)
  }
}
print(yVec[yv])
#8 use of function cumprod
odd <- seq(3,39,2)
even <- seq(2,39,2)
sum(c(1, cumprod(even/odd)))



