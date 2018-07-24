
#sIMULATION WITH A MEAN = 5 AND SD = 2
#AFTER A SAMPLING FROM A UNIFORM DISTRIBUTION, WE REPLICATE

#sampling from a size of 100
norm.simulated <- rnorm(n=100, mean = 5, sd=2)

#Perform the same operation like for more 1000 times
replicate(100, norm.simulated)

#set the appeareance, like the row and column coverage
par(mfrow=c(3,1))

#visualise the result on a graph
plot(norm.simulated)

#you can as well use the histogram to view your results
hist(norm.simulated)

#What we have just done is known as monte carlo simulation
#But we didn't really use any of the packages for Monte Carlo
#Next is the description of the Monte Carlo package
#It has two packages, namely montecarlo() and maketable()



#Description of MonteCarlo package
#############################################
##          Example: t-test
#Load the library
library(MonteCarlo)

#Define a function that generate the data
##########################################################
#       Parameters: n = the sample size to use           #
#                   loc = the mean variable              #
#                   scale = the standard deviation       #
#########################################################

ttest <- function(n, loc, scale){
  #generate the sample from a normal decision: 
  sample <- rnorm(n, loc, scale)
  
  #calculate the test statistics
  stat <- sqrt(n)*mean(sample)/sd(sample)
  
  #get the test decision
  decision<-abs(stat)>1.96
  
  #return the results in the form of list
  return(list("Decision" = decision))
  
}
#Define the parameter grid
n_grid <- c(50,100,250,500)
loc_grid <- seq(0,1,0.2)
scale_grid <- c(1,2)

#Collect the grid parameters together as a list
param_list <- list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)

#Run the simulation using function MonteCarlo()
#MonteCarlo() takes ttest() and the parameter grid, param_list
#as arguments and number of repetition, nrep

MC_results <- MonteCarlo(func = ttest, nrep = 1000,
                         param_list = param_list)

summary(MC_results)

# generate table:

MakeTable(output=MC_results, rows="n", 
          cols=c("loc","scale"), digits=2, include_meta=FALSE)


#########################################################################
#                                                                       #
#  Application of Monte Carlo Simulation in Batman Equation             #
#                                                                       #
#########################################################################

#Here is the raw equation of the batman broken into pieces
#Later, i will combine it to form the real Batman equation

a = (3*sqrt(4-(abs(x)-2)^2)+abs(x)-20-4*y)

b = (3*sqrt(4-(abs(x)-6)^2)+abs(x)-20-4*y)

c = (x2+4*y2-100*sqrt(abs((7-abs(2*y-1)))/(7-abs(2*y-1))))

d = (2*(abs(x)-3)^2-9*y+18*sqrt(abs((2-abs((abs(x)-4))))
                                /(2-abs((abs(x)-4)))))

e = (-68*abs((abs(x)-3/2))-9*y+54*sqrt(abs((43-abs((136*abs(x)-229))))
                                       /(43-abs((136*abs(x)-229)))))

f = (y-5*sqrt(abs((1-abs(x)))/(1-abs(x))))

#Here is now the full raw equation of Batman
a * b * c * d * e * f = 0

#To generate the Batman curve using Monte Carlo Simulation

#Load the package ggplot2 that will help us plot the curve
require(ggplot2)

f1 <- function(x) {
  y1 <- 3*sqrt(1-(x/7)^2)
  y2 <- -3*sqrt(1-(x/7)^2)
  y <- c(y1,y2)
  d <- data.frame(x=x,y=y)
  d <- d[d$y > -3*sqrt(33)/7,]
  return(d)
}

x1 <- c(seq(3, 7, 0.001), seq(-7, -3, 0.001))
d1 <- f1(x1)
p1 <- ggplot(d1,aes(x,y)) + geom_point(color="red")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#                                                                      $
#   There is an error with the below codes, the aes() isn't clear      $
#               in the ggplot2 package unlike in ggplot                $
#                                                                      $
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

x2 <- seq(-4,4, 0.001)
y2 <- abs(x2/2)-(3*sqrt(33)-7)*x2^2/112-3 + sqrt(1-(abs(abs(x2)-2)-1)^2)

p2 <- p1 + geom_point(aes(x=x2,y=y2, color="yellow"))


x3 <- c(seq(0.75,1,0.001), seq(-1,-0.75,0.001))
y3 <- 9-8*abs(x3)
p3 <- p2+geom_point(aes(x=x3,y=y3), color="green")

x4 <- c(seq(0.5,0.75,0.001), seq(-0.75,-0.5,0.001))
y4 <- 3*abs(x4)+0.75
p4 <- p3+geom_point(aes(x=x4,y=y4), color="steelblue")

x5 <- seq(-0.5,0.5,0.001)
y5 <- rep(2.25,length(x5))
p5 <- p4+geom_point(aes(x=x5,y=y5))

x6 <- c(seq(-3,-1,0.001), seq(1,3,0.001))
y6 <- 6 * sqrt(10)/7 +
  (1.5 - 0.5 * abs(x6)) * sqrt(abs(abs(x6)-1)/(abs(x6)-1)) -
  6 * sqrt(10) * sqrt(4-(abs(x6)-1)^2)/14
p6 <- p5+geom_point(aes(x=x6,y=y6), colour="blue")

p <- p6+theme_bw()
print(p)




#########################################################################
#                                                                       #
#  Application of Monte Carlo Simulation in Monte Carlo Integration     #
#                                                                       #
#########################################################################



