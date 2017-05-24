############ Susan Cherry ##############
###### Advanced Machine Learning #######
########## Coding Assignment 3 #########
library(MASS)


######## Squared Exponential ###########

squared_exponential = function(x, y, l){
  
  #find the length of each
  len1=length(x)
  len2=length(y)
  
  #initialize a matrix
  sq_matrix= matrix(0, nrow=len1, ncol=len2)
  
  #iterate and add to the matrix
  for (i in 1:len1) {
    for (j in 1:len2) {
      sq_matrix[i,j] = exp(-((x[i]-y[j])^2)/(2*l^2))
    }
  }
  #return matrix
  return(sq_matrix)
  
}
  
  ########### Gamma Exponential ##################
  gamma_exp=function(x, y, l, gamma) {
    #find the length of each
    len1=length(x)
    len2=length(y)
    
    #initialize a matrix
    sq_matrix= matrix(0, nrow=len1, ncol=len2)
    
    #iterate and add to the matrix
    for (i in 1:len1) {
      for (j in 1:len2) {
        a=abs(x[i]-y[j])
        sq_matrix[i,j] = exp(-(a/l)^gamma)
      }
    }
    #return matrix 
    return(sq_matrix)
  }
  


  
  
  ########## Rational Quadtratic ###########
  rational_quad=function(x, y, k, alpha) {
    #find the length of each
    len1=length(x)
    len2=length(y)
    
    #initialize a matrix
    sq_matrix= matrix(0, nrow=len1, ncol=len2)
    
    #iterate and add to the matrix
    for (i in 1:len1) {
      for (j in 1:len2) {
        
        sq_matrix[i,j] = (1 + ((x[i]-y[j])^2/(2*alpha*k^2)))   ^(-alpha)
      }
    }
    #return matrix 
    return(sq_matrix)
  }
  


######## Squared Exponential Plots ##################

x_values=seq(-5,5,length=100)

########## n=1, mu=1, l=1
plot1_sigma=squared_exponential(x_values,x_values,1)
p1_norm=mvrnorm(1, rep(1, 100), plot1_sigma)
plot(x_values,p1_norm,xlab="x", ylab=" ",main="n=1, mu=1, l=1",type="l")


########## n=3, mu=1, l=2
plot2_sigma=squared_exponential(x_values,x_values,2)
p2_norm1=mvrnorm(1, rep(1, 100), plot2_sigma)
p2_norm2=mvrnorm(1, rep(1, 100), plot2_sigma)
p2_norm3=mvrnorm(1, rep(1, 100), plot2_sigma)

plot(x_values,p2_norm1,xlab="x", ylab=" ",main="n=3, mu=1, l=2",type="l")
par(new=TRUE)
plot(x_values,p2_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p2_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")

########## n=5, mu=1, l=3
plot3_sigma=squared_exponential(x_values,x_values,3)
p3_norm1=mvrnorm(1, rep(1, 100), plot3_sigma)
p3_norm2=mvrnorm(1, rep(1, 100), plot3_sigma)
p3_norm3=mvrnorm(1, rep(1, 100), plot3_sigma)
p3_norm4=mvrnorm(1, rep(1, 100), plot3_sigma)
p3_norm5=mvrnorm(1, rep(1, 100), plot3_sigma)

plot(x_values,p3_norm1,xlab="x", ylab=" ",main="n=5, mu=1, l=3",type="l")
par(new=TRUE)
plot(x_values,p3_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p3_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p3_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p3_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")


########## n=1, mu=5, l=1
plot4_sigma=squared_exponential(x_values,x_values,1)
p4_norm=mvrnorm(1, rep(5, 100), plot4_sigma)
plot(x_values,p4_norm,xlab="x", ylab=" ",main="n=1, mu=5, l=1",type="l")

########## n=3, mu=5, l=2
plot5_sigma=squared_exponential(x_values,x_values,2)
p5_norm1=mvrnorm(1, rep(5, 100), plot5_sigma)
p5_norm2=mvrnorm(1, rep(5, 100), plot5_sigma)
p5_norm3=mvrnorm(1, rep(5, 100), plot5_sigma)

plot(x_values,p5_norm1,xlab="x", ylab=" ",main="n=3, mu=5, l=2",type="l")
par(new=TRUE)
plot(x_values,p5_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p5_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")

########## n=5, mu=5, l=3
plot6_sigma=squared_exponential(x_values,x_values,3)
p6_norm1=mvrnorm(1, rep(5, 100), plot6_sigma)
p6_norm2=mvrnorm(1, rep(5, 100), plot6_sigma)
p6_norm3=mvrnorm(1, rep(5, 100), plot6_sigma)
p6_norm4=mvrnorm(1, rep(5, 100), plot6_sigma)
p6_norm5=mvrnorm(1, rep(5, 100), plot6_sigma)

plot(x_values,p6_norm1,xlab="x", ylab=" ",main="n=5, mu=5, l=3",type="l")
par(new=TRUE)
plot(x_values,p6_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p6_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p6_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p6_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")

########## n=1, mu=10, l=1
plot7_sigma=squared_exponential(x_values,x_values,1)
p7_norm=mvrnorm(1, rep(10, 100), plot7_sigma)
plot(x_values,p7_norm,xlab="x", ylab=" ",main="n=1, mu=10, l=1",type="l")

########## n=3, mu=10, l=2
plot8_sigma=squared_exponential(x_values,x_values,2)
p8_norm1=mvrnorm(1, rep(10, 100), plot8_sigma)
p8_norm2=mvrnorm(1, rep(10, 100), plot8_sigma)
p8_norm3=mvrnorm(1, rep(10, 100), plot8_sigma)

plot(x_values,p8_norm1,xlab="x", ylab=" ",main="n=3, mu=10, l=2",type="l")
par(new=TRUE)
plot(x_values,p8_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p8_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")


########## n=5, mu=10, l=3

plot9_sigma=squared_exponential(x_values,x_values,3)
p9_norm1=mvrnorm(1, rep(10, 100), plot9_sigma)
p9_norm2=mvrnorm(1, rep(10, 100), plot9_sigma)
p9_norm3=mvrnorm(1, rep(10, 100), plot9_sigma)
p9_norm4=mvrnorm(1, rep(10, 100), plot9_sigma)
p9_norm5=mvrnorm(1, rep(10, 100), plot9_sigma)

plot(x_values,p9_norm1,xlab="x", ylab=" ",main="n=5, mu=10, l=3",type="l")
par(new=TRUE)
plot(x_values,p9_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p9_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p9_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p9_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")


################## Gamma Exponential Plots

###### n=1, mu=1, l=1, gamma=0.1 

plot10_sigma=  gamma_exp(x_values,x_values,1,0.1)
p10_norm=mvrnorm(1, rep(1, 100), plot10_sigma)
plot(x_values,p10_norm,xlab="x", ylab=" ",main="n=1, mu=1, l=1, gamma=0.1",type="l")

###### n=3, mu=1, l=2, gamma=1 
plot11_sigma=gamma_exp(x_values,x_values,2,1)
p11_norm1=mvrnorm(1, rep(5, 100), plot11_sigma)
p11_norm2=mvrnorm(1, rep(5, 100), plot11_sigma)
p11_norm3=mvrnorm(1, rep(5, 100), plot11_sigma)

plot(x_values,p11_norm1,xlab="x", ylab=" ",main="n=3, mu=5, l=2, gamma=1",type="l")
par(new=TRUE)
plot(x_values,p11_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p11_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")


###### n=5, mu=1, l=3, gamma=2
plot12_sigma=gamma_exp(x_values,x_values,3,2)
p12_norm1=mvrnorm(1, rep(1, 100), plot12_sigma)
p12_norm2=mvrnorm(1, rep(1, 100), plot12_sigma)
p12_norm3=mvrnorm(1, rep(1, 100), plot12_sigma)
p12_norm4=mvrnorm(1, rep(1, 100), plot12_sigma)
p12_norm5=mvrnorm(1, rep(1, 100), plot12_sigma)

plot(x_values,p12_norm1,xlab="x", ylab=" ",main="n=5, mu=1, l=3, gamma=2",type="l")
par(new=TRUE)
plot(x_values,p12_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p12_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p12_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p12_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")

###### n=1, mu=5, l=1, gamma=0.1 
plot13_sigma=  gamma_exp(x_values,x_values,1,0.1)
p13_norm=mvrnorm(1, rep(5, 100), plot13_sigma)
plot(x_values,p13_norm,xlab="x", ylab=" ",main="n=1, mu=5, l=1, gamma=0.1",type="l")

###### n=3, mu=5, l=2, gamma=1 
plot14_sigma=gamma_exp(x_values,x_values,2,1)
p14_norm1=mvrnorm(1, rep(10, 100), plot14_sigma)
p14_norm2=mvrnorm(1, rep(10, 100), plot14_sigma)
p14_norm3=mvrnorm(1, rep(10, 100), plot14_sigma)

plot(x_values,p14_norm1,xlab="x", ylab=" ",main="n=3, mu=10, l=2, gamma=1",type="l")
par(new=TRUE)
plot(x_values,p14_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p14_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")


###### n=5, mu=5, l=3, gamma=2
plot15_sigma=gamma_exp(x_values,x_values,3,2)
p15_norm1=mvrnorm(1, rep(5, 100), plot15_sigma)
p15_norm2=mvrnorm(1, rep(5, 100), plot15_sigma)
p15_norm3=mvrnorm(1, rep(5, 100), plot15_sigma)
p15_norm4=mvrnorm(1, rep(5, 100), plot15_sigma)
p15_norm5=mvrnorm(1, rep(5, 100), plot15_sigma)

plot(x_values,p15_norm1,xlab="x", ylab=" ",main="n=5, mu=5, l=3, gamma=2",type="l")
par(new=TRUE)
plot(x_values,p15_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p15_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p15_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p15_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")

###### n=1, mu=10, l=1, gamma=0.1 
plot16_sigma=  gamma_exp(x_values,x_values,1,0.1)
p16_norm=mvrnorm(1, rep(10, 100), plot16_sigma)
plot(x_values,p16_norm,xlab="x", ylab=" ",main="n=1, mu=10, l=1, gamma=0.1",type="l")


###### n=3, mu=10, l=2, gamma=1 
plot17_sigma=gamma_exp(x_values,x_values,2,1)
p17_norm1=mvrnorm(1, rep(5, 100), plot17_sigma)
p17_norm2=mvrnorm(1, rep(5, 100), plot17_sigma)
p17_norm3=mvrnorm(1, rep(5, 100), plot17_sigma)

plot(x_values,p17_norm1,xlab="x", ylab=" ",main="n=3, mu=5, l=2, gamma=1",type="l")
par(new=TRUE)
plot(x_values,p17_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p17_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")


###### n=5, mu=10, l=3, gamma=2
plot18_sigma=gamma_exp(x_values,x_values,3,2)
p18_norm1=mvrnorm(1, rep(10, 100), plot18_sigma)
p18_norm2=mvrnorm(1, rep(10, 100), plot18_sigma)
p18_norm3=mvrnorm(1, rep(10, 100), plot18_sigma)
p18_norm4=mvrnorm(1, rep(10, 100), plot18_sigma)
p18_norm5=mvrnorm(1, rep(10, 100), plot18_sigma)

plot(x_values,p18_norm1,xlab="x", ylab=" ",main="n=5, mu=10, l=3, gamma=2",type="l")
par(new=TRUE)
plot(x_values,p18_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p18_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p18_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p18_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")


########### Rational Quad Plots 



#### n=1, mu=1, l=1, alpha=0.1
plot20_sigma= rational_quad(x_values,x_values,1,0.1)
p20_norm=mvrnorm(1, rep(1, 100), plot20_sigma)
plot(x_values,p20_norm,xlab="x", ylab=" ",main="n=1, mu=1, l=1, alpha=0.1",type="l")


#### n=3, mu=1, l=1, alpha=1
plot21_sigma=rational_quad(x_values,x_values,2,1)
p21_norm1=mvrnorm(1, rep(1, 100), plot21_sigma)
p21_norm2=mvrnorm(1, rep(1, 100), plot21_sigma)
p21_norm3=mvrnorm(1, rep(1, 100), plot21_sigma)

plot(x_values,p21_norm1,xlab="x", ylab=" ",main="n=3, mu=1, l=2, alpha=1",type="l")
par(new=TRUE)
plot(x_values,p21_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p21_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")


#### n=5, mu=1, l=1, alpha=3
plot22_sigma=rational_quad(x_values,x_values,3,3)
p22_norm1=mvrnorm(1, rep(1, 100), plot22_sigma)
p22_norm2=mvrnorm(1, rep(1, 100), plot22_sigma)
p22_norm3=mvrnorm(1, rep(1, 100), plot22_sigma)
p22_norm4=mvrnorm(1, rep(1, 100), plot22_sigma)
p22_norm5=mvrnorm(1, rep(1, 100), plot22_sigma)

plot(x_values,p22_norm1,xlab="x", ylab=" ",main="n=5, mu=1, l=3, alpha=3",type="l")
par(new=TRUE)
plot(x_values,p22_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p22_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p22_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p22_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")


#### n=1, mu=5, l=1, alpha=0.1
plot23_sigma= rational_quad(x_values,x_values,1,0.1)
p23_norm=mvrnorm(1, rep(5, 100), plot23_sigma)
plot(x_values,p23_norm,xlab="x", ylab=" ",main="n=1, mu=5, l=1, alpha=0.1",type="l")


#### n=3, mu=5, l=1, alpha=1
plot24_sigma=rational_quad(x_values,x_values,2,1)
p24_norm1=mvrnorm(1, rep(5, 100), plot24_sigma)
p24_norm2=mvrnorm(1, rep(5, 100), plot24_sigma)
p24_norm3=mvrnorm(1, rep(5, 100), plot24_sigma)

plot(x_values,p24_norm1,xlab="x", ylab=" ",main="n=3, mu=5, l=2, alpha=1",type="l")
par(new=TRUE)
plot(x_values,p24_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p24_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")

#### n=5, mu=5, l=1, alpha=3
plot25_sigma=rational_quad(x_values,x_values,3,3)
p25_norm1=mvrnorm(1, rep(5, 100), plot25_sigma)
p25_norm2=mvrnorm(1, rep(5, 100), plot25_sigma)
p25_norm3=mvrnorm(1, rep(5, 100), plot25_sigma)
p25_norm4=mvrnorm(1, rep(5, 100), plot25_sigma)
p25_norm5=mvrnorm(1, rep(5, 100), plot25_sigma)

plot(x_values,p25_norm1,xlab="x", ylab=" ",main="n=5, mu=5, l=3, alpha=3",type="l")
par(new=TRUE)
plot(x_values,p25_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p25_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p25_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p25_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")


#### n=1, mu=10, l=1, alpha=0.1
plot26_sigma= rational_quad(x_values,x_values,1,0.1)
p26_norm=mvrnorm(1, rep(10, 100), plot20_sigma)
plot(x_values,p26_norm,xlab="x", ylab=" ",main="n=1, mu=10, l=1, alpha=0.1",type="l")


#### n=3, mu=10, l=1, alpha=1
plot27_sigma=rational_quad(x_values,x_values,2,1)
p27_norm1=mvrnorm(1, rep(10, 100), plot27_sigma)
p27_norm2=mvrnorm(1, rep(10, 100), plot27_sigma)
p27_norm3=mvrnorm(1, rep(10, 100), plot27_sigma)

plot(x_values,p27_norm1,xlab="x", ylab=" ",main="n=3, mu=10, l=2, alpha=3",type="l")
par(new=TRUE)
plot(x_values,p27_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p27_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")


#### n=5, mu=10, l=1, alpha=3
plot28_sigma=rational_quad(x_values,x_values,3,3)
p28_norm1=mvrnorm(1, rep(10, 100), plot28_sigma)
p28_norm2=mvrnorm(1, rep(10, 100), plot28_sigma)
p28_norm3=mvrnorm(1, rep(10, 100), plot28_sigma)
p28_norm4=mvrnorm(1, rep(10, 100), plot28_sigma)
p28_norm5=mvrnorm(1, rep(10, 100), plot28_sigma)

plot(x_values,p28_norm1,xlab="x", ylab=" ",main="n=5, mu=10, l=3, alpha=3",type="l")
par(new=TRUE)
plot(x_values,p28_norm2,col="blue",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p28_norm3,col="red",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p28_norm4,col="green",axes=FALSE,type="l",ylab=" ",xlab=" ")
par(new=TRUE)
plot(x_values,p28_norm5,col="purple",axes=FALSE,type="l",ylab=" ",xlab=" ")


