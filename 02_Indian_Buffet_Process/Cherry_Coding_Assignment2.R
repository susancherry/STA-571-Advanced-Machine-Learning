########## Coding Assignment 2 #############
############## Susan Cherry ################
########## Indian Buffet Process ###########

install.packages('plotrix')

IBP= function(N, alpha){
  ## Function for the Indian Buffet Process ##
  
  #### first customer
  num_first=rpois(1,alpha)
  #intialize grid
  customer_grid=matrix(1,nrow=1,ncol=num_first)
  rest_tally=rep(1,num_first)
  
  #### remaining customers
  for (i in 2:N){
    new_cust=rep(0,length(rest_tally))
  
    
    for (j in 1:length(rest_tally)){
      #prob of trying an already sampled dish
      dish_prob=rest_tally[j]/i
      
      #update if they choose to sample
      if (runif(1)<dish_prob){
        rest_tally[j]=rest_tally[j]+1
        new_cust[j]=1
      }
    }
      #choose how many new dishes they try 
      num_new=rpois(1,alpha/i)
      if (num_new>0){
        #add new dishes to rest_tally and the customers list 
        rest_tally=append(rest_tally, rep(1,num_new))  
        new_cust=append(new_cust, rep(1,num_new)) 
        #append 0s to prev customers for those dishes 
        new_matrix=matrix(0,dim(customer_grid)[1],ncol=num_new)
        customer_grid=cbind(customer_grid,new_matrix)
        
      }
    #add newest customer to cust_grid  
    customer_grid=rbind(customer_grid,new_cust)
  }
  
  #return the grid of which customers sampled which dishes
  return(customer_grid)
}

test=IBP(10,10)
plotrix::color2D.matplot(test*-1)
