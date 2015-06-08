#This function demonstrates how taking different sample sizes can impact the a sample's variance / mean
MeanChickWeight <- function(){
  
  #Create a vector containing the weights of chicks over a given period of time
  weights <- ChickWeight$weight
  
  #Create empty sets for our different sample sizes
  sample_mean50 <- rep(NA, 5000)
  sample_mean100 <- rep(NA, 5000)
  sample_mean150 <- rep(NA, 5000)
  
  #Take the average of 5000 samples based on the given sample size
  for (i in 1:5000){
    
    sample_mean50[i] <- mean(sample(weights,50))
    sample_mean100[i] <- mean(sample(weights,100))
    sample_mean150[i] <- mean(sample(weights,150))
    
  }
  
  #Plot three histograms to show how the three sample sizes differ
  hist(sample_mean50)
  hist(sample_mean100)
  hist(sample_mean150)
}