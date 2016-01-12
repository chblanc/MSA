## 
## Target Shuffle Function 
##

# libraries
library(MASS)
library(ggplot2)

##
## Target shuffle function will automatically shuffle the values of the target
## variable to get a sense of 'how good' the original model is. This will be 
## achieved by shuffling the values of the target variable while leaving everything
## else in the same position. A model will be run and the result recorded. The 
## process will be repeated n times and the end product will give the user
## a sense of how good the model is (compared to random)
##

## For future rounds, consider adding the following functionality:
##    1) Add more metrics (currently only supports adjusted r-squared)
##    2) Allow functionality for logistic regression models
##


targetShuffle <- function(df, n, graph = FALSE) {
  
  
    output <- list()
    
    y <- select.list(sort(colnames(df)), title = 'Select Target Variable:')
    xnames <- names(df[,-grep(y, names(df))])
    fmla <- as.formula(paste(y,  "~ ", paste(xnames, collapse= "+")))
    truth <- summary(lm(fmla, data = df))$adj.r.squared
    
  
    
    temp <- unlist(lapply(seq_len(n), function(i) {
      
      return(summary(lm(fmla, data = df))$adj.r.squared)
     
    }))
  
    output$adj.r.squared <- temp
    output$percentiles <- quantile(temp, probs = c(.05,.25,.5,.75,.95))
    
    output$true.value <- truth

   p <-  hist(c(temp, truth), main = paste('Adjusted R-Squared Over', n , 'Iterations'),
         xlab = 'Adjusted R-Squared', breaks = 50, col = 'grey')
    abline(v = truth, col = 'red', lwd = 2)
    mtext("Initial Value", at=truth, col="red")
  
    
    if(graph == TRUE) {
   
       plot(p)
  
    }
    
    output$plot <- p 
    cat(paste0('The original model has an adjusted r-squared of: ', round(truth,4)), '\n')
    cat('Percentile distribution of adjusted R-squared: ', '\n')
    print(output$percentiles)
    
    return(output)  

}

# Test
test <- targetShuffle(Boston, 100)
test2 <- targetShuffle(mtcars, 250)
test3 <- targetShuffle(Insurance, 500)
