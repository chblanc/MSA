## 
## Target Shuffle Function 
##

# libraries
library(MASS)
library(pscl)
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
  
  require(rms)
  
    output <- list()
    
    y <- select.list(sort(colnames(df)), title = 'Select Target Variable:')
    xnames <- names(df[,-grep(y, names(df))])
    fmla <<- as.formula(paste(y,  "~ ", paste(xnames, collapse= "+")))
    
    
    if(length(unique(df[,y])) > 2) {
      
      metric <- 'Adjusted R-Squared'
      cat('Note: Assuming continuous response', '\n')
      
    model <- lm(fmla, data = df)
    truth <- summary(lm(fmla, data = df))$adj.r.squared
    temp <- unlist(lapply(seq_len(n), function(i) {
    df[,y] <- df[sample(nrow(df)),y]
      
      return(summary(lm(fmla, data = df))$adj.r.squared)
     
      })) 
    
    }
    
    
    if(length(unique(df[,y])) == 2) {

      
      cat('Note: Assuming binary response', '\n')
  
      model <<- lrm(formula = fmla, data = df)
      print('madeit 1')
      
      truth <- model$stats
      metric <- select.list(sort(names((truth))), title = 'Select Model Metric:')
      truth <- truth[[metric]]
      
      temp <- unlist(lapply(seq_len(n), function(i) {
      df[,y] <- df[sample(nrow(df)),y]
      
      return(lrm(fmla, data = df)$stats[[metric]])
      
      })) 
      
      #temp <- do.call('rbind', temp)
      
    }
  
    output$original.model <- model
    output$shuffled.output <- temp
    output$percentiles <- quantile(temp, probs = c(.05,.25,.5,.75,.95))
    output$true.value <- truth

   p <-  hist(c(temp, truth), main = paste(metric, 'Over', n , 'Iterations'),
         xlab = metric, breaks = 50, col = 'grey')
    abline(v = truth, col = 'red', lwd = 2)
    mtext("Initial Value", at=truth, col="red")
  
    
    if(graph == TRUE) {
   
       plot(p)
  
    }
    
    output$plot <- p 
    cat(paste0('The original model has a ', metric, ' of: ', round(truth,4)), '\n')
    cat(paste0('Percentile distribution of ', metric, ' across ', n, ' interations',':', '\n'))
    print(output$percentiles)
    
    return(output)  

}

# Test
test <- targetShuffle(Boston, 100)
test2 <- targetShuffle(mtcars, 250)
test3 <- targetShuffle(Insurance, 500)
