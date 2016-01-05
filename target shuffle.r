## 
## Target Shuffle Function 
##

# libraries
library(dplyr)
library(ggplot2)
library(MASS)
data(Boston)


# Add a factor and character variable to data set in order to 
# find a proper way to deal with those types of variables

Boston$character <- rep(c('A','B'))
Boston$factor <- as.factor(rep(c('B','C')))

targetShuffle <- function(df, num.iters) {
  
  
    output <- list()
    
    y <- select.list(sort(colnames(df)), title = 'Select Target Variable:')

    xnames <- names(df[,-grep(y, names(df))])
    
    fmla <- as.formula(paste(y,  "~ ", paste(xnames, collapse= "+")))
    
    truth <- summary(lm(fmla, data = df))$adj.r.squared
    
  
    
    temp <- unlist(lapply(seq_len(num.iters), function(i) {
      
      newOrder <- sample(nrow(df))
      newY <- as.matrix(df[newOrder,y])
      df2 <- df
      df2[,y] <-df[newOrder,y]
      
      
      #return(summary(lm(newY ~ X))$adj.r.squared)
      return(summary(lm(fmla, data = df2))$adj.r.squared)
     
    }))
  
    output$adj.r.squared <- temp
    output$percentiles <- quantile(temp, probs = c(.05,.25,.5,.75,.95))
    
    output$true.value <- truth
    
    p <- ggplot(data = as.data.frame(x = temp), aes(temp)) +
      geom_histogram(color = 'white', alpha = .75) +
      xlab('Adjusted R-Squared') +
      ylab('Frequency') +
      ggtitle(paste('Adjusted R-Squared Over', num.iters, 'Iterations')) +
      theme_bw()
      #geom_vline(x = output$truth, color = 'red') +
      #geom_text(aes(x= output$truth, y = 10, label=paste('True Model')),
                #colour="red", size = 3, hjust = 1.25)
    
    output$p <- p
    
    return(output)  
  
    
}

# Test
test <- targetShuffle(Boston, 10)

