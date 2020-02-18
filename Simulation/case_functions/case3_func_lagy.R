# Function for case 3. Please read the walk through for an explanation of how this works.
library(tidyverse)
library(nlme)
library(MASS)

sims_function_case3_lagy <- function(N, lag_amount){
  
  # 1 #
  
  N <- N
  lag_amount <- lag_amount

  autor_x <- 0.22
  autor_y <- 0.22
  autor_z <- 0.22
  
  x_cause_y <- 0.082
  y_cause_x <- 0.082
  z_cause <- 0.082
  
  time <- 200
  
  # 2 #
  
  sims <- 500
  p_wo_partial <- numeric(sims)
  se_wo_partial <- numeric(sims)
  b1_wo_partial <- numeric(sims)
  p_partial <- numeric(sims)
  se_partial <- numeric(sims)
  b1_partial <- numeric(sims)
  
  
  # 3 #
  for(q in 1:sims){
    
    df <-  matrix(, ncol = 5, nrow = N*time)
    
    # 4 $
    
    count <- 0 
    
    for(i in 1:N){ 
      y_het <- rnorm(1,0,1) 
      
      for(j in 1:time){ 
        count <- count + 1
        
        if(j == 1 | j == 2 | j == 3 | j == 4 | j == 5){
          df[count,1] <- j
          df[count,2] <- i
          df[count,3] <- rnorm(1,0,1)
          df[count,4] <- rnorm(1,0,1)
          df[count,5] <- rnorm(1,0,1)
          
          
          
          
        }else{ 
          
          df[count,1] = j
          df[count,2] = i
          # generate z first because it is causal
          df[count,5] = autor_z * df[(count - 1),5] + rnorm(1,0,1)
          
          df[count,3] = autor_x * df[(count - 1),3] + z_cause * df[count,5] + rnorm(1,0,1)
          df[count,4] = autor_y * df[(count - 1),4] + z_cause * df[(count - lag_amount),5] + y_het + rnorm(1,0,1) # lag parameter here

          
        }
        
        
      }
    }
    
    # 5 #
    
    df <- data.frame(df[, c(1:4)])
    names(df) <- c('day','id','action','affect')
    
    df <- df %>%
      mutate(lag_affect = lag(affect)) %>%
      filter(day > 180)
    
    person_mean_df <- df %>%
      group_by(id) %>%
      summarise(
        action_person_mean = mean(action)
      )
    
    df <- left_join(df, person_mean_df)
    
    df$action_person_center <- df$action - df$action_person_mean
    
    
    # 6 #
    
    model_wo_partial <- lme(fixed = affect ~ action_person_center,
                            random = ~1|id,
                            data = df,
                            control = lmeControl(opt = 'optim'))
    
    
    model_partial <- lme(fixed = affect ~ action_person_center + lag_affect,
                         random = ~1|id,
                         data = df,
                         control = lmeControl(opt = 'optim'))
    
    # Without partial model results #
    # p-value for b1 #
    p_wo_partial[q] <- summary(model_wo_partial)$tTable[, "p-value"][2]
    # standard error for b1 #
    se_wo_partial[q] <- summary(model_wo_partial)$tTable[, "Std.Error"][2]
    # b1 itself #
    b1_wo_partial[q] <- summary(model_wo_partial)$tTable[, "Value"][2]
    
    # With partial model results #
    # p-value for b1 #
    p_partial[q] <- summary(model_partial)$tTable[, "p-value"][2]
    # standard error for b1 #
    se_partial[q] <- summary(model_partial)$tTable[, "Std.Error"][2]
    # b1 itself #
    b1_partial[q] <- summary(model_partial)$tTable[, "Value"][2]
    
    
  } # end the 500 sims loop here
  
  
  results <- list(
    'p_wo_partial' = p_wo_partial,
    'se_wo_partial' = se_wo_partial,
    'b1_wo_partial' = b1_wo_partial,
    'p_partial' = p_partial,
    'se_partial' = se_partial,
    'b1_partial' = b1_partial,
    'sample_size' = rep(N, sims),
    'lag_amount' = rep(lag_amount, sims),
    'case' = rep('case3_lagy', sims)
  )
  
  return(results)
  
}

