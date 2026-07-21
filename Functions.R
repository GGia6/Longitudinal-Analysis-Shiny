library(dplyr)
library(tidyr)
library(tidyverse)
library(brms)
library(emmeans)


##recode 98 and 99's for stat analysis 
clean_codes<- function(data, exclude = c(),
                       missing_code= c(98,99)){
  questions<- setdiff(names(data), exclude)
  
  for (col in questions){
    if(is.numeric(data[[col]])){
      data[[col]][data[[col]] %in% missing_code]<- NA
    }
  }
  return(data)
}

##Detect variable type function

detect_var<- function(x){
  
  x_clean<- x[!is.na(x)]
  
  if(length(x_clean)==0){
    return("empty")
  }
  n_unique<- length(unique(x_clean))
  
  if(n_unique==2){
    return("binary var")
  }
  
  if(n_unique >2 & n_unique <=10){
    return("categorical var")
  }
  
  return("numeric var")
}

## Testing the function
detect_var(c(1, 0, 1, 1, NA))              # "binary var"
detect_var(c(1, 2, 3, 2, 1, NA))           # "categorical var"
detect_var(c(23, 45, 67, 12, 89, 34, 56, 78, 90, 11, 22))  # "numeric var"
detect_var(c(NA, NA, NA)) #empty 


bayes_binaryvar<- function(data,
                           wave_var, outcome_var,
                           waveA, WaveB,
                           success_code =1,
                           priorA = 1,
                           priorB =1, 
                           draws = 10000){
  levels<- sort(unique(na.omit(data[[outcome_var]])))
  
  if(length(levels)!= 2){
    stop("Outcome variable must have exactly 2 categories.")
  }
  
  data<- data %>% 
    filter(.data[[wave_var]] %in% c(waveA, WaveB))
  
  wave_counts<- data %>%
    group_by(.data[[wave_var]])%>%
    summarise(
      success = sum(.data[[outcome_var]]== success_code, na.rm = TRUE),
      failure = sum(.data[[outcome_var]]!= success_code, na.rm =TRUE),
      n = success + failure,
      .groups = "drop"
    ) %>%
    mutate(
      alpha = priorA + success,
      beta = priorB + failure,
      post_mean = alpha/ (alpha + beta),
      lower_ci = qbeta(.025, alpha, beta),
      upper_ci = qbeta(.975, alpha, beta)
    )
  draw1<- rbeta(draws, wave_counts$alpha[1],
                wave_counts$beta[1])
  draw2<- rbeta(draws, wave_counts$alpha[2],
                wave_counts$beta[2])
  
  Probability_greater<- mean(draw2 > draw1)
  
  difference<- draw2 - draw1
  
  list(summary = wave_counts,
       Probability_greater= Probability_greater,
       mean_difference = mean(difference),
       lower_diff = quantile(difference, 0.025),
       upper_diff = quantile(difference, 0.975))
  
}





