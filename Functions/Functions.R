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

## categorical variables function

library(ordinal)
library(gofcat)
ordinal_analysis <- function(data,
                             outcome,
                             wave = "Wave",
                             missing_codes = c(98, 99)) {
  data[[outcome]][data[[outcome]] %in% missing_codes] <- NA
  
  data[[outcome]] <- factor(
    haven::as_factor(data[[outcome]]),
    ordered = TRUE
  )
  
  data[[wave]] <- factor(data[[wave]])
  
  model <- ordinal::clm(
    reformulate(wave, response = outcome),
    data = data
  )
  
  coef_table <- as.data.frame(coef(summary(model)))
  coef_table <- coef_table[!grepl("\\|", rownames(coef_table)), ]
  ##Put da results in a datatable in the shiny 
  results_table <- data.frame(
    Predictor = rownames(coef_table),
    Estimate = round( coef_table[, "Estimate"],4),
    Odds_Ratio = round( exp(coef_table[, "Estimate"]),4),
    Std_Error = round( coef_table[, "Std. Error"],4),
    z_value =round(  coef_table[, "z value"],4),
    p_value =  round(coef_table[, "Pr(>|z|)"],6),
    row.names = NULL
  )
  
  list(
    model = model,
    results = results_table
  )
}
## Proportional odds assumption check function for ordinal logistic regression of cat vars. 
library(gofcat)
brant_func<- function(data,
                      outcome,
                      wave = "Wave",
                      missing_codes = c(98, 99)) {
  data[[outcome]][data[[outcome]] %in% missing_codes] <- NA
  
  data[[outcome]] <- factor(data[[outcome]], ordered = TRUE)
  
  data[[wave]] <- factor(data[[wave]])
  
  
  model <- ordinal::clm(
    reformulate(wave, response = outcome),
    data = data
  )
  model_null<- ordinal::clm(
    reformulate("1", response = outcome),
    data = data
  )
  
  
  brant_results<- gofcat::brant.test(model)
  
  brant_data <- data.frame(
    term    = c("Omnibus", brant_results$vnames),
    chi_sq  = brant_results$chisq,
    df      = brant_results$df,
    p_value = pchisq(brant_results$chisq, brant_results$df, lower.tail = FALSE)
  )
  
  if (any(brant_data$p_value <= 0.05, na.rm = TRUE)) {
    list(
      Error  = "Proportional odds assumption was not met for one or more terms, can't determine Wave difference.
      Move to Generalized categorical analysis.",
      brant_data = brant_data
    )
  } else {
    list(
      Error = "All assumptions met, interpretations can be drawn from the data." ,
      brant_data = brant_data
    )
  }
}


glm_analysis<- function(data,
                        outcome,
                        wave = "Wave",
                        missing_codes = c(98, 99)) {
  data[[outcome]][data[[outcome]] %in% missing_codes] <- NA
  
  data[[outcome]] <- factor(data[[outcome]], ordered = TRUE)
  
  data[[wave]] <- factor(data[[wave]])
  
  
  model <- VGAM::vglm(
    reformulate(wave, response = outcome),
    data = data,
    family = cumulative(parallel = FALSE, reverse = TRUE)
  )
  result<-summary(model)
  
  coefs<- result@coef3
  
  vglm_table<- data.frame( 
    Predictor = rownames(coefs),
    Estimate = round( coefs[, "Estimate"],4),
    Odds_Ratio = round( exp(coefs[, "Estimate"]),4),
    Std_Error = round( coefs[, "Std. Error"],4),
    z_value =round(  coefs[, "z value"],4),
    p_value =  round(coefs[, "Pr(>|z|)"],6),
    row.names = NULL
    
  )
  
  ##check assumptions within function so if std errors blow up quasi-seperation violated
  
  if (any(vglm_table$Std_Error > 3, na.rm = TRUE)) {
    list(
      model = model,
      Error  = "Data reflects extreme uncertainty, Wave differences cannot be interpreted or concluded.",
      vglm_table = vglm_table
    )
  } else {
    list(
      model = model,
      Error = "All assumptions met, interpretations can be drawn from the data." ,
      vglm_table = vglm_table
    )}
  }


#MAking prediction plot function for ordinal analysis 

# OA plot funcy
oa_pred_plot <- function(model, data, wave_var = "Wave") {
  new_data <- data.frame(Wave = factor(sort(unique(data[[wave_var]])), levels = levels(as.factor(data[[wave_var]]))))
  names(new_data) <- wave_var
  
  probs <- predict(model, newdata = new_data, type = "prob")
  
  plot_df <- as.data.frame(probs)
  plot_df$Wave <- new_data[[wave_var]]
  
  plot_df_long <- tidyr::pivot_longer(
    plot_df, cols = -Wave,
    names_to = "Response_Category", values_to = "Probability"
  )
  
  ggplot(plot_df_long, aes(x = Wave, y = Probability,
                           color = Response_Category, group = Response_Category)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    labs(title = "Predicted Probabilities Across Waves",
         x = "Wave", y = "Predicted Probability", color = "Response Category") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#GLM plot func

glm_pred_plot <- function(model, data, wave_var = "Wave") {
  new_data <- data.frame(Wave = factor(sort(unique(data[[wave_var]])), levels = levels(as.factor(data[[wave_var]]))))
  names(new_data) <- wave_var
  
  probs <- predict(model, newdata = new_data, type = "response")
  
  plot_df <- as.data.frame(probs)
  plot_df$Wave <- new_data[[wave_var]]
  
  plot_df_long <- tidyr::pivot_longer(
    plot_df, cols = -Wave,
    names_to = "Response_Category", values_to = "Probability"
  )
  
  ggplot(plot_df_long, aes(x = Wave, y = Probability,
                           color = Response_Category, group = Response_Category)) +
    geom_line(linewidth = 1) +
    geom_point(shape =17 ,size = 2.5) +
    labs(title = "Predicted Probabilities Across Waves",
         x = "Wave", y = "Predicted Probability", color = "Response Category") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}










