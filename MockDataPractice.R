#Example: Creating a mock wide dataset
library(tidyr)
library(dplyr)
library(tidyverse)

wide_data <- tibble(
  id = 1:3,
  income = c(50000, 60000, 55000),
  score_wave1 = c(10, 15, 12),
  score_wave2 = c(12, 18, 11),
  score_wave3 = c(15, 22, 14)
)

# Convert Wide to Long format
long_data <- wide_data %>%
  pivot_longer(
    cols = starts_with("score_wave"),
    names_to = "wave",
    names_prefix = "score_wave",
    values_to = "score"
  ) %>%
  mutate(wave = as.numeric(wave)) # Convert wave to a numeric time variable

print(long_data)
library(lme4)
library(lmerTest)
#Fit model: random intercept for each participant
model <- lmer(score ~ wave + income + (1 | id), data = long_data)

# View detailed summary (including coefficients, standard errors, and p-values)
summary(model)
library(SurveyAssist)

Thailand<-read.csv("Southeast Asia Merge Snippet.csv")
attributes(Thailand$Country_M)


Thailand2<- Thailand %>%
  select(Wave, WatchCountryNews, PplPres, RiteRong , Mode,
         Country_M, IDNO)
Thailand2$RiteRong<- as.factor(Thailand2$RiteRong)
Thailand2$PplPres<- as.factor(Thailand2$PplPres)
Thailand2$Wave<- as.factor(Thailand2$Wave)

source("Functions.R")

binarys<- detect_var(name)
binary_vars<- names(Thailand)[sapply(Thailand, function(x){
  detect_var(x)== "binary var"
})]

cat_vars<- names(Thailand)[sapply(Thailand, function(x){
  detect_var(x)== "categorical var"
})]

num_vars<- names(Thailand)[sapply(Thailand, function(x){
  detect_var(x)== "numeric var"
})]


thailand_cat<- Thailand %>%
 dplyr::select(names(Thailand)[names(Thailand) %in% cat_vars] )
  
thailand_cat$Wave<- as.factor(thailand_cat$Wave)
thailand_cat$RiteRong<- as.factor(thailand_cat$RiteRong)

levels(as.factor(thailand_cat$RiteRong))
library(MASS)

model <- polr(RiteRong ~ Wave, data = thailand_cat, Hess = TRUE)

# View the default summary
summary(model)

exp(cbind(OR = coef(model), confint(model)))
exp(coef(model))
confint(model)

Anova.clm

library(car)
vif(model)

library(ordinal)

library(ordinalbayes)

thailand_cat$RiteRong<- factor(thailand_cat$RiteRong, 
                               levels = c(1, 2, 3, 98, 99),
                               ordered = TRUE)

library(rstanarm)
fitbayes<- stan_polr(formula = RiteRong ~ Wave, data = thailand_cat, 
               method = "logistic", prior = R2(0.3, "mean") ,
               chains = 2, cores= 2, iter = 500)
print(fitbayes)



thailand_cat$FavNatGov<- factor(
  thailand_cat$FavNatGov,
  levels = c(1, 2, 3),  # only the substantive levels
  ordered = TRUE
)
# anything coded 98 or 99 becomes NA automatically here

model <- polr(RiteRong_clean ~ Wave, data = thailand_cat, Hess = TRUE)
summary(model)

model3<-ordinal_analysis(data=thailand_cat, outcome = "RiteRong_clean")
summary(model3)


coef_table<-summary(model2)$coefficients
exp(coef_table[, "Estimate"])

##Test to see if proportional odds assumption is violated. 




##Test is violated when missingness is included such as codes 98 and 99, when removed from thedata brant test and prop odds assumptions satisfied
library(gofcat)
result<-brant.test(model2)
model2<- clm(FavNatGov ~ Wave, data = thailand_cat)
summary(model2)

brant_func(data = thailand_cat, outcome = "FavNatGov")

thailand_cat$FavNatGov<- as.factor(thailand_cat$FavNatGov)

model2_partial <- clm(RiteRong_clean ~ 1, 
                      nominal = ~ Wave, 
                      data = thailand_cat)

summary(model2_partial)

brant.test(model2_partial)
anova(model2, model2_partial)

thailand_cat$fitted <- fitted(model2)
plot(fitted ~ Wave, data = thailand_cat)

library(ggplot2)
ggplot(thailand_cat, aes(x = Wave, y = RiteR)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


thailand_cat %>%
  mutate(bin = ntile(RiteRong_clean, 10)) %>%
  group_by(bin) %>%
  summarize(mean_x = mean(Wave), p = mean(RiteRong_clean), logit = log(p/(1-p))) %>%
  plot(logit ~ mean_x, data = .)




afghan<- read.csv("AfghanFutures.csv")

afghan$Wave<- as.factor(afghan$Wave)

afghan$CndPower<- factor(
  afghan$CndPower,
  levels = c(1, 2, 3, 4),  # only the substantive levels
  ordered = TRUE
)

afghanmodel<- MASS::polr(CndPower ~ Wave, data = afghan, Hess = TRUE)
summary(afghanmodel)

brant.test( model_full_po)




##FInding another test for when assumption is violated 
partial<-VGAM::vglm(CndPower ~ Wave, family = cumulative(parallel = FALSE ~ Wave), data = afghan)
summary(partial)

model_full_po <- VGAM::vglm(
  CndPower ~ Wave,
  family = cumulative(parallel = TRUE, reverse = TRUE),
  data = afghan
)
summary(model_full_po)
VGAM::lrtest(partial, model_full_po)

glm_analysis(data = afghan, outcome = "CndPower")

marginal_effects(afghanmodel, categorical = TRUE)

anova()
plot(pred_probs) + 
 labs(title = "Predictive Probability Plot", 
      x = "Wave", 
      y = "Predicted Probability") +
  theme_minimal()
thailand_cat$EmailInt<- factor(
  thailand_cat$EmailInt,
  levels = c(1, 2, 3, 4, 5),  # only the substantive levels
  ordered = TRUE
)


model_null <- clm(FavNatGov ~ 1, data = thailand_cat)
model_wave <- clm(EmailInt ~ Wave, data = thailand_cat)
anova(model_null, model_wave)

pred_probs <- ggpredict(part, terms = "Wave")
