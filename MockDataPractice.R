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


## Viewing and testing predictive probability plots 

library(VGAM)

Afghan<- read.csv("AfghanFutures.csv")

testerr<-glm_analysis(data = Afghan, outcome = "CndPower")

test2<-ordinal_analysis(data = Afghan, outcome = "CndPower")

Afghan$Wave<- as.factor(Afghan$Wave)

# 1. Create a dummy dataset containing all your unique waves
# (Replace "Wave" with your actual wave column name if it is different)
new_data <- data.frame(Wave = sort(unique(Afghan$Wave)))

# 2. Get the predicted probabilities matrix
probs <- predict(testerr$result, newdata = new_data, type = "response")

# 3. Plot all 4 categories at once using standard R graphics
matplot(probs, type = "b", pch = 16, lty = 1, lwd = 2,
        xaxt = "n", xlab = "Waves", ylab = "Probability",
        main = "Predicted Probabilities Across Waves")

# 4. Add the wave labels to the bottom axis
axis(1, at = 1:nrow(new_data), labels = new_data$Wave, las = 2)

# 5. Add a simple legend
legend("top", legend = paste("Category", 1:4), col = 1:4, lty = 1, pch = 16)

##Testing glm pplot func 
glm_pred_plot(model = testerr$result, data = Afghan)


result <- glm_analysis(data = Afghan, outcome = "CndPower")
str(result, max.level = 1)
names(result)


library(haven)

afghan<- read_sav("AfghanFutures.sav")


afghan$LiveDC %>% attr('label')


numerics<- read_sav("dummy_survey_merged_waves.sav")

library(tidyverse)

numerics$Wave<- as_factor(numerics$Wave)

mdoel<- aov(log(AnnualIncome) ~ Wave, data = numerics)

summary(mdoel)

library(rstatix)

numerics %>% anova_test(AnnualIncome ~ Wave)

numerics %>% anova_test(Age ~ Wave)


plot(mdoel)

numerics2<- read_sav("dummy_survey_waves_anova_pass.sav")

numerics2$Wave<- as_factor(numerics2$Wave)


model3<-numerics2 %>% anova_test(AnnualIncome ~ Wave)

plot(model3)

model4<- aov(log(AnnualIncome) ~ Wave, data = numerics2)

summary(model4)

hist(model4$residuals)

shapiro.test(modelsquare$residuals)

plot(model4)


boxplot((AnnualIncome^2)~ Wave, data = numerics2)

library(car)
leveneTest(log(AnnualIncome)~ Wave, data = numerics2)


modelsquare<- aov((AnnualIncome^2) ~ Wave, data = numerics2)

summary(modelsquare)


leveneTest(TrustScore~ Wave, data = numerics2)

newmod<-oneway.test(log(AnnualIncome)~ Wave, data = numerics2, var.equal = FALSE)
summary(newmod)
pairwise.wilcox.test(numerics2$AnnualIncome, numerics2$Wave, p.adjust.method = "BH")

kruskal.test(AnnualIncome ~ Wave, data = numerics2)

kruskal.test(TrustScore ~ Wave, data = numerics2)

boxplot(TrustScore~ Wave, data = numerics2)

pairwise.wilcox.test(numerics2$TrustScore, numerics2$Wave, p.adjust.method = "BH")


numeric_analysis<- function(data, outcome, wave = "Wave"){
  
  data[[wave]] <- haven::as_factor(data[[wave]])
  
  num_mod<- kruskal.test(outcome ~ wave, data = data)
  
  wilcox<- 
  
}


