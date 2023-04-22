# Install libraries with install.packages("package.name") if required
library(tidyverse) # for data wrangling and visualization
library(ggpubr) # theme_pubr()
library(ggmosaic) # ggmosaic()
library(broom) # for tidy model output
library(forestmangr) # for convenient rounding
library(pROC) # rocobj() and ggroc()

# Load the data
data <- read.csv("ecommerce.csv")

# Inspect the data.
head(data)
summary(data)

# Convert the character variables to factor variables
data <- data %>% 
  mutate(discount = factor(discount),
         source = factor(source, levels = c("ads", "search", "direct")),
         country = factor(country, levels = c("france", "germany", "ireland", "uk")),
         conversion = factor(conversion))

# Logistic regression model m1
m1 <- glm(conversion ~ discount, data = data, family = binomial)

# Inspect model m1 output
summary(m1)

# The odds ratio for discountyes
exp(1.10090)
exp(coef(m1))

# The 95% confidence interval for the odds ratio for discountyes
exp(confint(m1))

# Mosaic plots
ggplot(data = data) + # start a ggplot
  geom_mosaic(aes( # use a mosaic geom
    x = product(discount),  # put 'promo' on x-axis
    fill = conversion), # use `pass` as fill color
    offset = 0.02, # set the space in-between 
    divider = ddecker()) + # double decker plot
  facet_grid(~source,# forms a matrix of panels by 'channel'
             scales = "free") + # let both facet height and width vary
  theme_pubr() + # use this theme
  theme(axis.text.y = element_blank(), # clustomize the theme
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  labs(x = "", y = "") # don't use axis labels

# Logistic regression model m2
m2 <- glm(conversion ~ discount + source, data = data, family = binomial)

# Inspect model m2 output
summary(m2)

# Coefficients table of m2
tidy(m2) %>% 
  mutate(exp.beta = exp(estimate)) %>% # calculate the exp(beta)
  round_df(digits = 7, rf = "round") %>% 
  rename(beta = estimate,
         se = std.error,
         z.value = statistic)

# Odds ratios
exp(coef(m2))

# Logistic regression model m3
m3 <- glm(conversion ~ discount + source + discount:source, data = data, family = binomial)

# Inspect model m3 output
summary(m3)

# The 95% confidence interval for the odds ratio
exp(confint(m3))

# Logistic regression model m4
m4 <- glm(conversion ~ country + source + total_pages_visited + visit_duration + discount
           + discount:source, data = data, family = binomial)

# Inspect model m4 output
summary(m4)

# Correlation between the two numerical variables
cor(data$total_pages_visited, data$visit_duration)

# Logistic regression model m5
m5 <- glm(conversion ~ country + source + total_pages_visited + discount
          + discount:source, data = data, family = binomial)

# Inspect model m5 output
summary(m5)

# Visualize the odds ratios
tidy(m5) %>% # tidy function from broom package
  mutate(exp_beta_llci = exp(confint(m5))[, 1], # lower ci
         exp_beta = exp(estimate), # odds ratio, midpoint
         exp_beta_ulci = exp(confint(m5))[, 2]) %>% # upper 
  select(term, estimate, exp_beta_llci, exp_beta, exp_beta_ulci) %>% 
  ggplot(aes(x = term, 
             y = exp_beta,
             ymin = exp_beta_llci,
             ymax = exp_beta_ulci)) +
  geom_point(size = 4) + 
  geom_errorbar(width = 0.25) +
  # add a horizontal line where odds ratio == 1.0 (no effect):
  geom_hline(yintercept = 1, linetype = "dashed", 
             size = 1, color = "dodgerblue") + 
  labs(title = "95% CI: Pass sign up odds by factor",
       x = NULL,
       y = "Likehood by Factor (odds ratio, main effect)") + 
  coord_flip() + # rotates the plot
  theme_pubr()

# Predicting probabilities
data$base_prob <- predict( # store the predictions onto a new variable
  m5, # use the full model
  data,  
  type = "response") # calculate logit probabilities

# Inspect
head(data)
mean(data$base_prob)

# Predicting behavior <-> p >= 0.5 or p < 0.5
data$pred_conversion <- 1 * (data$base_prob >= 0.5)
table(data$pred_conversion)

# Evaluating the Model
table(data$conversion, data$pred_conversion) 

# ROC curve
rocobj <- roc(data$conversion, # actual reference
              data$base_prob) # predictions

# Area under the curve
rocobj$auc

# Calculate New Logit Probabilities
data_new <- data # make a copy of the data
data_new$total_pages_visited <- data_new$total_pages_visited + 1 # overwrite Monetary in the new data
data$new_prob <- predict(m5, data_new, type = "response") 

# Mean predicted base probability
mean(data$new_prob)

# Lift metric
(mean(data$new_prob) - mean(data$base_prob))/mean(data$base_prob)