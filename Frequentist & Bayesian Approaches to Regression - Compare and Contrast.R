
### packages ------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(patchwork)

theme_set(theme_classic())

### data ----------------------------------------------------------
data("penguins")
dat <- penguins %>%
  select(bill_length = bill_length_mm, flipper_length = flipper_length_mm) %>%
  na.omit()

head(dat)

### EDA ------------------------------------------------------------
summary(dat)
cor.test(dat$bill_length, dat$flipper_length)

dat %>%
  ggplot(aes(x = bill_length, y = flipper_length)) +
  geom_point() +
  geom_smooth(method = "lm")

### Ordinary Least Squares Regression ------------------------------
fit_ols <- lm(flipper_length ~ I(bill_length - mean(dat$bill_length)), data = dat)
summary(fit_ols)


# Calculate the least squares regression line by hand now
dat_stats <- dat %>%
  summarize(x_bar = mean(bill_length),
            x_sd = sd(bill_length),
            y_bar = mean(flipper_length),
            y_sd = sd(flipper_length),
            r = cor(bill_length, flipper_length),
            x2 = x_bar^2,
            y2 = y_bar^2,
            xy_bar = x_bar * y_bar,
            .groups = "drop")

dat_stats

intercept <- dat_stats$y_bar
intercept

beta <- with(dat_stats,
             r * (y_sd / x_sd))

beta

x_bar <- dat_stats$x_bar
x_bar

cat("The model equation =", intercept, " + ", beta, "* x")


## Make predictions with the two models
dat %>%
  mutate(pred_model = predict(fit_ols),
         pred_hand = intercept + beta * (bill_length - x_bar))

# Calculate the estimated variance around the line
N_obs <- nrow(dat)

dat %>%
  mutate(pred = intercept + beta * (bill_length - x_bar),
         resid = (flipper_length - pred),
         resid2 = resid^2) %>%
  summarize(n_model_params = 2,
            deg_freedom = N_obs - n_model_params,
            model_var = sum(resid2) / deg_freedom,
            model_sd = sqrt(model_var))

# compare to the model built with the lm() function
sigma(fit_ols)

### Bayesian Linear Regression by Hand

## First we need to calculate the sum of squared error for X
# ss_x = N * (mean(mu_x^2) - mu_x^2)

N <- nrow(dat)
mean_x2 <- mean(dat$bill_length^2)
mu_x2 <- mean(dat$bill_length)^2

N
mean_x2
mu_x2

ss_x <- N * (mean_x2 - mu_x2)
ss_x

## Priors
# For the slope coefficient we decide to have a normal prior, N(1, 2^2)
# For the intercept coefficient we choose a normal prior, N(180, 10^2)
# We don't know the true variance so we use the estimated variance from the least
# squares regression line

prior_model_var <- sigma(fit_ols)^2
prior_model_var

prior_slope_mu <- 1
prior_slope_var <- 1^2

prior_intercept_mu <- 180
prior_intercept_var <- 10^2

## Posterior precision for the slope
# 1/prior_slope_var + (ss_x / prior_model_var)

posterior_slope_precision <- 1 / prior_slope_var + ss_x / prior_model_var
posterior_slope_precision

# Convert to SD
posterior_slope_sd <- posterior_slope_precision^-(1/2)
posterior_slope_sd

## Posterior mean for the slope
# (1/prior_slope_var) / posterior_slope_var * prior_slope_mu + (ss_x / prior_model_var) / posterior_slope_var * beta
posterior_slope_mu <- (1/prior_slope_var) / posterior_slope_precision * prior_slope_mu + (ss_x / prior_model_var) / posterior_slope_precision * beta
posterior_slope_mu

## Plot prior and posterior for the slope
set.seed(4)
prior_sim <- rnorm(n = 1e4, mean = prior_slope_mu, sd = sqrt(prior_slope_var))
posterior_sim <- rnorm(n = 1e4, mean = posterior_slope_mu, sd = posterior_slope_sd)

plot(density(posterior_sim),
     col = "blue",
     lwd = 4,
     xlim = c(-2, 3),
     main = "Prior & Posterior\nfor\nBayesian Regression Slope Coefficient")
lines(density(prior_sim),
      col = "red",
      lty = 2,
      lwd = 4)
legend("topleft",
       legend = c("Prior", "Posterior"),
       col = c("red", "blue"),
       lty = c(2, 1),
       lwd = c(2,2))


## Posterior precision for the intercept
# 1/prior_intercept_var + N/prior_model_var

posterior_intercept_precision <- 1/prior_intercept_var + N/prior_model_var
posterior_intercept_precision

# Convert to SD
posterior_intercept_sd <- posterior_intercept_precision^-(1/2)
posterior_intercept_sd

## Posterior intercept mean
# (1/prior_intercept_var) / posterior_intercept_precision * prior_intercept_mu + (N/prior_model_var) / posterior_intercept_precision * intercept
posterior_intercept_mu <- (1/prior_intercept_var) / posterior_intercept_precision * prior_intercept_mu + (N/prior_model_var) / posterior_intercept_precision * intercept
posterior_intercept_mu


## Our posterior outputs:
cat("The posterior intercept =", round(posterior_intercept_mu, 2), "±", round(posterior_intercept_sd, 2))
cat("The posterior slope =", round(posterior_slope_mu, 2), "±", round(posterior_slope_sd, 3))

# compare tot he OLS model
coef(fit_ols) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(Variable = case_when(Variable == 'I(bill_length - mean(dat$bill_length))' ~ "Mean Centered Bill Length",
                             TRUE ~ Variable)) %>%
  bind_cols(summary(fit_ols)$coefficients[, 2]) %>%
  setNames(c("Variable", "Effect", "SE"))

## 95% Credible Interval for the Posterior Intercept * Slope
posterior_intercept_mu + qt(p = c(0.025, 0.975), df = N - 2) * posterior_intercept_sd
posterior_slope_mu + qt(p = c(0.025, 0.975), df = N - 2) * posterior_slope_sd

# compare
confint(fit_ols)

## bayes model
cat("Flipper Length =", round(posterior_intercept_mu, 2), "+", round(posterior_slope_mu,2), "* x")


## Use simulation for the slope and intercept
set.seed(413)
posterior_intercept_sim <- rnorm(n = 1e4, mean = posterior_intercept_mu, sd = posterior_slope_sd)
posterior_slope_sim <- rnorm(n = 1e4, mean = posterior_slope_mu, sd = posterior_slope_sd)

par(mfrow = c(1, 2))
hist(posterior_intercept_sim)
hist(posterior_slope_sim)
dev.off()

quantile(posterior_intercept_sim, probs = c(0.5, 0.025,0.975))
quantile(posterior_slope_sim, probs = c(0.5, 0.025, 0.975))

mean(posterior_intercept_sim)
mean(posterior_slope_sim)

mean(posterior_intercept_sim) + qt(p = c(0.025, 0.975), df = N - 2) * sd(posterior_intercept_sim)
mean(posterior_slope_sim) + qt(p = c(0.025, 0.975), df = N - 2) * sd(posterior_slope_sim)


#### Making Predictions
## Add all predictions to the original data set
dat_final <- dat %>%
  mutate(pred_ols = predict(fit_ols),
         pred_ols_by_hand = intercept + beta * (bill_length - x_bar),
         pred_bayes_by_hand = posterior_intercept_mu + posterior_slope_mu * (bill_length - x_bar))

head(dat_final, 10)

## Predictions with uncertainty from the OLS model
ols_preds <- dat_final %>%
  bind_cols(predict(fit_ols, newdata = ., interval = "prediction")) %>%
  ggplot(aes(x = fit, y = flipper_length)) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  ggtitle("OLS Predictions with 95% Prediction Intervals")

## Now predict with the Bayesian Model
# The error is calculated as:
# residual_variance = prior_model_var +  posterior_intercept_sd^2 + posterior_slope^2*(x - x_bar)
bayes_preds <- dat %>%
  mutate(fit = posterior_intercept_mu + posterior_slope_mu * (bill_length-x_bar),
         error_var = prior_model_var + posterior_intercept_sd^2 + posterior_slope_sd^2 * (bill_length - x_bar),
         rse = sqrt(error_var),
         lwr = fit - qt(p = 0.975, df = N - 2) * rse,
         upr = fit + qt(p = 0.975, df = N - 2) * rse) %>%
  ggplot(aes(x = fit, y = flipper_length)) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  ggtitle("Bayesian Predictions with 95% Prediction Intervals")


ols_preds | bayes_preds

### Predicting a full distribution with Bayes
single_obs <- dat %>% slice(36)
single_obs

pred_bayes <- single_obs %>%
  mutate(fit = posterior_intercept_mu + posterior_slope_mu * (bill_length-x_bar),
         error_var = prior_model_var + posterior_intercept_sd^2 + posterior_slope_sd^2 * (bill_length - x_bar),
         rse = sqrt(error_var),
         lwr = fit - qt(p = 0.975, df = N - 2) * rse,
         upr = fit + qt(p = 0.975, df = N - 2) * rse) 

set.seed(582)
pred_bayes_sim <- rnorm(n = 1e4, mean = pred_bayes$fit, sd = pred_bayes$rse)

mean(pred_bayes_sim)
quantile(pred_bayes_sim, probs = c(0.5, 0.025, 0.975))
mean(pred_bayes_sim) + qt(p = c(0.025, 0.975), df = N - 2) * sd(pred_bayes_sim)

predict(fit_ols, newdata = single_obs, interval = "prediction")

hist(pred_bayes_sim)
