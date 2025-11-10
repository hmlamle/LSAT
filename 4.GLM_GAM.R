# Hannah-Marie Lamle
# LSAT habitat suitability modelling
# Combine all data to create GAM and GLMs 
# Creation date of this document: 11/6/2025

library(ggplot2)

model_turf <- glm(turf_length ~ rugo_mean + sapr + slope_mean + std_curve + plan_curve + tpi, 
                  family = Gamma(link = "log"), data = master)

# Create a sequence of rugo_mean values covering observed range
rugo_seq <- seq(min(master$rugo_mean, na.rm = TRUE),
                max(master$rugo_mean, na.rm = TRUE),
                length.out = 100)

# Create a new data frame holding other predictors constant at median
newdata <- data.frame(
  rugo_mean = rugo_seq,
  sapr = median(master$sapr, na.rm = TRUE),
  slope_mean = median(master$slope_mean, na.rm = TRUE),
  std_curve = median(master$std_curve, na.rm = TRUE),
  plan_curve = median(master$plan_curve, na.rm = TRUE),
  tpi = median(master$tpi, na.rm = TRUE)
)

pred <- predict(model_turf, newdata, type = "response", se.fit = TRUE)

newdata$fit <- pred$fit
newdata$se <- pred$se.fit

# Calculate 95% CI on response scale (approximate)
newdata <- newdata %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

ggplot(newdata, aes(x = rugo_mean, y = fit)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(x = "Rugosity (rugo_mean)", y = "Predicted Turf Length",
       title = "Effect of Rugosity on Turf Length") +
  theme_minimal()


model_sediment <- glm(sediment_depth ~ rugo_mean + sapr + slope_mean + std_curve + plan_curve + tpi, 
                      family = Gamma(link = "log"), data = master)
