require(tidyverse)
df_imputed <- readRDS('D:/AirBnB/cache/df_imputed.rds')
df_imputed <- df_imputed %>% mutate_if(is.factor, list(~factor(., ordered = F))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))
fit <- glm(price ~ host_response_time + host_is_superhost + host_total_listings_count + 
             host_has_profile_pic + neighbourhood_group_cleansed + latitude + 
             is_location_exact + property_type + room_type + accommodates + 
             bathrooms + bedrooms + security_deposit + cleaning_fee + 
             guests_included + number_of_reviews + review_scores_rating + 
             review_scores_accuracy + review_scores_location + review_scores_value + 
             cancellation_policy + calculated_host_listings_count + reviews_per_month + 
             month + week_day + days_first_review + days_last_review + 
             Internet + TV + `Wireless Internet` + `Cable TV` +
             `Elevator in Building` + longitude, family = Gamma('log'), data = df_imputed)

summary(fit)


# Puxando teste

teste_inputed <- readRDS('D:/AirBnB/cache/teste_imputed.rds')
teste_inputed <- teste_inputed %>%
  replace_na(list(host_is_superhost = T,
                  host_response_time = 'Unknown',
                  host_has_profile_pic = T)) %>%
  mutate(host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = T),
                                            host_total_listings_count)) %>%
  mutate_if(is.factor, list(~factor(., ordered = F))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))
  
  
  

predito_teste <- predict(fit, newdata = teste_inputed, type = 'response')

# Calculando o RMSE
(RMSE_treino <- sqrt(mean((df_imputed$price - fit$fitted.values)^2)))
(RMSE_teste <- sqrt(mean((teste_inputed$price - predito_teste)^2)))
cbind(RMSE_treino, RMSE_teste)
