df.treino3 <- readRDS('D:/AirBnB/cache/df.treino3.rds')
df.teste2 <- readRDS('D:/AirBnB/cache/df.teste2.rds')
teste <- df.teste2
require(tidyverse)

# Colunas numericas faltando

colSums(is.na(df.treino3))[colMeans(is.na(df.treino3))>0] # Remove os 67 errados

df_correto <- df.treino3 %>% filter(!is.na(days_as_host))

colSums(is.na(df_correto))[colMeans(is.na(df_correto))>0] # Todos que tinham 67 desapareceram
col_incomp <- names(colMeans(is.na(df_correto))>0)

col_comp <- names(df_correto)[colMeans(is.na(df_correto))==0][-1] # usar as colunas completas para predizer as incompletas retirando price

fo_host <- paste0('`', str_subset(col_comp, 'host')[-1], '`', collapse = '+') # Predizer apenas para as colunas de host, não converge se usar todas
# as colunas completas


# Métodos escolhidos ---------------
# host_response_rate, reviews_scores: Regressão beta
# bathrooms, bedrooms, beds, reviews_per_month, days_first_review, days_last_review: regressão poisson

# regressão beta ------------------

require(betareg)
# host_response_rate
temp <- df_correto
temp$host_response_rate[temp$host_response_rate==1] <- .99
temp$host_response_rate[temp$host_response_rate==0] <- .01


hrr_betareg_mod <- betareg(as.formula(paste('host_response_rate ~', fo_host)), data = temp)
hrr_predicted <- predict(hrr_betareg_mod, newdata = df_correto)
df_correto <- df_correto %>%
  mutate(host_response_rate_informed = as.numeric(!is.na(host_response_rate)),
         host_response_rate = coalesce(host_response_rate, hrr_predicted))

test_pred <- predict(hrr_betareg_mod, newdata = teste)
teste <- teste %>%
  mutate(host_response_rate_informed = as.numeric(!is.na(host_response_rate)),
         host_response_rate = coalesce(host_response_rate, test_pred, mean(host_response_rate, na.rm = T)))

saveRDS(hrr_betareg_mod, 'D:/AirBnB/cache/host_response_rate_betareg.rds')

# review_scores_rating escala de 0 a 100

temp <- df_correto
temp <- temp %>%
  mutate_at(vars(starts_with('review_scores_')), list(~./max(., na.rm = T))) %>%
  mutate_at(vars(starts_with('review_scores_')), list(~case_when(
    .==1~.99,
    .==0~.01,
    TRUE~.
  )))

rev_sc_rating <- betareg(as.formula(paste('review_scores_rating ~', fo_host)), data = temp)
rev_sc_predicted <- predict(rev_sc_rating, newdata = df_correto)

df_correto <- df_correto %>%
  mutate(review_scores_rating_informed = as.numeric(!is.na(review_scores_rating)),
         review_scores_rating = coalesce(review_scores_rating, rev_sc_predicted*100))

test_pred <- predict(rev_sc_rating, newdata = teste)
teste <- teste %>%
  mutate(review_scores_rating_informed = as.numeric(!is.na(review_scores_rating)),
         review_scores_rating = coalesce(review_scores_rating, test_pred*100, mean(review_scores_rating, na.rm = T)))
rm(rev_sc_rating, rev_sc_predicted, hrr_betareg_mod, hrr_predicted)
gc()
# Os outros reviews_scores possui escala de 0 a 10
col_reviews <- str_subset(col_incomp, 'review_scores_(?!rating)')
for(j in col_reviews){
  # j= col_reviews[1]
  j <- as.name(j)

  
  rev_sc_reg <- betareg(as.formula(paste(j, '~', fo_host)), data = temp)
  rev_sc_predicted <- predict(rev_sc_reg, newdata = df_correto)
  
  df_correto <- df_correto %>%
    mutate(!!paste0(j, '_informed') := as.numeric(!is.na(!!j)),
           !!j := coalesce(!!j, rev_sc_predicted*10))
  
  test_pred <- predict(rev_sc_reg, newdata = teste)
  teste <- teste %>%
    mutate(!!paste0(j, '_informed') := as.numeric(!is.na(!!j)),
           !!j := coalesce(!!j, test_pred*10, mean(!!j, na.rm = T)))
  
  saveRDS(rev_sc_reg, paste0('D:/AirBnB/cache/', j, '_betareg.rds'))
  cat('writted ', paste0('D:/AirBnB/cache/', j, '_betareg.rds'), '\n')
}
rm(temp, col_reviews); gc()
# Regressão Poisson ---------------

poi_reg_var <- unlist(str_split('bathrooms,bedrooms,beds,reviews_per_month,days_first_review,days_last_review', ','))

for(j in poi_reg_var){
  # j= poi_reg_var[2]
  j <- as.name(j)
  
  
  poi_reg <- glm(as.formula(paste(j, '~', fo_host)), data = df_correto, family = poisson('log'))
  poi_predicted <- predict(rev_sc_reg, newdata = df_correto, type=  'response')
  
  df_correto <- df_correto %>%
    mutate(!!paste0(j, '_informed') := as.numeric(!is.na(!!j)),
           !!j := coalesce(!!j, rev_sc_predicted))
  
  test_pred <- predict(poi_reg, newdata = teste)
  teste <- teste %>%
    mutate(!!paste0(j, '_informed') := as.numeric(!is.na(!!j)),
           !!j := coalesce(!!j, test_pred, mean(!!j, na.rm = T)))
  
  
  saveRDS(rev_sc_reg, paste0('D:/AirBnB/cache/', j, '_poisson_reg.rds'))
  cat('writted ',paste0('D:/AirBnB/cache/', j, '_poisson_reg.rds'), '\n')
}
teste <- teste %>% replace_na(list(cleaning_fee = 0,
                                   security_deposit = 0))
saveRDS(teste, 'D:/AirBnB/cache/teste_imputed.rds')
saveRDS(df_correto, 'D:/AirBnB/cache/df_imputed.rds')
