# Arrumando o banco de dados --------------------------

### Carregando os pacotes exigidos
library(readr)
require(tidyverse)
set.seed(22019)
setwd("C:\\Users\\alvar\\Dropbox\\6Semestre\\Modelos_Lineares_Generalizados")

### Carregando os dados
listings <- read_csv("listings.csv")
calendar <- read_csv("calendar.csv")
### Limpando a base
calendar <- calendar %>%
  filter(!is.na(price)) %>% # filtrando os imóveis disponíveis
  mutate(price=str_replace(.$price, "\\$", "") %>%
           str_replace("\\,", "") %>%
           as.numeric()) # transformando os dados de preços em numérico
### Dividindo o banco de dados em treino e teste
n.obs <- nrow(calendar)
treino <- sample(n.obs, n.obs*.8)
teste <- setdiff(1:n.obs, treino)
calendar.treino <- calendar[treino, ]
calendar.teste <- calendar[teste, ]

### Definindo as informações a serem consideradas no modelo
covariaveis <- c("date", "host_since", "host_location", "host_response_time",
                 "host_response_rate", "host_acceptance_rate", "host_is_superhost",
                 "host_total_listings_count", "host_has_profile_pic", "host_identity_verified",
                 "neighbourhood_group_cleansed", "zipcode", "latitude", "longitude", "is_location_exact",
                 "property_type", "room_type", "accommodates", "bathrooms", "bedrooms", "beds",
                 "bed_type", "amenities", "square_feet", "security_deposit", "cleaning_fee",
                 "guests_included", "extra_people", "minimum_nights", "maximum_nights","number_of_reviews",
                 "first_review", "last_review", "review_scores_rating", "review_scores_accuracy",
                 "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication",
                 "review_scores_location", "review_scores_value", "requires_license", "instant_bookable",
                 "cancellation_policy", "require_guest_profile_picture","require_guest_phone_verification",
                 "calculated_host_listings_count", "reviews_per_month")

# Início do nosso trabalho
listings2 = listings %>% dplyr::select(id, covariaveis[-1])
calendar2 = calendar %>% dplyr::select(-available)
df = left_join(calendar2, listings2, by = c("listing_id" = "id"))
df = df[,-1]
df.treino = df[treino, ]
df.teste = df[teste, ]
dir.create('D:/AirBnB/cache')
lapply(ls(), function(i) saveRDS(get(i), paste0('D:/AirBnB/cache/', i, '.rds')))
rm(list = ls()[!ls()%in%c("df.treino", "df.teste")])
gc()

# Descritiva Preço
summary(df.treino$price)
hist(df.treino$price) # Escolhemos a Gama

# Arrumando as variáveis
require(lubridate)
df.treino2 <- df.treino %>%
  mutate(year = factor(year(date)), 
         month = month(date, label = TRUE),
         week_day = wday(date, label = TRUE),
         days_as_host = as.numeric(date - host_since),
         host_response_time = factor(host_response_time,
                                     levels = c("N/A", 
                                                "within an hour",
                                                "within a few hours",
                                                "within a day",
                                                "a few days or more"),
                                     labels = c("Unknown", 
                                                "within an hour",
                                                "within a few hours",
                                                "within a day",
                                                "a few days or more"),
                                     ordered = TRUE),
         host_response_rate = as.numeric(sub("%","",host_response_rate))/100,
         host_acceptance_rate = as.numeric(sub("%","",host_acceptance_rate))/100,
         property_type = ifelse(is.na(property_type), "Unknown", property_type),
         room_type = factor(room_type),
         bed_type = factor(bed_type),
         security_deposit = as.numeric(gsub(",|[$]","",security_deposit)),
         cleaning_fee = as.numeric(gsub(",|[$]","",cleaning_fee)),
         extra_people = as.numeric(gsub(",|[$]","",extra_people)),
         days_first_review = as.numeric(date - first_review),
         days_last_review = as.numeric(date - last_review),
         cancellation_policy = factor(cancellation_policy)) %>%
  dplyr::select(-date,-host_since,-host_location,-zipcode,-first_review,-last_review,-requires_license)

amen = str_split(str_remove_all(df.treino2$amenities, '[{}\"]'),
                 ',', simplify = T) %>% as.data.frame() %>%
  gather(V, word) %>% filter(str_length(word) > 0) %>% distinct(word) %>% na.omit() %>% as_vector()
names(amen) <- NULL

for(i in amen){
  df.treino2 = df.treino2 %>% 
    mutate(!!i := str_detect(amenities, i))
}

df.treino2 <- dplyr::select(df.treino2, -amenities)

df.treino3 = df.treino2 %>%
  dplyr::select(-square_feet, -host_acceptance_rate) %>% 
  replace_na(list(cleaning_fee = 0, 
                  security_deposit = 0))
covars <- names(sort(colMeans(is.na(df.treino3))[colMeans(is.na(df.treino3)) > 0], 
                     decreasing = T))

df.treino3 %>% filter_at(vars(covars), all_vars(!is.na(.))) %>% nrow()

df.treino4 = df.treino3 %>% filter_at(vars(covars), all_vars(!is.na(.))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))

rm(list = ls()[!ls()%in%c("df.treino4")])
gc()

mod3 <- glm(price ~ . -`Dog(s)` - `Cat(s)` -`Other pet(s)`, family = Gamma(link="log"), data = df.treino4)
summary(mod3)

require(MASS)
mod0 = glm(price ~ 1, family = Gamma(link="log"), data = df.treino4)

ultimo_modelo <- glm(price ~ accommodates + room_type + neighbourhood_group_cleansed +
                       bedrooms + cleaning_fee + month + bathrooms + days_last_review +
                       host_is_superhost + review_scores_location + reviews_per_month +
                       property_type + `Elevator in Building` + week_day + calculated_host_listings_count +
                       latitude + bed_type + guests_included + longitude + `Cable TV` +
                       review_scores_value + review_scores_rating + host_has_profile_pic +
                       Doorman + `Laptop Friendly Workspace` + Shampoo + `Suitable for Events` +
                       `Smoking Allowed` + review_scores_accuracy + host_total_listings_count +
                       `24-Hour Check-in` + `Pets live on this property` + `Indoor Fireplace` +
                       `Air Conditioning` + host_identity_verified + `Fire Extinguisher` +
                       security_deposit + `Wireless Internet` + Internet + maximum_nights +
                       Iron + Hangers + is_location_exact + `Hot Tub` + host_response_time +
                       `Wheelchair Accessible` + Kitchen + require_guest_profile_picture +
                       `Safety Card` + beds + `Lock on Bedroom Door` + `Hair Dryer` +
                       `Washer / Dryer` + Heating + cancellation_policy + `Smoke Detector` +
                       Breakfast + days_first_review + number_of_reviews + minimum_nights +
                       days_as_host + instant_bookable + TV + host_response_rate +
                       `Family/Kid Friendly` + `Carbon Monoxide Detector` + review_scores_checkin +
                       `Buzzer/Wireless Intercom` + extra_people + Essentials +
                       review_scores_cleanliness + Pool + year + review_scores_communication +
                       `First Aid Kit` + `Free Parking on Premises` + require_guest_phone_verification +
                       `Pets Allowed`, family = Gamma(link="log"), data = df.treino4)

mod_step = stepAIC(ultimo_modelo, direction = "forward", scope = list(upper = mod3, lower = mod0))



saveRDS(mod_step, 'D:/AirBnB/cache/modelo_stepAIC.rds')
saveRDS(mod3, 'D:/AirBnB/cache/modelo_3.rds')

rm(mod_step, mod3, df.treino4)
gc()
# Imputação ----------------------------

require(tidyverse)
require(lubridate)
df.teste <- readRDS('D:/AirBnB/cache/df.teste.rds')
df.teste2 <- df.teste %>%
  mutate(year = factor(year(date)), 
         month = month(date, label = TRUE),
         week_day = wday(date, label = TRUE),
         days_as_host = as.numeric(date - host_since),
         host_response_time = factor(host_response_time,
                                     levels = c("N/A", 
                                                "within an hour",
                                                "within a few hours",
                                                "within a day",
                                                "a few days or more"),
                                     labels = c("Unknown", 
                                                "within an hour",
                                                "within a few hours",
                                                "within a day",
                                                "a few days or more"),
                                     ordered = TRUE),
         host_response_rate = as.numeric(sub("%","",host_response_rate))/100,
         host_acceptance_rate = as.numeric(sub("%","",host_acceptance_rate))/100,
         property_type = ifelse(is.na(property_type), "Unknown", property_type),
         room_type = factor(room_type),
         bed_type = factor(bed_type),
         security_deposit = as.numeric(gsub(",|[$]","",security_deposit)),
         cleaning_fee = as.numeric(gsub(",|[$]","",cleaning_fee)),
         extra_people = as.numeric(gsub(",|[$]","",extra_people)),
         days_first_review = as.numeric(date - first_review),
         days_last_review = as.numeric(date - last_review),
         cancellation_policy = factor(cancellation_policy)) %>%
  dplyr::select(-date,-host_since,-host_location,-zipcode,-first_review,-last_review,-requires_license)

amen <- str_split(str_remove_all(df.teste2$amenities, '[{}\"]'),
                  ',', simplify = T) %>% as.data.frame() %>%
  gather(V, word) %>% filter(str_length(word) > 0) %>% distinct(word) %>% na.omit() %>% as_vector()

names(amen) <- NULL

for(i in amen){
  df.teste2 <- df.teste2 %>% 
    mutate(!!i := str_detect(amenities, i))
}

df.teste2 <- dplyr::select(df.teste2, -amenities)

saveRDS(df.teste2, 'D:/AirBnB/cache/df.teste2.rds')

df.treino3 <- readRDS('D:/AirBnB/cache/df.treino3.rds')
df.teste2 <- readRDS('D:/AirBnB/cache/df.teste2.rds')
teste <- df.teste2


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

rm(list = ls()); gc()

# Construção de Modelo -----------------------------

require(tidyverse)
df_imputed <- readRDS('D:/AirBnB/cache/df_imputed.rds')
df_imputed <- df_imputed %>% mutate_if(is.factor, list(~factor(., ordered = F))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))
# Modelo escolhido pelo método automático, com algumas alterações
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

teste_inputed <- readRDS('D:/AirBnB/cache/teste_imputed.rds')
teste_inputed <- teste_inputed %>%
  replace_na(list(host_is_superhost = T, # imputado pela Moda
                  host_response_time = 'Unknown', # Nova categoria
                  host_has_profile_pic = T)) %>% # Moda
  mutate(host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm = T),
                                            host_total_listings_count)) %>% # host_total_listings_count imputado pela média
  mutate_if(is.factor, list(~factor(., ordered = F))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))




predito_teste <- predict(fit, newdata = teste_inputed, type = 'response') # Predizendo

# Calculando o RMSE
(RMSE_treino <- sqrt(mean((df_imputed$price - fit$fitted.values)^2))) # EQM treino
(RMSE_teste <- sqrt(mean((teste_inputed$price - predito_teste)^2))) # EQM teste
cbind(RMSE_treino, RMSE_teste)
