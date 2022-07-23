### Carregando os pacotes exigidos
library(readr)
require(tidyverse)
require(lubridate)
set.seed(22019)
setwd("C:\\Users\\willi\\Documents\\UNB\\6º Semestre\\Modelos Lineares Generalizados\\Trabalhos\\Modelos para dados positivos")

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
listings2 <- listings %>% dplyr::select(id, covariaveis[-1])
calendar2 <- calendar %>% dplyr::select(-available)
df <- left_join(calendar2, listings2, by = c("listing_id" = "id"))
df <- df[,-1]
df.treino <- df[treino, ]
df.teste <- df[teste, ]

# Descritiva Preços
summary(df.treino$price)
hist(df.treino$price) # Escolhemos a Gama

# Arrumando as variáveis
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

amen <- str_split(str_remove_all(df.treino2$amenities, '[{}\"]'),
                 ',', simplify = T) %>% as.data.frame() %>%
  gather(V, word) %>% filter(str_length(word) > 0) %>% distinct(word) %>% na.omit() %>% as_vector()

names(amen) <- NULL

for(i in amen){
  df.treino2 <- df.treino2 %>% 
    mutate(!!i := str_detect(amenities, i))
}

df.treino2 <- dplyr::select(df.treino2, -amenities)

# Vendo quais variáveis tem NA
(tem_na <- names(colSums(is.na(df.treino2))[colSums(is.na(df.treino2)) > 0]))

# Vendo quais variáveis tem mais de 5000 NA
(tem_5000_na <- names(colSums(is.na(df.treino2))[colSums(is.na(df.treino2)) > 5000]))

# Modelos de teste
# mod1 <- glm(price ~ ., family = Gamma(link="log"), data = df.treino2)
# summary(mod1)
# mod2 <- glm(price ~ ., family = Gamma(link="log"), data = dplyr::select(df.treino2, -tem_na))
# summary(mod2)

# Criando banco sem as variáveis que tem muito muito NA e inputando cleaning_fee e secutiry_deposit
df.treino3 <- df.treino2 %>%
  dplyr::select(-square_feet, -host_acceptance_rate) %>% 
  replace_na(list(cleaning_fee = 0, 
                  security_deposit = 0))

# Nomes das variáveis que tem NA
(covars <- names(sort(colMeans(is.na(df.treino3))[colMeans(is.na(df.treino3)) > 0], decreasing = T)))

# Número de observações retirando variáveis que tem NA
df.treino3 %>% filter_at(vars(covars), all_vars(!is.na(.))) %>% nrow()

# Banco de dados só com as observações 100% preenchidas
df.treino4 <- df.treino3 %>% filter_at(vars(covars), all_vars(!is.na(.))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))

# Modelo Forward
mod_step <- readRDS('mod_step.rds')
summary(mod_step)

# Seleção de variáveis
coef(mod_step)[abs(coef(mod_step)) > 0.5] # mod1
coef(mod_step)[abs(coef(mod_step)) > 0.4] # mod2
coef(mod_step)[abs(coef(mod_step)) > 0.3] # mod3
coef(mod_step)[abs(coef(mod_step)) > 0.2] # mod4
coef(mod_step)[abs(coef(mod_step)) > 0.1] # mod5

names(coef(mod_step)[abs(coef(mod_step)) > 0.5])[-1]
names(coef(mod_step)[abs(coef(mod_step)) > 0.4])[-1]
names(coef(mod_step)[abs(coef(mod_step)) > 0.3])[-1]
names(coef(mod_step)[abs(coef(mod_step)) > 0.2])[-1]
names(coef(mod_step)[abs(coef(mod_step)) > 0.1])[-1]

fit1 <- glm(price ~ room_type + property_type + latitude + longitude, family = Gamma(link="log"),
            data = df.treino4)
summary(fit1)

fit2 <- glm(price ~ room_type + property_type + latitude + longitude + host_has_profile_pic, 
            family = Gamma(link="log"), data = df.treino4)
summary(fit2)

fit3 <- glm(price ~ room_type + property_type + latitude + longitude + host_has_profile_pic +
              neighbourhood_group_cleansed, family = Gamma(link="log"),
            data = df.treino4)
summary(fit3)

fit4 <- glm(price ~ room_type + property_type + latitude + longitude + host_has_profile_pic +
              neighbourhood_group_cleansed + Internet + `Wireless Internet` + 
              `Washer / Dryer`, family = Gamma(link="log"),
            data = df.treino4)
summary(fit4)

fit5 <- glm(price ~ room_type + property_type + latitude + longitude + host_has_profile_pic +
              neighbourhood_group_cleansed + Internet + `Wireless Internet` + 
              `Washer / Dryer` + bedrooms + month + bed_type + Doorman, family = Gamma(link="log"),
            data = df.treino4)
summary(fit5)


# Vendo quantas observações estão 100% preenchidas nas variáveis do fit5
df.treino2 %>%  select(price,room_type, property_type, latitude, longitude, host_has_profile_pic,
         neighbourhood_group_cleansed, `Wireless Internet`, Internet, `Washer / Dryer`, month, bedrooms,
         bed_type, Doorman) %>% na.omit() %>% nrow()

# Criando amostra de treino final
treino <- df.treino2 %>%  
  select(price,room_type, property_type, latitude, longitude, host_has_profile_pic, 
         neighbourhood_group_cleansed, `Wireless Internet`, Internet, `Washer / Dryer`, month, 
         bedrooms, bed_type, Doorman, review_scores_rating)

# Vendo quais tem NA
colSums(is.na(treino))

# Inputando Host_has_profile_pic
table(treino$host_has_profile_pic)
treino$host_has_profile_pic[is.na(treino$host_has_profile_pic)] <- TRUE

# Inputando Bedrooms
treino$bedrooms[is.na(treino$bedrooms)] <- mean(treino$bedrooms, na.rm = TRUE)

# Inputando Review_scores_rating
treino$review_scores_rating[is.na(treino$review_scores_rating)] <- mean(treino$review_scores_rating, na.rm = TRUE)

# Confirmando que não tem mais NA
sum(colSums(is.na(treino)))

# Ajustando modelo final
mod_final <- glm(price ~ room_type + property_type + latitude + longitude + host_has_profile_pic +
              neighbourhood_group_cleansed + Internet + `Wireless Internet` + 
              `Washer / Dryer` + bedrooms + month + bed_type + Doorman + review_scores_rating,
              family = Gamma(link="log"), data = treino)
summary(mod_final)

# Calculando o RMSE
(RMSE_treino <- sqrt(mean((treino$price - mod_final$fitted.values)^2)))

# Arrumando as variáveis no banco de teste
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

# Vendo quantas observações estão 100% preenchidas nas variáveis do fit_5
df.teste2 %>% select(price,room_type, property_type, latitude, longitude, host_has_profile_pic,
                      neighbourhood_group_cleansed, `Wireless Internet`, Internet, `Washer / Dryer`, 
                      month, bedrooms, bed_type, Doorman) %>% na.omit() %>% nrow()

# Criando amostra de teste final
teste <- df.teste2 %>%  
  select(price,room_type, property_type, latitude, longitude, host_has_profile_pic, 
         neighbourhood_group_cleansed, `Wireless Internet`, Internet, `Washer / Dryer`, month, 
         bedrooms, bed_type, Doorman, review_scores_rating)

# Vendo quais tem NA
colSums(is.na(teste))

# Inputando Host_has_profile_pic
table(teste$host_has_profile_pic)
teste$host_has_profile_pic[is.na(teste$host_has_profile_pic)] <- TRUE

# Inputando Bedrooms
teste$bedrooms[is.na(teste$bedrooms)] <- mean(teste$bedrooms, na.rm = TRUE)

# Inputando Review_scores_rating
teste$review_scores_rating[is.na(teste$review_scores_rating)] <- mean(teste$review_scores_rating, na.rm = TRUE)

# Confirmando que não tem mais NA
sum(colSums(is.na(teste)))

# Calculando valores preditos
preditos <- predict(mod_final, newdata = teste, type = "response")

# Calculando o RMSE
(RMSE_teste <- sqrt(mean((teste$price - preditos)^2)))
cbind(RMSE_treino, RMSE_teste)