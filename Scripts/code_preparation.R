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
tem_na <- names(colSums(is.na(df.treino2))[colSums(is.na(df.treino2)) > 0])
tem_5000_na <- names(colSums(is.na(df.treino2))[colSums(is.na(df.treino2)) > 5000])

# mod1 <- glm(price ~ ., family = Gamma(link="log"), data = df.treino2)
# summary(mod1)
# mod2 <- glm(price ~ ., family = Gamma(link="log"), data = dplyr::select(df.treino2, -tem_na))
# summary(mod2)

df.treino3 = df.treino2 %>%
  dplyr::select(-square_feet, -host_acceptance_rate) %>% 
  replace_na(list(cleaning_fee = 0, 
                  security_deposit = 0))
covars <- names(sort(colMeans(is.na(df.treino3))[colMeans(is.na(df.treino3)) > 0], 
                     decreasing = T))

df.treino3 %>% filter_at(vars(covars), all_vars(!is.na(.))) %>% nrow()

df.treino4 = df.treino3 %>% filter_at(vars(covars), all_vars(!is.na(.))) %>%
  mutate_if(is.logical, list(~as.numeric(.)))

saveRDS(df.treino3, 'D:/AirBnB/cache/df.treino3.rds')
saveRDS(df.treino4, 'D:/AirBnB/cache/df.treino4.rds')

# rm(list = ls()[!ls()%in%c("df.treino4")])
# gc()
# 
# mod3 <- glm(price ~ . -`Dog(s)` - `Cat(s)` -`Other pet(s)`, family = Gamma(link="log"), data = df.treino4)
# summary(mod3)
# 
# require(MASS)
# mod0 = glm(price ~ 1, family = Gamma(link="log"), data = df.treino4)
# 
# ultimo_modelo <- glm(price ~ accommodates + room_type + neighbourhood_group_cleansed +
# bedrooms + cleaning_fee + month + bathrooms + days_last_review +
#   host_is_superhost + review_scores_location + reviews_per_month +
#   property_type + `Elevator in Building` + week_day + calculated_host_listings_count +
#   latitude + bed_type + guests_included + longitude + `Cable TV` +
#   review_scores_value + review_scores_rating + host_has_profile_pic +
#   Doorman + `Laptop Friendly Workspace` + Shampoo + `Suitable for Events` +
#   `Smoking Allowed` + review_scores_accuracy + host_total_listings_count +
#   `24-Hour Check-in` + `Pets live on this property` + `Indoor Fireplace` +
#   `Air Conditioning` + host_identity_verified + `Fire Extinguisher` +
#   security_deposit + `Wireless Internet` + Internet + maximum_nights +
#   Iron + Hangers + is_location_exact + `Hot Tub` + host_response_time +
#   `Wheelchair Accessible` + Kitchen + require_guest_profile_picture +
#   `Safety Card` + beds + `Lock on Bedroom Door` + `Hair Dryer` +
#   `Washer / Dryer` + Heating + cancellation_policy + `Smoke Detector` +
#   Breakfast + days_first_review + number_of_reviews + minimum_nights +
#   days_as_host + instant_bookable + TV + host_response_rate +
#   `Family/Kid Friendly` + `Carbon Monoxide Detector` + review_scores_checkin +
#   `Buzzer/Wireless Intercom` + extra_people + Essentials +
#   review_scores_cleanliness + Pool + year + review_scores_communication +
#   `First Aid Kit` + `Free Parking on Premises` + require_guest_phone_verification +
#   `Pets Allowed`, family = Gamma(link="log"), data = df.treino4)
# 
# mod_step = stepAIC(ultimo_modelo, direction = "forward", scope = list(upper = mod3, lower = mod0))
# 
# 
# 
# saveRDS(mod_step, 'D:/AirBnB/cache/modelo_stepAIC.rds')
# saveRDS(mod3, 'D:/AirBnB/cache/modelo_3.rds')

# mod_step = readRDS('D:/AirBnB/cache/modelo_stepAIC.rds')
# 
# fo <- formula(mod_step)
# rm(mod0, mod3, ultimo_modelo)
# gc()
# 
# df.treino <- readRDS('D:/AirBnB/cache/df.treino.rds')



# Nao da para fazer grafico de envelope

# pred <- predict(mod_step, type = 'response')



# Gerando valores
 # Parametro alpha
# Média alpha/beta, mu = alpha/beta -> beta = alpha/mu = fi/fitted(eta_i)


