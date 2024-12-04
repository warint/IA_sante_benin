library(dplyr)
library(tidymodels)
library(lubridate)
library(xgboost)
library(glmnet)    # Pour Ridge/Lasso
library(ranger)    # Pour Random Forest
library(caret) 
library(leaflet)
library(robustbase)
library(readxl)
library(ggplot2)
library(recipes)
library(dbscan)
library(cluster)
library(htmlwidgets)
library(viridis)

# Chargement des données
medication_access_data <- read_xlsx("./dataset_combined_final1.xlsx")
phcies_data <- read_xlsx("./phcies_benin.xlsx")

# Renommer les colonnes pour correspondre aux noms attendus
colnames(medication_access_data)[colnames(medication_access_data) == "lat"] <- "med_lat"
colnames(medication_access_data)[colnames(medication_access_data) == "long"] <- "med_lon"

# Conversion des colonnes en types appropriés
medication_access_data$med_lon <- as.numeric(medication_access_data$med_lon)
medication_access_data$med_lat <- as.numeric(medication_access_data$med_lat)
medication_access_data$date <- as.Date(medication_access_data$date, format = "%Y-%m")

# Transformation des colonnes de disponibilité en valeurs numériques
medication_access_data <- medication_access_data %>%
  mutate(disponibilite_num = ifelse(location_disponibility == "Oui", 1, 0),
         availability_num = ifelse(availability == "Oui", 1, 0))

# Calcul des demandes totales et autres métriques
# Calcul de la demande totale par location et médicament
demande_totale_location <- medication_access_data %>%
  group_by(location, medications) %>%
  summarize(
    demande_totale_location = n(),  # Compte les demandes de chaque médicament
    disponibilite_moyenne_location = mean(disponibilite_num, na.rm = TRUE),
    prix_moyen_location = median(price_approx, na.rm = TRUE),  # Prix moyen par médicament
    total_price_location = sum(price_approx, na.rm = TRUE),  # Somme des prix des médicaments demandés
    resources_available = mean(financial_resources, na.rm = TRUE)  # Ressources financières disponibles pour la localité
  ) %>%
  ungroup()

# Calcul du déficit ou excédent des ressources financières
demande_totale_location <- demande_totale_location %>%
  mutate(
    deficit_resources = total_price_location - resources_available,  # Calcul du déficit ou excédent
    besoin_aide = ifelse(deficit_resources > 0, "Oui", "Non")  # Si déficit > 0, il y a besoin d'aide
  )

# Affichage des résultats
print(demande_totale_location)




# Calcul de la demande totale par disponibilité des médicaments et localité
demande_totale_location_availability <- medication_access_data %>%
  group_by(location_availability, medications) %>%
  summarize(
    demande_totale_location_availability = n(),  # Compte les demandes pour chaque médicament
    disponibilite_moyenne_location_availability = mean(disponibilite_num, na.rm = TRUE),
    prix_moyen_location_availability = mean(price_approx, na.rm = TRUE),  # Prix moyen des médicaments
    total_price_location_availability = sum(price_approx, na.rm = TRUE),  # Somme des prix des médicaments demandés
    resources_available = mean(financial_resources, na.rm = TRUE)  # Ressources financières disponibles pour la localité
  ) %>%
  ungroup()

# Calcul du déficit ou excédent des ressources financières
demande_totale_location_availability <- demande_totale_location_availability %>%
  mutate(
    deficit_resources = total_price_location_availability - resources_available,  # Calcul du déficit ou excédent
    besoin_aide = ifelse(deficit_resources > 0, "Oui", "Non")  # Si déficit > 0, il y a besoin d'aide
  )

# Affichage des résultats
print(demande_totale_location_availability)

# Supprimer les doublons dans la table demande_totale_location_availability
demande_totale_location_availability_distinct <- demande_totale_location_availability %>%
  distinct(location_availability, .keep_all = TRUE)  # Garder une seule ligne par 'location_availability'


# Ajouter demande_totale au dataframe original pour location
medication_access_data <- medication_access_data %>%
  left_join(demande_totale_location %>% select(location, demande_totale_location, disponibilite_moyenne_location, prix_moyen_location), by = "location")

# Ajouter demande_totale au dataframe original pour location_availability
# Jointure avec la table principale après suppression des doublons
medication_access_data <- medication_access_data %>%
  left_join(demande_totale_location_availability_distinct %>%
              select(location_availability, demande_totale_location_availability, disponibilite_moyenne_location_availability, prix_moyen_location_availability), 
            by = "location_availability")


# Préparation des données pour le clustering par location
cluster_data_location <- medication_access_data %>%
  group_by(location) %>%
  summarize(
    # Nombre de demandes total pour chaque location
    demande_totale_location = n(),  # Comptabilise le nombre de demandes
    
    # Disponibilité moyenne des médicaments pour chaque location
    disponibilite_moyenne_location = mean(disponibilite_num, na.rm = TRUE),
    
    # Prix moyen des médicaments pour chaque location
    prix_moyen_location = median(price_approx, na.rm = TRUE),
    
    # Somme des doses journalières pour chaque location
    daily_dose_totale_location = sum(morning_dose + noon_dose + evening_dose, na.rm = TRUE)
  ) %>%
  ungroup()

# Affichage des résultats
print(cluster_data_location)

# Détermination du nombre optimal de clusters pour location avec DBSCAN
dbscan_location <- dbscan::dbscan(cluster_data_location[, -1], eps = 0.2, minPts = 2)
cluster_data_location$cluster_location <- dbscan_location$cluster

# Fusionner les clusters avec les données originales pour location
medication_access_data <- medication_access_data %>%
  left_join(cluster_data_location %>% select(location, cluster_location), by = "location")

# Préparation des données pour le clustering par location_availability
cluster_data_location_availability <- medication_access_data %>%
  group_by(location_availability) %>%
  summarize(
    # Nombre de demandes total pour chaque location_availability
    demande_totale_location_availability = n(),  # Comptabilise le nombre de demandes
    
    # Disponibilité moyenne des médicaments pour chaque location_availability
    disponibilite_moyenne_location_availability = mean(disponibilite_num, na.rm = TRUE),
    
    # Prix moyen des médicaments pour chaque location_availability
    prix_moyen_location_availability = median(price_approx, na.rm = TRUE),
    
    # Somme des doses journalières pour chaque location_availability
    daily_dose_totale_location_availability = sum(morning_dose + noon_dose + evening_dose, na.rm = TRUE)
  ) %>%
  ungroup()

# Affichage des résultats
print(cluster_data_location_availability)

# Détermination du nombre optimal de clusters pour location_availability avec DBSCAN
dbscan_location_availability <- dbscan::dbscan(cluster_data_location_availability[, -1], eps = 0.2, minPts = 2)
cluster_data_location_availability$cluster_location_availability <- dbscan_location_availability$cluster

# Fusionner les clusters avec les données originales pour location_availability
medication_access_data <- medication_access_data %>%
  left_join(cluster_data_location_availability %>% select(location_availability, cluster_location_availability), by = "location_availability")

# Calcul du chiffre d'affaires par médicament pour chaque location
chiffre_affaire_location <- medication_access_data %>%
  group_by(location, medications) %>%
  summarize(
    demande_totale_medicament_location = n(),  # Compte le nombre de demandes pour chaque médicament
    chiffre_affaire_location = sum(price_approx, na.rm = TRUE),  # Somme des prix approximatifs pour chaque médicament
    .groups = "drop"  # Retirer la structure de groupe après la summarisation
  )

# Affichage du résultat
print(chiffre_affaire_location)

# Calcul du chiffre d'affaires par médicament pour chaque location_availability
chiffre_affaire_location_availability <- medication_access_data %>%
  group_by(location_availability, medications) %>%
  summarize(
    demande_totale_medicament_location_availability = n(),  # Compte le nombre de demandes pour chaque médicament
    chiffre_affaire_location_availability = sum(price_approx, na.rm = TRUE),  # Somme des prix approximatifs pour chaque médicament
    .groups = "drop"  # Retirer la structure de groupe après la summarisation
  )

# Affichage du résultat
print(chiffre_affaire_location_availability)

# Joindre les résultats du chiffre d'affaires au dataframe
medication_access_data <- medication_access_data %>%
  left_join(chiffre_affaire_location %>%
              select(location, medications, chiffre_affaire_location), 
            by = c("location", "medications"))

medication_access_data <- medication_access_data %>%
  left_join(chiffre_affaire_location_availability %>%
              select(location_availability, medications, chiffre_affaire_location_availability), 
            by = c("location_availability", "medications"))

# Ajout des informations sur les médicaments concernés par location
medications_by_location <- medication_access_data %>%
  group_by(location) %>%
  summarize(
    medications_concernes_location = paste(unique(medications), collapse = ", "), 
    .groups = "drop"  # Retirer la structure de groupe après la summarisation
  )

# Ajout des informations sur les médicaments concernés par location_availability
medications_by_location_availability <- medication_access_data %>%
  group_by(location_availability) %>%
  summarize(
    medications_concernes_location_availability = paste(unique(medications), collapse = ", "), 
    .groups = "drop"  # Retirer la structure de groupe après la summarisation
  )

# Jointure pour voir les correspondances
medication_access_data <- medication_access_data %>%
  full_join(medications_by_location, by = "location") %>%
  full_join(medications_by_location_availability, by = "location_availability", suffix = c("_location", "_location_availability"))

# Affichage des colonnes pour vérifier
print(colnames(medication_access_data))


# Calcul des métriques par cluster géographique
metrics_by_cluster_location <- medication_access_data %>%
  group_by(cluster_location) %>%
  summarize(
    demande_totale_cluster_location = sum(demande_totale_location, na.rm = TRUE),
    disponibilite_moyenne_cluster_location = mean(disponibilite_moyenne_location, na.rm = TRUE),
    chiffre_affaire_cluster_location = sum(chiffre_affaire_location, na.rm = TRUE),  # Total du manque à gagner pour chaque cluster
    medications_concernes_cluster_location = paste(unique(medications), collapse = ", "),  # Médicaments concernés dans chaque cluster
    .groups = "drop"
  )

# Calcul des métriques par cluster géographique
metrics_by_cluster_location_availability <- medication_access_data %>%
  group_by(cluster_location_availability) %>%
  summarize(
    demande_totale_cluster_location_availability = sum(demande_totale_location_availability, na.rm = TRUE),
    disponibilite_moyenne_cluster_location_availability = mean(disponibilite_moyenne_location_availability, na.rm = TRUE),
    chiffre_affaire_cluster_location_availability = sum(chiffre_affaire_location_availability, na.rm = TRUE),  # Total du manque à gagner pour chaque cluster
    medications_concernes_cluster_location_availability = paste(unique(medications), collapse = ", "),  # Médicaments concernés dans chaque cluster
    .groups = "drop"
  )

# Ajouter les métriques aux données initiales
medication_access_data <- medication_access_data %>%
  left_join(metrics_by_cluster_location, by = "cluster_location")

# Ajouter les métriques aux données initiales pour location_availability
medication_access_data <- medication_access_data %>%
  left_join(metrics_by_cluster_location_availability, by = "cluster_location_availability")

# Affichage du dataframe final avec les métriques
print(head(medication_access_data))


# Fonction pour détecter les ruptures de stock
detecter_ruptures_stock <- function(stock_shortage, deadline_stock_shortage) {
  stock_shortage == "Oui" & !is.na(deadline_stock_shortage)
}


# Application de la fonction pour prédire les ruptures de stock
medication_access_data <- medication_access_data %>%
  mutate(predicted_stock_outage = detecter_ruptures_stock(stock_shortage, deadline_stock_shortage))

map_cluster_data_location <- leaflet(cluster_data_location) 

# Affichage des résultats sur une carte Leaflet
map_ruptureStock <- leaflet(medication_access_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~med_lon,
    lat = ~med_lat,
    radius = 5,
    color = ~ifelse(predicted_stock_outage, "red", "green"),
    fillOpacity = 0.8,
    popup = ~paste(
      "Localité:", location, "<br>",
      "Demande totale:", demande_totale_location, "<br>",
      "Disponibilité moyenne:", disponibilite_moyenne_location, "<br>",
      "Chiffre d'affaires:", chiffre_affaire_location, "FCFA", "<br>",
      "Médicaments concernés:", medications_concernes_location
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("red", "green"),
    labels = c("Rupture de stock", "Disponible"),
    title = "Statut des Médicaments"
  )

map_ruptureStock

# Barplot de la demande totale par cluster
barplot_location <- ggplot(cluster_data_location, aes(x = reorder(location, demande_totale_location), y = demande_totale_location)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +  # Ajuster la largeur des barres
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +  # Rotation et ajustement des étiquettes
  labs(
    title = "Demande Totale par Cluster Géographique",
    x = "Cluster Géographique",
    y = "Demande Totale"
  )
barplot_location

barplot_location_availability <- ggplot(cluster_data_location_availability, aes(x = reorder(location_availability, demande_totale_location), y = demande_totale_location_availability)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +  # Couleur personnalisée
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(
    title = "Demande Totale par Cluster Géographique",
    x = "Cluster Géographique",
    y = "Demande Totale"
  )
barplot_location_availability

# Filtrer ou préparer les données pour l'entraînement
# Filtrer ou préparer les données pour l'entraînement
medication_access_data1 <- medication_access_data %>%
  select(demande_totale_location, demande_totale_location_availability, medications, everything())

# Diviser les données en ensemble d'entraînement et de test
set.seed(123)
data_split <- initial_split(medication_access_data1, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Créer la recette de transformation des données
recipe <- recipe(demande_totale_location ~ ., data = train_data) %>%
  step_zv(all_predictors()) %>%  # Retirer les colonnes à variance nulle
  step_normalize(all_numeric_predictors()) %>%  # Normaliser les prédicteurs numériques
  step_dummy(all_nominal_predictors(), -all_outcomes())  # Encoder les colonnes catégoriques

# Définir les modèles
ridge_model <- linear_reg(penalty = 0.1, mixture = 0) %>% set_engine("glmnet") %>% set_mode("regression")
lasso_model <- linear_reg(penalty = 0.1, mixture = 1) %>% set_engine("glmnet") %>% set_mode("regression")
rf_model <- rand_forest(trees = 100, mtry = 3, min_n = 2) %>% set_engine("ranger") %>% set_mode("regression")
xgb_model <- boost_tree(trees = 100, tree_depth = 6, min_n = 2) %>% set_engine("xgboost") %>% set_mode("regression")

# Créer des workflows pour chaque modèle
ridge_workflow <- workflow() %>% add_recipe(recipe) %>% add_model(ridge_model)
lasso_workflow <- workflow() %>% add_recipe(recipe) %>% add_model(lasso_model)
rf_workflow <- workflow() %>% add_recipe(recipe) %>% add_model(rf_model)
xgb_workflow <- workflow() %>% add_recipe(recipe) %>% add_model(xgb_model)

# Entraîner les modèles
ridge_fit <- fit(ridge_workflow, data = train_data)
lasso_fit <- fit(lasso_workflow, data = train_data)
rf_fit <- fit(rf_workflow, data = train_data)
xgb_fit <- fit(xgb_workflow, data = train_data)

# Prédictions sur les données de test
ridge_pred <- predict(ridge_fit, new_data = test_data) %>% bind_cols(test_data)
lasso_pred <- predict(lasso_fit, new_data = test_data) %>% bind_cols(test_data)
rf_pred <- predict(rf_fit, new_data = test_data) %>% bind_cols(test_data)
xgb_pred <- predict(xgb_fit, new_data = test_data) %>% bind_cols(test_data)

# Calculer les métriques de performance (RMSE, MAE)
ridge_rmse <- rmse(ridge_pred, truth = demande_totale_location, estimate = .pred)
lasso_rmse <- rmse(lasso_pred, truth = demande_totale_location, estimate = .pred)
rf_rmse <- rmse(rf_pred, truth = demande_totale_location, estimate = .pred)
xgb_rmse <- rmse(xgb_pred, truth = demande_totale_location, estimate = .pred)

ridge_mae <- mae(ridge_pred, truth = demande_totale_location, estimate = .pred)
lasso_mae <- mae(lasso_pred, truth = demande_totale_location, estimate = .pred)
rf_mae <- mae(rf_pred, truth = demande_totale_location, estimate = .pred)
xgb_mae <- mae(xgb_pred, truth = demande_totale_location, estimate = .pred)

# Organiser les résultats dans un tibble
results <- tibble(
  Model = c("Ridge", "Lasso", "Random Forest", "XGBoost"),
  RMSE = c(ridge_rmse$.estimate, lasso_rmse$.estimate, rf_rmse$.estimate, xgb_rmse$.estimate),
  MAE = c(ridge_mae$.estimate, lasso_mae$.estimate, rf_mae$.estimate, xgb_mae$.estimate)
)

# Trouver le meilleur modèle en fonction du RMSE et du MAE
best_model_rmse <- results %>%
  filter(RMSE == min(RMSE)) %>%
  slice(1)

best_model_mae <- results %>%
  filter(MAE == min(MAE)) %>%
  slice(1)

# Afficher le meilleur modèle en fonction du RMSE
cat("Le meilleur modèle en fonction du RMSE est :\n")
print(best_model_rmse)

# Afficher le meilleur modèle en fonction du MAE
cat("\nLe meilleur modèle en fonction du MAE est :\n")
print(best_model_mae)

# Afficher la comparaison des résultats
cat("\nComparaison des modèles (RMSE et MAE) :\n")
print(results)
