library(httr2)
library(tibble)

# Définition de l'URL de l'API
url <- "https://api.open-meteo.com/v1/forecast"

# Requête à l'API
response <- request(url) |>
  req_url_query(latitude = 48.85,
                longitude = 2.35,
                hourly = c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"),
                .multi = "comma") |>
  req_perform() |>
  resp_body_json()

# Convertir en tibble
weather_data <- as_tibble(response)

"Suite à notre requête, nous avons obtenu un ensemble de données météorologiques sous forme de tibble. Voici une brève description des colonnes présentes dans le tibble weather_data :

latitude et longitude: les coordonnées géographiques pour lesquelles les prévisions météorologiques ont été demandées.
generationtime_ms: le temps de génération des données en millisecondes.
utc_offset_seconds: le décalage horaire en secondes par rapport au temps universel coordonné (UTC).
timezone et timezone_abbreviation: l'heure locale et son abréviation.
elevation: l'altitude du site pour lequel les prévisions sont demandées.
hourly_units et hourly: ces colonnes contiennent des données sur les prévisions horaires, mais elles sont sous forme de listes.
Pour récupérer les prévisions météo pour tous les sites des Jeux olympiques, nous devrons modifier les coordonnées géographiques spécifiées dans la requête. En particulier, nous devrons fournir les coordonnées de chaque site des Jeux olympiques au lieu de coordonnées spécifiques comme c(48.85, 2.35) pour Paris.
"


perform_request <- function(latitude, longitude) {
  library(httr2)
  library(tibble)

  # URL de l'API
  url <- "https://api.open-meteo.com/v1/forecast"

  # Requête à l'API avec les coordonnées GPS
  response <- request(url) |>
    req_url_query(latitude = latitude,
                  longitude = longitude,
                  hourly = c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"),
                  .multi = "comma") |>
    req_perform() |>
    resp_body_json()

  # Convertir le contenu en tibble et retourner
  as_tibble(response)
}


unnest_response <- function(data) {
  # Renommer les colonnes
  colnames(data) <- c("date_heure", "temperature_celsius", "temperature_ressentie_celsius", "precipitation_proba", "precipitation")

  # Retourner la tibble transformée
  return(data)
}



# Créer un script de tests
usethis::use_test("unnest_response")

# Charger les packages nécessaires pour les tests
library(testthat)
library(tibble)

# Définir les tests
test_that("unnest_response function works correctly", {
  # Créer un jeu de données minimal pour tester la fonction
  input_data <- tibble(
    date_heure = "2024-03-04T00:00",
    temperature_celsius = 20,
    temperature_ressentie_celsius = 18,
    precipitation_proba = 0.2,
    precipitation = 0
  )

  # Appeler la fonction unnest_response avec les données d'entrée
  output_data <- unnest_response(input_data)

  # Tester le nombre de lignes en sortie
  expect_equal(nrow(output_data), 1)

  # Tester les valeurs de la colonne temperature
  expect_equal(output_data$temperature_celsius, 20)

  # Tester le nom des colonnes en sortie
  expect_true("date_heure" %in% colnames(output_data))
  expect_true("temperature_celsius" %in% colnames(output_data))
  expect_true("temperature_ressentie_celsius" %in% colnames(output_data))
  expect_true("precipitation_proba" %in% colnames(output_data))
  expect_true("precipitation" %in% colnames(output_data))

  # Tester le nombre de colonnes en sortie
  expect_equal(ncol(output_data), 5)
})



library(tidygeocoder)

# Définir une fonction pour obtenir les coordonnées GPS à partir d'une adresse
address_to_gps <- function(address) {
  # Utiliser reverse_geocode pour obtenir les coordonnées GPS de l'adresse
  result <- reverse_geocode(address)

  # Extraire les coordonnées GPS
  gps <- c(result$longitude, result$latitude)

  return(gps)
}

# Définir une fonction générique pour obtenir les prévisions météorologiques
get_forecast <- function(location) {
  UseMethod("get_forecast")
}

# Implémentation de get_forecast pour les adresses sous forme de caractère
get_forecast.character <- function(address) {
  # Vérifier si address est de type character et de taille 1
  if (!is.character(address) || length(address) != 1) {
    stop("L'adresse doit être fournie sous forme d'une chaîne de caractères de taille 1.")
  }

  # Appeler address_to_gps pour obtenir les coordonnées GPS
  xy <- address_to_gps(address)

  # Appeler get_forecast.numeric avec les coordonnées GPS
  get_forecast.numeric(xy)
}

# Implémentation de get_forecast pour les coordonnées GPS sous forme de vecteur numérique
get_forecast.numeric <- function(xy) {
  # Vérifier si xy est un vecteur numérique de taille 2
  if (!is.numeric(xy) || length(xy) != 2) {
    stop("Les coordonnées doivent être fournies sous forme d'un vecteur numérique de taille 2.")
  }

  # Appeler perform_request avec les coordonnées xy
  weather_data <- perform_request(xy[1], xy[2])

  # Transformer les données
  formatted_data <- unnest_response(weather_data)

  return(formatted_data)
}


get_gps_coordinate <- function(address) {
  # Utiliser reverse_geocode pour obtenir les coordonnées GPS de l'adresse
  result <- reverse_geocode(address)

  # Extraire les coordonnées GPS
  gps <- c(result$longitude, result$latitude)

  return(gps)
}



get_forecast.numeric <- function(xy) {
  # Vérifier si xy est un vecteur numérique de taille 2
  if (!is.numeric(xy) || length(xy) != 2) {
    stop("Les coordonnées doivent être fournies sous forme d'un vecteur numérique de taille 2.")
  }

  # Appeler perform_request avec les coordonnées xy
  weather_data <- perform_request(xy[1], xy[2])

  # Transformer les données
  formatted_data <- unnest_response(weather_data)

  return(formatted_data)
}

get_forecast.character <- function(address) {
  # Vérifier si address est de type character et de taille 1
  if (!is.character(address) || length(address) != 1) {
    stop("L'adresse doit être fournie sous forme d'une chaîne de caractères de taille 1.")
  }

  # Appeler address_to_gps pour obtenir les coordonnées GPS
  xy <- address_to_gps(address)

  # Appeler get_forecast.numeric avec les coordonnées GPS
  forecast_data <- get_forecast.numeric(xy)

  return(forecast_data)
}



#' Obtenir les prévisions météorologiques à partir de coordonnées GPS ou d'une adresse.
#'
#' Cette fonction permet d'obtenir les prévisions météorologiques en fonction des coordonnées GPS
#' fournies ou d'une adresse spécifiée. Elle utilise les coordonnées GPS pour appeler la fonction
#' perform_request et obtenir les données de prévision météorologique, ou elle convertit l'adresse
#' en coordonnées GPS en utilisant la fonction address_to_gps avant d'appeler perform_request.
#'
#' @param location Coordonnées GPS sous forme d'un vecteur numérique de taille 2 (latitude, longitude) ou une adresse de type character de taille 1.
#' @return Une tibble contenant les données de prévision météorologique.
#' @export
#'
#' @examples
#' # Obtenir les prévisions météorologiques pour les coordonnées GPS (48.85, 2.35)
#' get_forecast(c(48.85, 2.35))
#'
#' # Obtenir les prévisions météorologiques pour l'adresse "Paris, France"
#' get_forecast("Paris, France")
get_forecast <- function(location) {
  UseMethod("get_forecast")
}

"library(usethis)
use_description()
library(devtools)
document()
"





visualize_forecast <- function(forecast_data) {
  # Créer un graphique de ligne pour la température et la probabilité de précipitations
  plot <- ggplot(forecast_data, aes(x = date_heure)) +
    geom_line(aes(y = temperature_celsius, color = "Température")) +
    geom_line(aes(y = precipitation_proba, color = "Probabilité de précipitations")) +
    labs(x = "Date et heure", y = "Valeur", color = "Variable") +
    theme_minimal() +
    theme(legend.position = "top")

  # Afficher le graphique
  print(plot)
}

