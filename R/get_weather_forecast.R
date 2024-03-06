library(httr2)
library(tibble)

# Définition de l'URL de l'API
url <- "https://api.open-meteo.com/v1/forecast"

# Requête à l'API
response <- request(url) |>
  req_url_query(latitude = 48.85,
                longitude = 2.35,
                hourly = c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"),
                .multi = "comma"  ) |>
  req_perform() |>
  resp_body_json()

# Convertir en tibble et afficher
weather_data <- as_tibble(response)
View(weather_data)




perform_request <- function(lat, lon) {
  library(httr2)
  library(tibble)

  # Définition de l'URL de l'API
  url <- "https://api.open-meteo.com/v1/forecast"

  # Requête à l'API
  response <- request(url) |>
    req_url_query(latitude = lat,
                  longitude = lon,
                  hourly = c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"),
                  .multi = "comma"  ) |>
    req_perform() |>
    resp_body_json()

  # Convertir en tibble et retourner
  as_tibble(response)
  return(response)
}




library(dplyr)

unnest_response <- function(data) {
  # Renommer les colonnes
  colnames(data) <- c("date_heure", "temperature_celsius", "temperature_ressentie_celsius", "precipitation_proba", "precipitation")

  # Retourner la tibble transformée
  return(data)
}





library(tidygeocoder)

address_to_gps <- function(address) {
  # Utiliser reverse_geocode pour obtenir les coordonnées GPS de l'adresse
  result <- reverse_geocode(address)

  # Extraire les coordonnées GPS
  gps <- c(result$longitude, result$latitude)

  return(gps)
}
get_forecast <- function(location) {
  UseMethod("get_forecast")
}

get_forecast.character <- function(location) {
  # Convertir l'adresse en coordonnées GPS
  gps <- address_to_gps(location)

  # Appeler la fonction get_forecast.numeric avec les coordonnées GPS
  get_forecast(gps)
}

get_forecast.numeric <- function(location) {
  # Appeler la fonction perform_request avec les coordonnées GPS
  perform_request(location[2], location[1])
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

#' @rdname get_forecast
#' @export
get_forecast.character <- function(address) {
  if (!is.character(address) || length(address) != 1) {
    stop("L'adresse doit être fournie sous forme d'une chaîne de caractères de taille 1.")
  }
  xy <- address_to_gps(address)
  get_forecast.numeric(xy)
}

#' @rdname get_forecast
#' @export
get_forecast.numeric <- function(xy) {
  if (!is.numeric(xy) || length(xy) != 2) {
    stop("Les coordonnées doivent être fournies sous forme d'un vecteur numérique de taille 2.")
  }
  perform_request(xy[1], xy[2]) |> unnest_response()
}



"# Assurez-vous que devtools est chargé
library(devtools)

# Mettez à jour la documentation et le NAMESPACE
document()

# Mettez à jour le fichier DESCRIPTION
use_description()"



#' Visualiser les prévisions météorologiques sous forme de graphique de ligne
#' @param forecast_data Une tibble contenant les données de prévision météorologique.
#' @export
#' @import ggplot2
#' @examples
#' # Visualiser les prévisions météorologiques
#' visualize_forecast(forecast_data)
visualize_forecast <- function(forecast_data) {
  # Créer un graphique de ligne pour la température et la température ressentie
  plot <- ggplot(forecast_data, aes(x = date_heure)) +
    geom_line(aes(y = temperature_celsius, color = "Température")) +
    geom_line(aes(y = temperature_ressentie_celsius, color = "Température ressentie")) +
    labs(x = "Date et heure", y = "Température (°C)", color = "Variable") +
    theme_minimal() +
    theme(legend.position = "top")

  # Afficher le graphique
  print(plot)
}



