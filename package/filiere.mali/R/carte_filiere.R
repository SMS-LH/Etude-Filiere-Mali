#' Cartographier un indicateur de filière par région
#'
#' Produit une carte choroplèthe d'un indicateur agrégé par région, sur un fond
#' de carte fourni (objet sf). L'appariement se fait par le nom de région.
#' Les noms des régions sont affichés en leur centre.
#'
#' @param data Un data frame contenant l'indicateur par région et une colonne
#'   de nom de région correspondant au fond de carte.
#' @param fond Un objet sf (fond de carte des régions).
#' @param col_valeur Nom de la colonne de l'indicateur à cartographier.
#' @param col_region_data Nom de la colonne de nom de région dans \code{data}.
#' @param col_region_fond Nom de la colonne de nom de région dans \code{fond}
#'   (défaut "NAME_1").
#' @param couleur_basse,couleur_haute Couleurs des valeurs basses et hautes du
#'   dégradé (défaut bleu clair -> bleu foncé).
#' @param na_couleur Couleur des régions sans donnée (défaut "grey85").
#' @param titre,legende Titre de la carte et titre de la légende.
#' @param afficher_valeurs Si TRUE, affiche la valeur sous le nom de chaque
#'   région (défaut FALSE).
#'
#' @return Un objet ggplot (la carte).
#'
#' @export
carte_filiere <- function(data,
                          fond,
                          col_valeur,
                          col_region_data,
                          col_region_fond = "NAME_1",
                          couleur_basse = "#deebf7",
                          couleur_haute = "#08519c",
                          na_couleur = "grey85",
                          titre = "",
                          legende = "",
                          afficher_valeurs = FALSE) {

  by_vec <- stats::setNames(col_region_data, col_region_fond)
  carte_data <- dplyr::left_join(fond, data, by = by_vec)

  centro <- sf::st_centroid(carte_data)
  coords <- sf::st_coordinates(centro)
  centro$.lon <- coords[, 1]
  centro$.lat <- coords[, 2]
  centro$.nom <- centro[[col_region_fond]]
  if (afficher_valeurs) {
    val <- centro[[col_valeur]]
    centro$.label <- ifelse(is.na(val), centro$.nom,
                            paste0(centro$.nom, "\n", round(val)))
  } else {
    centro$.label <- centro$.nom
  }

  ggplot2::ggplot(carte_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[col_valeur]]),
                     color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient(low = couleur_basse, high = couleur_haute,
                                 na.value = na_couleur, name = legende) +
    ggplot2::geom_text(data = centro,
                       ggplot2::aes(x = .lon, y = .lat, label = .label),
                       size = 2.8, fontface = "bold", lineheight = 0.9) +
    ggplot2::labs(title = titre) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}
