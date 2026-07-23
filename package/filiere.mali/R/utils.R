#' Utilitaires de chargement et sauvegarde
#'
#' Fonctions pratiques pour charger et sauvegarder des données au format
#' Stata (.dta) et RDS.
#'
#' @param folder Dossier contenant le fichier.
#' @param filename Nom du fichier.
#' @param object Objet R à sauvegarder.
#' @param path Chemin du dossier à créer si nécessaire.
#' @param file_path Chemin complet du fichier à vérifier.
#' @param x Vecteur numérique.
#' @param probs Vecteur de deux probabilités pour la winsorisation.
#' @param num,den Numérateur et dénominateur pour une division sécurisée.
#'
#' @name utils
NULL

#' @rdname utils
#' @export
create_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

#' @rdname utils
#' @export
load_dta <- function(folder, filename) {
  full_path <- file.path(folder, filename)
  if (!file.exists(full_path)) stop("Fichier introuvable : ", full_path)
  haven::read_dta(full_path)
}

#' @rdname utils
#' @export
save_rds <- function(object, folder, filename) {
  create_dir(folder)
  full_path <- file.path(folder, filename)
  saveRDS(object, full_path)
  message("Sauvegarde : ", full_path)
}

#' @rdname utils
#' @export
load_rds <- function(folder, filename) {
  full_path <- file.path(folder, filename)
  if (!file.exists(full_path)) stop("Fichier introuvable : ", full_path)
  readRDS(full_path)
}

#' @rdname utils
#' @export
check_file <- function(file_path) {
  if (!file.exists(file_path)) {
    warning("Fichier manquant : ", file_path)
    return(FALSE)
  }
  TRUE
}

#' @rdname utils
#' @export
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

#' @rdname utils
#' @export
safe_divide <- function(num, den) {
  ifelse(den != 0, num / den, NA_real_)
}