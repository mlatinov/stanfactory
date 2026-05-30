#' Derive a deterministic target name
#'
#' The single source of truth for how `targets` names are constructed across
#' every factory in stanfactory. Names derive deterministically from the
#' identity tuple so that (a) editing one model's Stan code invalidates only
#' that model's downstream targets, and (b) registering a new model variant
#' fans out predictably. Never paste target-name strings by hand elsewhere;
#' always route through this function.
#'
#' @param kind Character scalar. The factory / stage producing the target,
#'   e.g. `"fit"`, `"check"`, `"ppc"`, `"summ"`, `"prior"`, `"recov"`,
#'   `"stress"`, `"compare"`, `"report"`.
#' @param model_id Character scalar identifying the model. Required.
#' @param dataset_id Optional character scalar identifying the dataset
#'   (inference side).
#' @param estimand_id Optional character scalar identifying the estimand.
#' @param design_id Optional character scalar identifying a design point
#'   (validation side).
#' @param suffix Optional trailing tag (e.g. `"agg"`, `"verdict"`).
#'
#' @return A syntactically valid, unique target name (character scalar).
#' @export
#'
#' @examples
#' bayes_target_name("fit", model_id = "m3", dataset_id = "d1")
#' bayes_target_name("summ", model_id = "m3", dataset_id = "d1", estimand_id = "ate")
#' bayes_target_name("recov", model_id = "m3", design_id = "g07")
bayes_target_name <- function(kind,
                              model_id,
                              dataset_id = NULL,
                              estimand_id = NULL,
                              design_id = NULL,
                              suffix = NULL) {
  if (missing(kind) || !is_string(kind)) {
    cli::cli_abort("{.arg kind} must be a single non-empty string.")
  }
  if (missing(model_id) || !is_string(model_id)) {
    cli::cli_abort("{.arg model_id} must be a single non-empty string.")
  }

  parts <- c(
    kind,
    model_id,
    dataset_id,
    estimand_id,
    design_id,
    suffix
  )
  parts <- parts[!vapply(parts, is.null, logical(1))]

  # sanitise each component: allowed chars only, collapse the rest to "_"
  clean <- vapply(parts, sanitize_token, character(1))
  name <- paste(clean, collapse = "_")

  # targets requires names match ^[A-Za-z][A-Za-z0-9._]*$
  if (!grepl("^[A-Za-z]", name)) {
    name <- paste0("t_", name)
  }
  name
}

#' @keywords internal
#' @noRd
sanitize_token <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) {
    cli::cli_abort("A target-name component reduced to an empty string.")
  }
  x
}

#' @keywords internal
#' @noRd
is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}
