#' Default diagnostic thresholds
#'
#' The cutoffs used to turn raw sampler diagnostics into a green / amber / red
#' status. Exposed as a function so you can override per call (e.g. relax ESS
#' during rapid iteration). Values follow common Stan-workflow guidance.
#'
#' @param rhat_amber,rhat_red R-hat above these flags amber / red.
#' @param ess_bulk_amber,ess_bulk_red Bulk-ESS below these flags amber / red.
#' @param ess_tail_amber,ess_tail_red Tail-ESS below these flags amber / red.
#' @param divergences_red Any divergent transitions at or above this count is
#'   red (default 1: a single divergence is treated as red, as is standard).
#'
#' @return A named list of thresholds.
#' @export
bayes_thresholds <- function(rhat_amber = 1.01,
                             rhat_red = 1.05,
                             ess_bulk_amber = 400,
                             ess_bulk_red = 100,
                             ess_tail_amber = 400,
                             ess_tail_red = 100,
                             divergences_red = 1) {
  list(
    rhat_amber = rhat_amber,
    rhat_red = rhat_red,
    ess_bulk_amber = ess_bulk_amber,
    ess_bulk_red = ess_bulk_red,
    ess_tail_amber = ess_tail_amber,
    ess_tail_red = ess_tail_red,
    divergences_red = divergences_red
  )
}

#' Score convergence diagnostics into a one-row verdict
#'
#' Takes the worst-case scalars from a diagnostics table (plus a divergence
#' count) and reduces them to a single green / amber / red status with the
#' contributing reasons. This is the engine behind the cheap glance for
#' `tar_bayes_check()`.
#'
#' @param model_id Character scalar.
#' @param max_rhat Numeric scalar: the largest R-hat across monitored params.
#' @param min_ess_bulk Numeric scalar: the smallest bulk-ESS.
#' @param min_ess_tail Numeric scalar: the smallest tail-ESS.
#' @param n_divergences Integer scalar: number of divergent transitions.
#' @param thresholds A list from [bayes_thresholds()].
#'
#' @return A one-row data frame with `model_id`, `kind`, `status`, the scalar
#'   diagnostics, and a `reasons` string.
#' @export
score_convergence <- function(model_id,
                              max_rhat,
                              min_ess_bulk,
                              min_ess_tail,
                              n_divergences = 0,
                              thresholds = bayes_thresholds()) {
  th <- thresholds
  reasons <- character(0)
  level <- 0L # 0 green, 1 amber, 2 red

  bump <- function(level, to, msg) {
    if (to > level) level <- to
    if (nzchar(msg)) reasons[[length(reasons) + 1L]] <<- msg
    level
  }

  if (!is.na(max_rhat)) {
    if (max_rhat >= th$rhat_red) {
      level <- bump(level, 2L, sprintf("max R-hat %.3f >= %.2f", max_rhat, th$rhat_red))
    } else if (max_rhat >= th$rhat_amber) {
      level <- bump(level, 1L, sprintf("max R-hat %.3f >= %.2f", max_rhat, th$rhat_amber))
    }
  }
  if (!is.na(min_ess_bulk)) {
    if (min_ess_bulk < th$ess_bulk_red) {
      level <- bump(level, 2L, sprintf("min bulk-ESS %.0f < %d", min_ess_bulk, th$ess_bulk_red))
    } else if (min_ess_bulk < th$ess_bulk_amber) {
      level <- bump(level, 1L, sprintf("min bulk-ESS %.0f < %d", min_ess_bulk, th$ess_bulk_amber))
    }
  }
  if (!is.na(min_ess_tail)) {
    if (min_ess_tail < th$ess_tail_red) {
      level <- bump(level, 2L, sprintf("min tail-ESS %.0f < %d", min_ess_tail, th$ess_tail_red))
    } else if (min_ess_tail < th$ess_tail_amber) {
      level <- bump(level, 1L, sprintf("min tail-ESS %.0f < %d", min_ess_tail, th$ess_tail_amber))
    }
  }
  if (!is.na(n_divergences) && n_divergences >= th$divergences_red) {
    level <- bump(level, 2L, sprintf("%d divergent transition%s", n_divergences,
                                     if (n_divergences == 1L) "" else "s"))
  }

  status <- c("green", "amber", "red")[level + 1L]

  data.frame(
    model_id = model_id,
    kind = "check",
    status = status,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    n_divergences = n_divergences,
    reasons = if (length(reasons)) paste(reasons, collapse = "; ") else NA_character_,
    stringsAsFactors = FALSE
  )
}
