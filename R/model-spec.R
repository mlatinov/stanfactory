#' Declare what a Stan model exposes (the sidecar)
#'
#' A `model_spec` is the interface between your Stan model and the stanfactory
#' factories. The factories never parse Stan code; they trust this declaration.
#' It records the generated-quantity names your model emits, so the reporting
#' layer can dispatch to the appropriate `stanviz` functions: a populated
#' `pit` field enables `plot_ppc_pit()`, populated `arms` enable
#' `extract_ate_from_draws()` + `plot_ate_posterior()`, populated `groups`
#' enable the hierarchical suite, and so on.
#'
#' The same names are what you would otherwise pass to `stanviz` at every call
#' site; declaring them once here removes that repetition and lets the report
#' template light up sections polymorphically.
#'
#' @param model_id Character scalar. Stable identifier for this model. Used as
#'   the spine of every derived target name, so keep it short and unique
#'   (e.g. `"m3"`, `"hier_v2"`).
#' @param outcome Character scalar. Name of the observed outcome in your data
#'   list (e.g. `"y"`). Used by PPC plots as the `y` argument.
#' @param y_rep Character scalar. Generated-quantity name for posterior
#'   predictive draws. Default `"y_rep"`. Set `NULL` to disable PPC dispatch.
#' @param log_lik Character scalar or `NULL`. Pointwise log-likelihood name for
#'   LOO. Default `"log_lik"`. `NULL` disables LOO / out-of-sample dispatch.
#' @param pit Character scalar or `NULL`. PIT quantity name. `NULL` (default)
#'   disables PIT calibration dispatch.
#' @param lprior Character scalar or `NULL`. Log-prior name enabling priorsense
#'   power-scaling. `NULL` (default) disables prior-sensitivity dispatch.
#' @param estimands Named character vector mapping short estimand ids to their
#'   generated-quantity names, e.g. `c(ate = "est_ate", cate = "est_cate")`.
#'   The names are the ids used in target naming; the values are the Stan
#'   variable names. Empty by default.
#' @param arms Named character vector of per-arm prediction quantities, e.g.
#'   `c(treated = "mu_treated", control = "mu_control")`. Enables
#'   counterfactual / ATE-from-arms dispatch. `NULL` by default.
#' @param groups Named list describing hierarchical structure with elements
#'   `effects` (group-effect vector name), `grand` (grand-mean name), and
#'   `sd` (group SD name). `NULL` by default; enables the hierarchical suite.
#' @param data_vars Optional character vector of expected keys in the data
#'   list. When supplied, factories assert a data list carries exactly these
#'   keys before fitting, catching shape mismatches at build time.
#'
#' @return An object of class `model_spec`.
#' @export
#'
#' @examples
#' model_spec(
#'   model_id  = "m3",
#'   outcome   = "y",
#'   estimands = c(ate = "est_ate", cate = "est_cate"),
#'   arms      = c(treated = "mu_treated", control = "mu_control")
#' )
model_spec <- function(model_id,
                       outcome,
                       y_rep = "y_rep",
                       log_lik = "log_lik",
                       pit = NULL,
                       lprior = NULL,
                       estimands = character(0),
                       arms = NULL,
                       groups = NULL,
                       data_vars = NULL) {

  spec <- structure(
    list(
      model_id  = model_id,
      outcome   = outcome,
      y_rep     = y_rep,
      log_lik   = log_lik,
      pit       = pit,
      lprior    = lprior,
      estimands = estimands,
      arms      = arms,
      groups    = groups,
      data_vars = data_vars
    ),
    class = "model_spec"
  )

  validate_model_spec(spec)
}

#' Validate a model_spec
#'
#' @param spec A `model_spec` object.
#' @return The validated `spec`, invisibly on success; errors otherwise.
#' @export
validate_model_spec <- function(spec) {
  if (!inherits(spec, "model_spec")) {
    cli::cli_abort("{.arg spec} must be a {.cls model_spec}.")
  }

  if (!is_string(spec$model_id)) {
    cli::cli_abort("{.field model_id} must be a single non-empty string.")
  }
  if (!is_string(spec$outcome)) {
    cli::cli_abort("{.field outcome} must be a single non-empty string.")
  }

  # optional single-string fields
  for (f in c("y_rep", "log_lik", "pit", "lprior")) {
    v <- spec[[f]]
    if (!is.null(v) && !is_string(v)) {
      cli::cli_abort("{.field {f}} must be a single string or {.code NULL}.")
    }
  }

  # estimands: named character vector
  est <- spec$estimands
  if (length(est)) {
    if (!is.character(est) || is.null(names(est)) || any(!nzchar(names(est)))) {
      cli::cli_abort(
        "{.field estimands} must be a named character vector, e.g. \\
         {.code c(ate = \"est_ate\")}."
      )
    }
  }

  # arms: named character vector
  if (!is.null(spec$arms)) {
    arms <- spec$arms
    if (!is.character(arms) || is.null(names(arms)) || any(!nzchar(names(arms)))) {
      cli::cli_abort("{.field arms} must be a named character vector or {.code NULL}.")
    }
  }

  # groups: named list with the expected slots
  if (!is.null(spec$groups)) {
    g <- spec$groups
    need <- c("effects", "grand", "sd")
    if (!is.list(g) || !all(need %in% names(g))) {
      cli::cli_abort(
        "{.field groups} must be a list with elements \\
         {.val effects}, {.val grand}, and {.val sd}."
      )
    }
  }

  if (!is.null(spec$data_vars) &&
      (!is.character(spec$data_vars) || !length(spec$data_vars))) {
    cli::cli_abort("{.field data_vars} must be a non-empty character vector or {.code NULL}.")
  }

  invisible(spec)
}

#' @export
print.model_spec <- function(x, ...) {
  cli::cli_h2("model_spec: {.strong {x$model_id}}")
  cli::cli_text("outcome: {.val {x$outcome}}")

  enabled <- character(0)
  if (!is.null(x$y_rep))   enabled <- c(enabled, "PPC")
  if (!is.null(x$log_lik)) enabled <- c(enabled, "LOO")
  if (!is.null(x$pit))     enabled <- c(enabled, "PIT")
  if (!is.null(x$lprior))  enabled <- c(enabled, "prior-sensitivity")
  if (!is.null(x$arms))    enabled <- c(enabled, "ATE/counterfactual")
  if (!is.null(x$groups))  enabled <- c(enabled, "hierarchical")
  if (length(enabled)) {
    cli::cli_text("dispatch enabled: {.field {enabled}}")
  }

  if (length(x$estimands)) {
    cli::cli_text("estimands:")
    for (nm in names(x$estimands)) {
      cli::cli_li("{.field {nm}} <- {.val {x$estimands[[nm]]}}")
    }
  }
  invisible(x)
}
