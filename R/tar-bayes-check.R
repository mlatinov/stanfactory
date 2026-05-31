#' Check a fitted model on real data (targets factory)
#'
#' Emits a list of `targets` targets that interrogate an **already-fitted**
#' model: convergence diagnostics, posterior predictive checks, out-of-sample
#' fit (LOO), and optional prior-sensitivity power-scaling. The factory never
#' fits anything — you own the fit target and its sampler settings, and hand
#' its name in here. Plotting is delegated to `stanviz`; this factory only
#' orchestrates, scores a verdict, and gathers everything into a single
#' [bayes_result()].
#'
#' This is the mode-(a) workhorse: edit a model's Stan code, `tar_make()`, and
#' only this model's check targets re-run. Glance at `verdict()`; open the
#' report only when it is not green.
#'
#' @param spec A [model_spec()] declaring what the model exposes. Drives which
#'   `stanviz` functions are dispatched.
#' @param fit A symbol or string naming the upstream fit target (the
#'   `CmdStanMCMC` / fit object). E.g. `fit = m3_fit` or `fit = "m3_fit"`.
#' @param data A symbol or string naming the upstream target holding the
#'   observed data list (used to pull the outcome for PPC).
#' @param prior_sensitivity Logical. If `TRUE` (default) and the spec declares
#'   an `lprior`, emit a priorsense power-scaling target. Set `FALSE` to skip
#'   during rapid iteration.
#' @param thresholds A list from [bayes_thresholds()] controlling verdict
#'   scoring.
#' @param pars Optional character vector of parameters to focus diagnostics /
#'   trace plots on. `NULL` uses the model's default monitored parameters.
#'
#' @return A list of `tar_target` objects. Splice it into your `_targets.R`
#'   target list.
#' @export
#'
#' @examples
#' \dontrun{
#' # in _targets.R
#' list(
#'   tar_target(m3_data, make_data()),
#'   tar_target(m3_fit, m3_model$sample(data = m3_data, adapt_delta = 0.99)),
#'   tar_bayes_check(spec = m3_spec, fit = m3_fit, data = m3_data)
#' )
#' }
tar_bayes_check <- function(spec,
                            fit,
                            data,
                            prior_sensitivity = TRUE,
                            thresholds = bayes_thresholds(),
                            pars = NULL) {
  validate_model_spec(spec)
  fit_name  <- as_target_name(rlang::enquo(fit), "fit")
  data_name <- as_target_name(rlang::enquo(data), "data")

  mid <- spec$model_id
  fit_sym  <- rlang::sym(fit_name)
  data_sym <- rlang::sym(data_name)

  nm <- function(k, suffix = NULL) {
    bayes_target_name(k, model_id = mid, suffix = suffix)
  }

  targets_list <- list()

  ## --- diagnostics: always ---
  diag_name <- nm("checkdiag")
  targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
    diag_name,
    rlang::expr(stanviz::diagnostic_summary(!!fit_sym, pars = !!pars))
  )

  ## --- ppc density (always, if y_rep declared) ---
  # Build the runtime expression data[["<outcome>"]] explicitly. Note: we must
  # construct the [[ ]] call rather than splicing data_sym[[spec$outcome]],
  # which would evaluate at factory-build time instead of run time.
  y_expr <- rlang::call2("[[", data_sym, spec$outcome)

  plot_names <- character(0)
  if (!is.null(spec$y_rep)) {
    ppc_dens_name <- nm("checkppcdens")
    targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
      ppc_dens_name,
      rlang::expr(stanviz::plot_ppc_dens(
        !!fit_sym,
        y = !!y_expr,
        yrep_var = !!spec$y_rep
      ))
    )
    plot_names <- c(plot_names, ppc_dens = ppc_dens_name)

    ppc_stat_name <- nm("checkppcstat")
    targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
      ppc_stat_name,
      rlang::expr(stanviz::plot_ppc_stat_grid(
        !!fit_sym,
        y = !!y_expr,
        yrep_var = !!spec$y_rep
      ))
    )
    plot_names <- c(plot_names, ppc_stat = ppc_stat_name)
  }

  ## --- PIT (if declared) ---
  if (!is.null(spec$pit)) {
    pit_name <- nm("checkpit")
    targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
      pit_name,
      rlang::expr(stanviz::plot_ppc_pit(!!fit_sym, pit_var = !!spec$pit))
    )
    plot_names <- c(plot_names, ppc_pit = pit_name)
  }

  ## --- LOO (if log_lik declared) ---
  loo_name <- NULL
  if (!is.null(spec$log_lik)) {
    loo_name <- nm("checkloo")
    targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
      loo_name,
      rlang::expr(stanviz::compute_loo(!!fit_sym, loglik_var = !!spec$log_lik))
    )
    pareto_name <- nm("checkpareto")
    targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
      pareto_name,
      rlang::expr(stanviz::plot_loo_pareto_k(!!rlang::sym(loo_name)))
    )
    plot_names <- c(plot_names, loo_pareto = pareto_name)
  }

  ## --- prior sensitivity (if lprior declared and requested) ---
  ps_name <- NULL
  if (isTRUE(prior_sensitivity) && !is.null(spec$lprior)) {
    ps_name <- nm("checkpowerscale")
    targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
      ps_name,
      rlang::expr(stanviz::powerscale_summary(!!fit_sym))
    )
  }

  ## --- verdict: score the diagnostics table ---
  verdict_name <- nm("checkverdict")
  targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
    verdict_name,
    rlang::expr(score_convergence_from_table(
      model_id = !!mid,
      diag = !!rlang::sym(diag_name),
      fit = !!fit_sym,
      thresholds = !!thresholds
    ))
  )

  ## --- terminal bayes_result: gather tables + plots + verdict ---
  # build a named list expression of plot targets
  plot_exprs <- lapply(unname(plot_names), rlang::sym)
  names(plot_exprs) <- names(plot_names)
  plots_call <- rlang::call2("list", !!!plot_exprs)

  table_syms <- list(diagnostics = rlang::sym(diag_name))
  if (!is.null(loo_name)) table_syms$loo <- rlang::sym(loo_name)
  if (!is.null(ps_name))  table_syms$powerscale <- rlang::sym(ps_name)
  tables_call <- rlang::call2("list", !!!table_syms)

  result_name <- nm("check")
  targets_list[[length(targets_list) + 1L]] <- targets::tar_target_raw(
    result_name,
    rlang::expr(bayes_result(
      model_id = !!mid,
      kind = "check",
      tables = !!tables_call,
      plots = !!plots_call,
      verdict = !!rlang::sym(verdict_name)
    ))
  )

  targets_list
}

#' Score convergence from a stanviz diagnostic table
#'
#' Run-time helper: pulls worst-case scalars out of a `diagnostic_summary()`
#' table and the divergence count from the fit, then calls [score_convergence()].
#' Kept separate so the factory can reference it by name in an emitted target.
#'
#' @param model_id Character scalar.
#' @param diag A data frame from `stanviz::diagnostic_summary()`.
#' @param fit The fit object (for divergence count).
#' @param thresholds A list from [bayes_thresholds()].
#' @return A one-row verdict data frame.
#' @export
score_convergence_from_table <- function(model_id, diag, fit, thresholds = bayes_thresholds()) {
  col <- function(df, candidates) {
    hit <- intersect(candidates, names(df))
    if (length(hit)) df[[hit[[1]]]] else NA_real_
  }
  max_rhat <- suppressWarnings(max(col(diag, c("rhat", "Rhat", "r_hat")), na.rm = TRUE))
  min_bulk <- suppressWarnings(min(col(diag, c("ess_bulk", "ess_bulk_ratio", "ess")), na.rm = TRUE))
  min_tail <- suppressWarnings(min(col(diag, c("ess_tail", "ess_tail_ratio")), na.rm = TRUE))
  if (!is.finite(max_rhat)) max_rhat <- NA_real_
  if (!is.finite(min_bulk)) min_bulk <- NA_real_
  if (!is.finite(min_tail)) min_tail <- NA_real_

  ndiv <- tryCatch(count_divergences(fit), error = function(e) NA_integer_)

  score_convergence(
    model_id = model_id,
    max_rhat = max_rhat,
    min_ess_bulk = min_bulk,
    min_ess_tail = min_tail,
    n_divergences = ndiv,
    thresholds = thresholds
  )
}

#' @keywords internal
#' @noRd
count_divergences <- function(fit) {
  # CmdStanR
  if (inherits(fit, "CmdStanMCMC")) {
    ds <- tryCatch(fit$sampler_diagnostics(), error = function(e) NULL)
    if (!is.null(ds)) {
      m <- posterior::as_draws_matrix(ds)
      if ("divergent__" %in% colnames(m)) return(as.integer(sum(m[, "divergent__"])))
    }
  }
  NA_integer_
}

#' @keywords internal
#' @noRd
as_target_name <- function(quo, arg) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr)) return(rlang::as_string(expr))
  if (is.character(expr) && length(expr) == 1L) return(expr)
  cli::cli_abort("{.arg {arg}} must be a target name (a bare symbol or a string).")
}