#' stanfactory: targets factories for Bayesian Stan workflows
#'
#' stanfactory provides a small set of `targets` factory functions that
#' streamline a full Bayesian workflow around Stan models. Each factory emits a
#' list of targets and produces a [bayes_result()] object; the reporting layer
#' renders those results to Quarto. Plotting is delegated to stanviz.
#'
#' The foundation is three pieces: [bayes_result()] (the object every factory
#' emits), [model_spec()] (the sidecar declaring what a Stan model exposes), and
#' [bayes_target_name()] (the single source of truth for deterministic target
#' names).
#'
#' @keywords internal
"_PACKAGE"
