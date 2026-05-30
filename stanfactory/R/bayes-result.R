#' The stanfactory result object
#'
#' Every factory emits exactly one `bayes_result` as its terminal target. It is
#' the contract that the reporting layer (`tar_bayes_report()`) and the
#' interactive glance (`verdict()`) consume. Crucially, computation and
#' rendering are separated: the result carries the computed tables, the named
#' `stanviz` plots, and a one-row verdict, so you can pull any plot at the REPL
#' (`res$plots$ppc_dens`) independently of whether a report was ever rendered.
#'
#' @param model_id Character scalar. The model this result pertains to. For
#'   `kind = "compare"` use a composite id (e.g. `"m1_m2_m3"`).
#' @param kind One of `"check"`, `"prior"`, `"inference"`, `"stress"`,
#'   `"compare"`.
#' @param tables Named list of data frames / tibbles (diagnostics, loo,
#'   powerscale, reduced scalar grids, ...). May be empty.
#' @param plots Named list of `ggplot` objects. May be empty (e.g. the
#'   simulation side stores scalars, not plots).
#' @param verdict A one-row data frame: the cheap health glance. Should carry
#'   at least `model_id`, `kind`, and a `status` column
#'   (`"green"`/`"amber"`/`"red"`/`"info"`).
#' @param meta Named list of free-form metadata (mode, grid dims, timings).
#'
#' @return An object of class `bayes_result`.
#' @export
bayes_result <- function(model_id,
                         kind = c("check", "prior", "inference", "stress", "compare"),
                         tables = list(),
                         plots = list(),
                         verdict = NULL,
                         meta = list()) {
  kind <- match.arg(kind)

  if (is.null(verdict)) {
    verdict <- tibble::tibble(
      model_id = model_id,
      kind     = kind,
      status   = "info"
    )
  }

  res <- structure(
    list(
      model_id = model_id,
      kind     = kind,
      tables   = tables,
      plots    = plots,
      verdict  = verdict,
      meta     = meta
    ),
    class = "bayes_result"
  )

  validate_bayes_result(res)
}

#' Validate a bayes_result
#' @param x A `bayes_result`.
#' @return `x` invisibly on success.
#' @export
validate_bayes_result <- function(x) {
  if (!inherits(x, "bayes_result")) {
    cli::cli_abort("{.arg x} must be a {.cls bayes_result}.")
  }
  if (!is_string(x$model_id)) {
    cli::cli_abort("{.field model_id} must be a single non-empty string.")
  }
  if (!is.list(x$tables) || (length(x$tables) && is.null(names(x$tables)))) {
    cli::cli_abort("{.field tables} must be a named list (possibly empty).")
  }
  if (!is.list(x$plots) || (length(x$plots) && is.null(names(x$plots)))) {
    cli::cli_abort("{.field plots} must be a named list (possibly empty).")
  }
  if (!is.data.frame(x$verdict) || nrow(x$verdict) != 1L) {
    cli::cli_abort("{.field verdict} must be a one-row data frame.")
  }
  if (!"status" %in% names(x$verdict)) {
    cli::cli_abort("{.field verdict} must contain a {.val status} column.")
  }
  invisible(x)
}

#' @export
print.bayes_result <- function(x, ...) {
  status <- x$verdict$status %||% "info"
  sym <- switch(status,
    green = cli::col_green(cli::symbol$tick),
    amber = cli::col_yellow("!"),
    red   = cli::col_red(cli::symbol$cross),
    cli::col_blue("i")
  )
  cli::cli_h2("{sym} bayes_result [{x$kind}] {.strong {x$model_id}}")

  # verdict line: show non-id columns compactly
  vc <- x$verdict[setdiff(names(x$verdict), c("model_id", "kind"))]
  if (ncol(vc)) {
    pairs <- vapply(names(vc), function(nm) {
      paste0(nm, "=", format(vc[[nm]][[1]], digits = 3))
    }, character(1))
    cli::cli_text("verdict: {paste(pairs, collapse = '  ')}")
  }

  if (length(x$tables)) {
    cli::cli_text("{length(x$tables)} table{?s}: {.field {names(x$tables)}}")
  }
  if (length(x$plots)) {
    cli::cli_text("{length(x$plots)} plot{?s}: {.field {names(x$plots)}}")
    cli::cli_text(cli::col_grey("access with $plots${.field <name>}"))
  } else {
    cli::cli_text(cli::col_grey("no plots stored (scalar-only result)"))
  }
  invisible(x)
}

#' The cheap health glance
#'
#' Returns the one-row verdict from a `bayes_result` (or row-binds several)
#' without rendering anything. This is the 80%-case loop: tweak a model,
#' rerun, glance at the verdict, move on if green.
#'
#' @param ... One or more `bayes_result` objects, or a single list of them.
#' @return A tibble of verdict rows, row-bound.
#' @export
verdict <- function(...) {
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1]]) &&
      !inherits(dots[[1]], "bayes_result")) {
    dots <- dots[[1]]
  }
  ok <- vapply(dots, inherits, logical(1), what = "bayes_result")
  if (!all(ok)) {
    cli::cli_abort("All inputs to {.fn verdict} must be {.cls bayes_result} objects.")
  }
  rows <- lapply(dots, function(r) r$verdict)
  rbind_fill(rows)
}

#' @keywords internal
#' @noRd
rbind_fill <- function(rows) {
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[all_cols]
  })
  do.call(rbind, rows)
}

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
