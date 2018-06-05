#' Bayesian Hierarchical Model for Meta-Analysis
#'
#' Bayesian hierarchical model used here to answer the question does the
#' compiled literature provide evidence for the effectiveness of the
#' treatment.
#'
#' @param object A data frame containing the 'study' column, the 'yi' observed effect column ,  and at least one of the columns 'vi' variance or ; 'sei' standard error.
#' @param ... Additonal arguments to pass to [brms::brm()]
#'  such as `set_prior`, `seed`, `warmup`, etc.
#' @return An object of class `ic_meta`.
#' @details These functions can be used to perform a Meta-analysis via
#' Bayesian multilevel modeling.
#'
#' By default, a generalized linear model with Gaussian error and
#'  an identity link is fit to the data.
#' @examples
#'
#'
#'
#' # Example objects from the "Getting Started" vignette at
#' #  https://topepo.github.io/tidyposterior/articles/Getting_Started.html
#'
#' file <- system.file("examples", "roc_model.RData", package = "tidyposterior")
#' load(file)
#'
#' roc_model
#'
#' # Summary method shows the underlying `stan` model
#' summary(roc_model)
#' @export
ic_meta <- function(object, ...)
  UseMethod("ic_meta")


#' @export
ic_meta.default <- function(object, ...)
  stop("`object` should have at least one of these classes: ",
       "'tibble', 'data.frame' ",
       "See ?ic_meta")

# Make a general data.frame method.

#' @rdname ic_meta
#' @param transform An named list of transformation and inverse
#'  transformation fuctions. See [logit_trans()] as an example.
#' @param hetero_var A logical; if `TRUE`, then different
#'  variances are estimated for each model group. Otherwise, the
#'  same variance is used for each group akinf to the fixed effect model in the
#'  frequentist literature.
#' @export
#' @importFrom dplyr filter select mutate %>%
#' @importFrom brms brm
#' @importFrom rlang !!
ic_meta.data.frame <-
  function(object, transform = no_trans, hetero_var = TRUE, ...) {
    #check_trans(transform)   Find a way to solve object 'no_trans' not found

    hmc_type <- try(pretty(object), silent = TRUE)
    if(inherits(hmc_type, "try-error"))
      hmc_type <- NA

    dat <- as.data.frame(object)

    ## Make a formula based on resampling type (repeatedcv, rof),
    ## This could be done with more specific classes

    study_names <- unique(as.character(dat$study))

    if (hetero_var) {

      mod <- brm(yi | se(sei) ~ 1 + (1|study),
                 data = dat, ...)
    } else {
      mod <- brm(yi | se(sei) ~ 1,
                 data = dat, ...)
    }

    res <- list(stan = mod,
                hetero_var = hetero_var,
                names = study_names,
                #transform = transform,
                hmc_type = hmc_type
                )
    class(res) <- "ic_meta"
    res
  }

# #' @importFrom utils globalVariables
# utils::globalVariables(c("study", "yi", "sei"))

#' @export
print.ic_meta <- function(x, ...) {
  cat("Bayesian Meta-Analysis\n")
  if(!is.na(x$hmc_type)) {
    cat("Original data: ")
    cat(x$hmc_type, sep = "\n")
  }
  cat("\n")
  invisible(x)
}

#' @export
summary.ic_meta <- function(object, ...) {
  summary(object$stan)
}



