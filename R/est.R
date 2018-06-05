#' Estimate the standard error
#'
#' Extract relevant features from a glmnet cox fit object.
#'
#' @param level It's the level of the ci which is used for finding the quantile function
#'  from a t-distribution with df degrees of freedom. Default is CI 95% which correspond
#'   to perc .975 (Others ci may be used).  The quantile will be differently obtained
#'   if p_value is provided.
#' @param n number of patients
#' @param ul upper limit of confidence interval
#' @param ll lower limit of confidence interval
#' @param p It is assumed that p is equal to the upper bound, difference is required.
#' @param diff The difference
#' @return est_se
#' @keywords se
#' @author Carlos S Traynor
#' @export est_se

est_se <- function(n, level = .95 , ul, ll, diff, p = NA){
  df <- n -2   # This is  -2  because assumes that we are estimating two values one in treatment group and one in control group
  if(is.na(p)){
    perc <- 1-(1-level)/2 #Find percentile
    t_alpha <- qt(perc, df)  # Find the value of the t statistic for the CI
    se = (ul - ll) / (2 * t_alpha)
  } else{
    t_alpha <- qt(1- p/2, df)
    se = diff / t_alpha
  }
  return(se)
}
