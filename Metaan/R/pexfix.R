#' @title  Pooled excess risk estimate using the fixed effect model meta-analysis
#' @description Fixed effect model for standard meta-analysis of excess relative risk (ERR) or excess odds ratio (EOR) estimates.
#'
#' @param err A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper confidence interval of the risk estimated from the individual studies.
#' @param l A numeric vector of the lower confidence interval of the risk estimated from the individual studies.
#' @param type Logical indicating the method to be used. The default is "excess" indicating that risk estimate model should be used.
#' @param test Logical indicating the statistical method to be used. The default if FIXE for the fixe effect model.
#'
#'
#'
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
#' "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
#' Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
#' 0.198, 1.012)
#' lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179,
#' 0.393, 0.078, 0.895)
#' upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312,
#' 0.996, 0.499, 1.145)
#'
#' donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))
#'
#' donne$Risk <- as.numeric(as.character(donne$Risk))
#' donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
#' donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
#'
#' pexfix(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
#' type = "excess", test = "FIXE")
#'
#'
#' @export
#'
#'
pexfix <- function(err, u, l,
                   type="excess", test="FIXE"){
  sd = (u-l)/(2*1.96)
  var = sd^2
  sum_num = sum(err/var, na.rm = T)
  sum_den = sum(1/var, na.rm = T)
  err_tot = sum_num/sum_den
  sd_tot = 1/(sqrt(sum_den))
  ret = as.data.frame(cbind(type="Standard approach wiht fixed effect model",
                            err_tot = err_tot,
                            sd_tot = sd_tot,
                            l_tot = err_tot - 1.96*sd_tot,
                            u_tot = err_tot + 1.96*sd_tot

  ))
  #class(ret) <- "metaan"
  ret
}


