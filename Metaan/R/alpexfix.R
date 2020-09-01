#'
#' @title  Pooled excess risk estimate using the alternative fixed effect model meta-analysis
#' @description Alternative fixed effect model for meta-analysis of excess relative risk (ERR) or excess odds ratio (EOR) estimates.
#'
#' @param err A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper confidence interval of the risk estimated from the individual studies.
#' @param l A numeric vector of the lower confidence interval of the risk estimated from the individual studies.
#' @param d A numeric vector of the maximum dose reported from the individual studies.
#' @param type Logical indicating the method to be used. The default is "excess" indicating that excess risk estimate model should be used.
#' @param test Logical indicating the statistical method to be used. The default is "FIXE" for the fixed effect model.
#'
#'
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm", "Madanapalle",
#' "UK", "South Africa", "Haiti", "Madras")
#' Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625, 0.198, 1.012)
#' lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393, 0.078, 0.895)
#' upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312, 0.996, 0.499, 1.145)
#' dose <- c(32.586, 15.257, 72.431, 6.528, 10.886, 11.659, 17.254, 20.312, 10.996, 30.499,
#' 41.145)
#'
#' donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci, dose))
#'
#' donne$Risk <- as.numeric(as.character(donne$Risk))
#' donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
#' donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
#' donne$dose <- as.numeric(as.character(donne$dose))
#'
#' alpexfix(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, d=donne$dose,
#' type = "excess", test = "FIXE")
#'
#'
#' @references
#' Richardson, D. B., Abalo, K., Bernier, M. O., Rage, E., Leuraud, K., Laurier, D., ... & Little, M. P. (2020). Meta-analysis of published excess relative risk estimates. Radiation and Environmental Biophysics, 1-11.
#'
#' @export
#'
alpexfix <- function(err, u, l, d,
                     type="excess", test="FIXE"){
  C = min(d, na.rm=T)
  A = log(C*err+1)
  sd_A = log((C*u+1)/(C*l+1))/(2*1.96)
  var_A = sd_A^2
  sum_num = sum(A/var_A, na.rm = T)
  sum_den = sum(1/var_A, na.rm = T)
  A_tot = sum_num/sum_den
  sd_Atot = 1/(sqrt(sum_den))
  ret = as.data.frame(cbind(type="Alternative proposed approach with fixed effect model",
                            err_tot = (exp(A_tot)-1)/C,
                            sd_tot_lnERR = sd_Atot,
                            l_tot = (exp(A_tot - 1.96*sd_Atot)-1)/C,
                            u_tot = (exp(A_tot + 1.96*sd_Atot)-1)/C

  ))
  #class(ret) <- "metaan"
  ret
}


