#'
#' @title  Pooled excess risk estimate using the alternative fixed effect model meta-analysis
#' @description Fixed effect meta-analysis of excess relative risk (ERR) or excess odds ratio (EOR) estimates with Richardson et al 2020 alternative model.
#'
#' @param err A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk reported from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk reported from the individual studies.
#' @param d A numeric vector of the maximum dose reported from the individual studies.
#' @param type Logical indicating the method to be used. The default is "excess" indicating that excess risk estimate model should be used.
#' @param test Logical indicating the statistical method to be used. The default is "FIXED" for the fixed effect model.
#' @param conf.level Coverage for the confidence interval
#'
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#'
#' @return Object of class "metaan.arf". A list that print the output from the alpexfix function. The following could be found from the list :
#'  - err_tot (Effect): The pooled effect from excess relative risk (ERR) or excess odd ratio (EOR) estimates
#'  - sd_tot_lnERR (SE-Log(Effect)): The standard error of the  logarithm of the pooled effect (see reference Richardson et al 2020 for more details)
#'  - l_tot (Lower CI): The lower confidence interval bound of the pooled effect (err_tot)
#'  - u_tot (Upper CI): The upper confidence interval bound of the pooled effect (err_tot)
#'  - Cochrane_stat (Cochran’s Q statistic): The value of the Cochrane's statistic of inter-study heterogeneity
#'  - Degree_freedom (Degree of Freedom): The degree of freedom
#'  - p_value (P-Value): The p-value of the statistic of Cochrane
#'  - I_square (Higgins’ and Thompson’s I^2 (%)): I square value in percent (%) indicating the amount of the inter-study heterogeneity
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
#' type = "excess", test = "FIXE", conf.level=0.95)
#'
#'
#' @references
#' Richardson, D. B., Abalo, K., Bernier, M. O., Rage, E., Leuraud, K., Laurier, D., ... & Little, M. P. (2020). Meta-analysis of published excess relative risk estimates. Radiation and Environmental Biophysics, 1-11.
#'
#' @export
#'
alpexfix <- function(err, u, l, d,
                     type="excess", test="FIXED", conf.level=0.95){

  if (conf.level>1 & conf.level<100)
    conf.level<-conf.level/100

  z.alpha <- (-qnorm((1-conf.level)/2))

  C = min(d, na.rm=T)
  A = log(C*err+1)
  sd_A = log((C*u+1)/(C*l+1))/(2*z.alpha)
  var_A = sd_A^2
  sum_num = sum(A/var_A, na.rm = T)
  sum_den = sum(1/var_A, na.rm = T)
  A_tot = sum_num/sum_den
  Q = sum(((A - A_tot)/sd_A)^2)
  sd_Atot = 1/(sqrt(sum_den))

  # Compute heterogeneity

  k = length(A)
  df = k - 1
  I = max(0 , round((1 - (df/Q))*100, 2))


  # Compute the result

  ret = list(err_tot = round((exp(A_tot)-1) / C, 2),
             sd_tot_lnERR = round(sd_Atot, 2),
             l_tot = round((exp(A_tot - z.alpha*sd_Atot)-1)/C, 2),
             u_tot = round((exp(A_tot + z.alpha*sd_Atot)-1)/C, 2),
             Cochrane_stat = round(Q, 2),
             Degree_freedom = round(df, 2),
             p_value = round(stats::pchisq(Q, df, lower.tail = F), 2),
             I_square = I )

  class(ret) <- "metaan.arf"
  ret
}




#'
#' @title  Pooled excess risk estimate using the alternative fixed effect model meta-analysis
#' @description Alternative fixed effect model for meta-analysis of excess relative risk (ERR) or excess odds ratio (EOR) estimates.
#'
#'
#'
#' @param x Object of class metaan.arf
#' @param ... Other arguments
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#' @rdname metaan.arf
#'
#' @return
#' @export
#'
#'
#'
#'
print.metaan.arf <- function(x, ...){
  retmat_a = cbind(x$err_tot, x$sd_tot, x$l_tot, x$u_tot)

  retmat_b = cbind(x$Cochrane_stat, x$Degree_freedom, x$p_value)

  retmat_c = cbind(x$I_square)

  colnames(retmat_a) <- c("Effect", "SE-Log(Effect)", "Lower CI", "Upper CI")
  colnames(retmat_b) <- c("Cochran’s Q statistic", "Degree of Freedom", "P-Value")
  colnames(retmat_c) <- c("Higgins’ and Thompson’s I^2 (%)")

  rownames(retmat_a) <- " "
  rownames(retmat_b) <- " "
  rownames(retmat_c) <- " "

  if(any(is.na(x$sd_tot))) retmat_a = retmat_a[,-2, drop=FALSE]
  cat("                                                     \n")
  cat(" Alternative meta-analysis with random effect model  \n")
  cat("---------------------------------------------------- \n")
  cat("                                                     \n")
  printCoefmat(retmat_a)
  cat("                                                     \n")
  cat("---------------------------------------------------- \n")
  cat("                                                     \n")
  cat("              Test of heterogeneity  \n")
  cat("                                                     \n")
  printCoefmat(retmat_b)
  cat("                                                     \n")
  cat("---------------------------------------------------- \n")
  cat("                                                     \n")
  printCoefmat(retmat_c)
  cat("____________________________________________________ \n")
  cat("                                                     \n")

}
