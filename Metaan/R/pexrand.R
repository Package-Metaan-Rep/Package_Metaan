#' @title  Pooled excess risk estimate using the random effect model meta-analysis
#' @description DerSimonian and Laird-based Random effect model for standard meta-analysis of excess relative risk (ERR) or excess odds ratio (EOR) estimates.
#'
#' @param err A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk reported from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk reported from the individual studies.
#' @param type Logical indicating the method to be used. The default is "excess" indicating that risk estimate model should be used.
#' @param test Logical indicating the statistical method to be used. The default is "RANDOM" for the random effect model.
#' @param conf.level Coverage for confidence interval
#'
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#'
#' @return Object of class "metaan.erf". A list that print the output from the pexrand function. The following could be found from the list :
#'  - err_tot (Effect): The pooled effect from excess relative risk (ERR) or excess odd ratio (EOR) estimates
#'  - sd_tot (SE Effect): The standard error of the pooled effect (see reference Richardson et al 2020 for more details)
#'  - l_tot (Lower CI): The lower confidence interval bound of the pooled effect (err_tot)
#'  - u_tot (Upper CI): The upper confidence interval bound of the pooled effect (err_tot)
#'  - Cochrane_stat (Cochran’s Q statistic): The value of the Cochrane's statistic of inter-study heterogeneity
#'  - Degree_freedom (Degree of Freedom): The degree of freedom
#'  - p_value (P-Value): The p-value of the statistic of Cochrane
#'  - I_square (Higgins’ and Thompson’s I^2 (%)): I square value in percent (%) indicating the amount of the inter-study heterogeneity
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
#' pexrand(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
#' type = "excess", test = "RANDOM", conf.level=0.95)
#'
#' @references
#' DerSimonian R, Laird N (1986) Meta-analysis in clinical trials. Controlled clinical trials 7:177–188.
#'
#'
#' @export
#'
pexrand <- function(err, u, l,
                    type="excess", test="RANDOM", conf.level=0.95){

  if (conf.level>1 & conf.level<100)
    conf.level<-conf.level/100

  z.alpha <- (-qnorm((1-conf.level)/2))

  sd = (u-l)/(2*z.alpha)
  var = sd^2
  err_tot1 = sum(err/var, na.rm = T)/sum(1/var, na.rm = T)
  q = sum(((err - err_tot1)/sd)^2)
  sum_num_rand = sum(1/sd^4, na.rm = T)
  sum_den_rand = sum(1/var, na.rm = T)
  nstudies = length(err)
  deltasq = max(0, (q - (nstudies-1)) / (sum_den_rand - (sum_num_rand/sum_den_rand)))
  sum_num = sum(err/(var + deltasq))
  sum_den = sum(1/(var + deltasq))
  err_tot = sum_num/sum_den
  sd_tot = 1/sqrt(sum_den)


  # Compute the heterogeneity

  err_tot1 = sum(err/var, na.rm = T)/sum(1/var, na.rm = T)
  Q = round(sum(((err - err_tot1)/sd)^2, na.rm = T), 2)
  k = length(err)
  df = k - 1
  I = max(0 , round((1 - (df/Q))*100, 2))

  # Compute the result

  ret = list(err_tot = round(err_tot , 2),
             sd_tot = round(sd_tot, 2),
             l_tot = round(err_tot - z.alpha*sd_tot, 2),
             u_tot = round(err_tot + z.alpha*sd_tot, 2),
             Cochrane_stat = round(Q, 2),
             Degree_freedom = round(df, 2),
             p_value = round(stats::pchisq(Q, df, lower.tail = F), 2),
             I_square = I )

  class(ret) <- "metaan.erf"
  ret

}










#' @title  Pooled excess risk estimate using the random effect model meta-analysis
#' @description Random effect model for standard meta-analysis of excess relative risk (ERR) or excess odds ratio (EOR) estimates.
#'
#'
#' @param x Object of class metaan.erf
#' @param ... Other arguments
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#' @rdname metaan.erf
#'
#' @return
#' @export
#'
#'
#'
print.metaan.erf <- function(x, ...){
  retmat_a = cbind(x$err_tot, x$sd_tot, x$l_tot, x$u_tot)

  retmat_b = cbind(x$Cochrane_stat, x$Degree_freedom, x$p_value)

  retmat_c = cbind(x$I_square)

  colnames(retmat_a) <- c("Effect", "SE Effect", "Lower CI", "Upper CI")
  colnames(retmat_b) <- c("Cochran’s Q statistic", "Degree of Freedom", "P-Value")
  colnames(retmat_c) <- c("Higgins’ and Thompson’s I^2 (%)")

  rownames(retmat_a) <- " "
  rownames(retmat_b) <- " "
  rownames(retmat_c) <- " "

  if(any(is.na(x$sd_tot))) retmat_a = retmat_a[,-2, drop=FALSE]
  cat("                                                  \n")
  cat("  Standard meta-analysis with random effect model \n")
  cat("------------------------------------------------- \n")
  cat("                                                  \n")
  printCoefmat(retmat_a)
  cat("                                                  \n")
  cat("------------------------------------------------- \n")
  cat("                                                  \n")
  cat("              Test of heterogeneity \n")
  cat("                                                  \n")
  printCoefmat(retmat_b)
  cat("                                                  \n")
  cat("------------------------------------------------- \n")
  cat("                                                  \n")
  printCoefmat(retmat_c)
  cat("_________________________________________________ \n")
  cat("                                                  \n")

}

