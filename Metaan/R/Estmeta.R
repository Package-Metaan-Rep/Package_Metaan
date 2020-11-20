#' @title  Meta-analysis of Beta (the parameters or coefficients estimated) from regression models
#' @description Fixed effect model or DerSimonian and Laird-based Random effect model for standard meta-analysis of Beta (the parameters or coefficients) estimated from regression models (e.g linear regression or generalised linear regression models).
#'
#' @param Beta A numeric vector of Beta (the parameters or coefficients) estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the Beta reported from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the Beta reported from the individual studies.
#' @param test Logical, indicating the statistical method to be used. "FIXED" for the fixed effect odel and "RANDOM" for the random effect model.
#' @param conf.level Coverage for confidence interval
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#'
#' @return Object of class "metaan.ra". A list that print the output from the priskran function. The following could be found from the list :
#'  - rr_tot (Effect): The pooled effect from the individual studies' estimate (RR, OR, or HR)
#'  - sd_tot_lnRR (SE-Log(Effect)): The standard error of the pooled effect (see reference Richardson et al 2020 for more details)
#'  - l_tot (Lower CI): The lower confidence interval bound of the pooled effect (rr_tot)
#'  - u_tot (Upper CI): The upper confidence interval bound of the pooled effect (rr_tot)
#'  - Cochrane_stat (Cochran’s Q statistic): The value of the Cochrane's statistic of inter-study heterogeneity
#'  - Degree_freedom (Degree of Freedom): The degree of freedom
#'  - p_value (P-Value): The p-value of the statistic of Cochrane
#'  - I_square (Higgins’ and Thompson’s I^2 (%)): I square value in percent (%) indicating the amount of the inter-study heterogeneity
#'
#'
#'
#' @export
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
#' "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
#' beta<- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
#' 0.198, 1.012)
#' lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393,
#' 0.078, 0.895)
#' upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254,
#' 0.312, 0.996, 0.499, 1.145)
#'
#' donne <- data.frame(cbind(study, beta, lower_ci, upper_ci))
#'
#' donne$beta <- as.numeric(as.character(donne$beta))
#' donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
#' donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
#'
#'
#'estmeta(Beta=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, test="RANDOM")
#'estmeta(Beta=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, test="FIXED")
#'
#'
#'
estmeta <- function(Beta, u, l, test=c("FIXED", "RANDOM"), conf.level=0.95){

  if (conf.level>1 & conf.level<100)
    conf.level<-conf.level/100

  z.alpha <- (-qnorm((1-conf.level)/2))

  if(missing(test)){
    stop("Arg test should be Log or nonLog. Please precise")
  }else{

    if(test=="FIXED"){
      sd = (u-l)/(2*z.alpha)
      var = sd^2
      sum_num = sum(Beta/var, na.rm = T)
      sum_den = sum(1/var, na.rm = T)
      Beta_tot = sum_num/sum_den
      sd_tot = 1/(sqrt(sum_den))

      l_tot = Beta_tot - z.alpha*sd_tot # lower Confidence Interval of the Beta
      u_tot = Beta_tot + z.alpha*sd_tot # Upper confidence interval of the Beta

      # Compute heterogeneity
      Beta_tot1 = sum(Beta/var, na.rm = T)/sum(1/var, na.rm = T)
      Q = round(sum(((Beta - Beta_tot1)/sd)^2, na.rm = T), 2)
      k = length(Beta)
      df = k - 1
      I = max(0 , round((1 - (df/Q))*100, 2))


      # compute result

      ret = list(#type="Standard approach with fixed effect model",
        Beta_tot = round(Beta_tot, 2),
        sd_tot = round(sd_tot, 2),
        l_tot = round(l_tot, 2),
        u_tot = round(u_tot, 2),
        Cochrane_stat = round(Q, 2),
        Degree_freedom = round(df, 2),
        p_value = round(stats::pchisq(Q, df, lower.tail = F), 4),
        I_square = round(I, 2), test=test )

      class(ret) <- "metaan.est"
      ret
    }else{
      if (test=="RANDOM"){


        sd = (u-l)/(2*z.alpha)
        var = sd^2
        Beta_tot1 = sum(Beta/var, na.rm = T)/sum(1/var, na.rm = T)
        q = sum(((Beta - Beta_tot1)/sd)^2)
        sum_num_rand = sum(1/sd^4, na.rm = T)
        sum_den_rand = sum(1/var, na.rm = T)
        nstudies = length(Beta)
        deltasq = max(0, (q - (nstudies-1)) / (sum_den_rand - (sum_num_rand/sum_den_rand)))
        sum_num = sum(Beta/(var + deltasq))
        sum_den = sum(1/(var + deltasq))
        Beta_tot = sum_num/sum_den
        sd_tot = 1/sqrt(sum_den)

        l_tot = Beta_tot - z.alpha*sd_tot # lower Confidence Interval of the Beta
        u_tot = Beta_tot + z.alpha*sd_tot # Upper confidence interval of the Beta


        # Compute heterogeneity
        Beta_tot1 = sum(Beta/var, na.rm = T)/sum(1/var, na.rm = T)
        Q = round(sum(((Beta - Beta_tot1)/sd)^2, na.rm = T), 2)
        k = length(Beta)
        df = k - 1
        I = max(0 , round((1 - (df/Q))*100, 2))


        # compute result

        ret = list(#type="Standard approach with fixed effect model",
          Beta_tot = round(Beta_tot, 2),
          sd_tot = round(sd_tot, 2),
          l_tot = round(l_tot, 2),
          u_tot = round(u_tot, 2),
          Cochrane_stat = Q,
          Degree_freedom = df,
          p_value = round(stats::pchisq(Q, df, lower.tail = F), 4),
          I_square = I, test=test)

        class(ret) <- "metaan.est"
        ret

        }

      }

    }


}






#' @title  Meta-analysis of Beta (the parameters or coefficients estimated) from regression models
#' @description Fixed effect model or DerSimonian and Laird-based Random effect model for standard meta-analysis of Beta (the parameters or coefficients) estimated from regression models (e.g linear regression or generalised linear regression models).
#'
#'
#'@param x Object of class metaan.est
#' @param ... Other arguments
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#' @rdname metaan.est
#' @return
#' @export
#'
#'
print.metaan.est <- function(x, ...){
  retmat_a = cbind(x$Beta_tot, x$sd_tot, x$l_tot, x$u_tot)

  retmat_b = cbind(x$Cochrane_stat, x$Degree_freedom, x$p_value)

  retmat_c = cbind(x$I_square)

  colnames(retmat_a) <- c("Beta", "SE-Beta", "Lower CI", "Upper CI")
  colnames(retmat_b) <- c("Cochran’s Q statistic", "Degree of Freedom", "P-Value")
  colnames(retmat_c) <- c("Higgins’ and Thompson’s I^2 (%)")

  rownames(retmat_a) <- " "
  rownames(retmat_b) <- " "
  rownames(retmat_c) <- " "

  if(any(is.na(x$sd_tot))) retmat_a = retmat_a[,-2, drop=FALSE]
  cat("                                                   \n")
  cat("Standard meta-analysis with",paste(x$test), "effect model  \n")
  cat("-------------------------------------------------- \n")
  cat("                                                   \n")
  printCoefmat(retmat_a)
  cat("                                                   \n")
  cat("-------------------------- ----------------------- \n")
  cat("                                                   \n")
  cat("            Test of heterogeneity : \n")
  cat("                                                   \n")
  printCoefmat(retmat_b)
  cat("                                                   \n")
  cat("-------------------------------------------------- \n")
  cat("                                                   \n")
  printCoefmat(retmat_c)
  cat("__________________________________________________ \n")
  cat("                                                   \n")

}



