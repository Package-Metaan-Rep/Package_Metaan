#' @title  Pooled risk estimate using the random effect model meta-analysis
#' @description Random effect model for standard meta-analysis of risk estimate (e.g relative risk (RR), odds ratio (OR) and hazard ratio (HR))
#'
#' @param rr A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk estimated from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk estimated from the individual studies.
#' @param form Logical indicating the scale of the data. If Log, then the original data are in logarithme scale.
#' @param type Logical indicating the method to be used. The default is risk indicating that risk estimate model should be used.
#' @param test Logical indicating the statistical method to be used. The default if RANDOM for the random effect model.
#' @param conf.level Coverage for confidence intervals
#'
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#'
#' @return A list of a pooled result from the individual studies
#'
#'
#'
#' @references DerSimonian R, Laird N (1986) Meta-analysis in clinical trials. Controlled clinical trials 7:177–188.
#'
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
#' "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
#' Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
#' 0.198, 1.012)
#' lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393,
#' 0.078, 0.895)
#' upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254,
#' 0.312, 0.996, 0.499, 1.145)
#'
#' donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))
#'
#' donne$Risk <- as.numeric(as.character(donne$Risk))
#' donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
#' donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
#'
#' # on the log form
#' donne$ln_risk <- log(donne$Risk)
#' donne$ln_lower_ci <- log(donne$lower_ci)
#' donne$ln_upper_ci <- log(donne$upper_ci)
#'
#'
#' priskran(rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
#' type = "risk", test = "RANDOM", form="nonLog", conf.level=0.95)
#'
#' priskran(rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
#' type = "risk", test = "RANDOM", form="Log", conf.level=0.95)
#'
#' @export
#'
#'
priskran <- function(rr, u, l, form = c("Log", "nonLog"),
                     type="risk", test="RANDOM", conf.level=0.95){

  if (conf.level>1 & conf.level<100)
    conf.level<-conf.level/100

  z.alpha <- (-qnorm((1-conf.level)/2))

  if(missing(form)){
    stop("Arg form should be Log or nonLog. Please precise")
  }else{

    if(form=="Log"){

      sd = (u-l)/(2*z.alpha)
      var = sd^2
      rr_tot1 = sum(rr/var, na.rm = T)/sum(1/var, na.rm = T)
      q = sum(((rr - rr_tot1)/sd)^2)
      sum_num_rand = sum(1/sd^4, na.rm = T)
      sum_den_rand = sum(1/var, na.rm = T)
      nstudies = length(rr)
      deltasq = max(0, (q - (nstudies-1)) / (sum_den_rand - (sum_num_rand/sum_den_rand)))
      sum_num = sum(rr/(var + deltasq))
      sum_den = sum(1/(var + deltasq))
      rr_tot = sum_num/sum_den
      sd_tot = 1/sqrt(sum_den)

      l_tot = rr_tot - z.alpha*sd_tot # lower Confidence Interval of the lnRR
      u_tot = rr_tot + z.alpha*sd_tot # Upper confidence interval of the lnRR


      # Compute heterogeneity
      rr_tot1 = sum(rr/var, na.rm = T)/sum(1/var, na.rm = T)
      Q = round(sum(((rr - rr_tot1)/sd)^2, na.rm = T), 2)
      k = length(rr)
      df = k - 1
      I = max(0 , round((1 - (df/Q))*100, 2))


      # compute result

      ret = list(#type="Standard approach with fixed effect model",
        rr_tot = round(exp(rr_tot), 2),
        sd_tot_lnRR = round(sd_tot, 2),
        l_tot = round(exp(l_tot), 2),
        u_tot = round(exp(u_tot), 2),
        Cochrane_stat = Q,
        Degree_freedom = df,
        p_value = stats::pchisq(Q, df, lower.tail = F),
        I_square = I)

      class(ret) <- "metaan.ra"
      ret
    }else{

      if(form=="nonLog"){
        # The data is not in log form
        # Then the data into log form

        len <- length(which(c(rr, u, l)<=0))

        if(len==0){

          rr <- log(rr)
          u <- log(u)
          l <- log(l)
          # Then run the code

          sd = (u-l)/(2*z.alpha)
          var = sd^2
          rr_tot1 = sum(rr/var, na.rm = T)/sum(1/var, na.rm = T)
          q = sum(((rr - rr_tot1)/sd)^2)
          sum_num_rand = sum(1/sd^4, na.rm = T)
          sum_den_rand = sum(1/var, na.rm = T)
          nstudies = length(rr)
          deltasq = max(0, (q - (nstudies-1)) / (sum_den_rand - (sum_num_rand/sum_den_rand)))
          sum_num = sum(rr/(var + deltasq))
          sum_den = sum(1/(var + deltasq))
          rr_tot = sum_num/sum_den
          sd_tot = 1/sqrt(sum_den)

          l_tot = rr_tot - z.alpha*sd_tot # lower Confidence Interval of the lnRR
          u_tot = rr_tot + z.alpha*sd_tot # Upper confidence interval of the lnRR


          # Compute heterogeneity
          rr_tot1 = sum(rr/var, na.rm = T)/sum(1/var, na.rm = T)
          Q = round(sum(((rr - rr_tot1)/sd)^2, na.rm = T), 2)
          k = length(rr)
          df = k - 1
          I = max(0 , round((1 - (df/Q))*100, 2))


          # compute result

          ret = list(#type="Standard approach with fixed effect model",
            rr_tot = round(exp(rr_tot), 2),
            sd_tot_lnRR = round(sd_tot, 2),
            l_tot = round(exp(l_tot), 2),
            u_tot = round(exp(u_tot), 2),
            Cochrane_stat = Q,
            Degree_freedom = df,
            p_value = stats::pchisq(Q, df, lower.tail = F),
            I_square = I )


          class(ret) <- "metaan.ra"
          ret

        }else{
          stop("ERROR: To compute a logarithme, x should be a positif numeric number" )
        }
      }
    }
  }
}



#' @title  Pooled risk estimate using the random effect model meta-analysis
#' @description Random effect model for standard meta-analysis of risk estimate (e.g relative risk (RR), odds ratio (OR) and hazard ratio (HR))
#'
#'
#'@param x Object of class metaan.ra
#' @param ... Other arguments
#'
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#' @rdname metaan.ra
#' @return
#' @export
#'
#'
print.metaan.ra <- function(x, ...){
  retmat_a = cbind(x$rr_tot, x$sd_tot_lnRR, x$l_tot, x$u_tot)

  retmat_b = cbind(x$Cochrane_stat, x$Degree_freedom, x$p_value)

  retmat_c = cbind(x$I_square)

  colnames(retmat_a) <- c("Effect", "SE-Log(Effect)", "Lower CI", "Upper CI")
  colnames(retmat_b) <- c("Cochran’s Q statistic", "Degree of Freedom", "P-Value")
  colnames(retmat_c) <- c("Higgins’ and Thompson’s I^2 (%)")

  rownames(retmat_a) <- " "
  rownames(retmat_b) <- " "
  rownames(retmat_c) <- " "

  if(any(is.na(x$sd_tot_lnRR))) retmat_a = retmat_a[,-2, drop=FALSE]
  cat("                                                   \n")
  cat("  Standard meta-analysis with random effect model  \n")
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
