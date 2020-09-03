#' @title Pooled risk estimate using the fixed effect model meta-analysis
#' @description Fixed effect model for standard meta-analysis of risk estimate (e.g relative risk (RR), odds ratio (OR) and hazard ratio (HR))
#'
#' @param rr A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk estimated from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk estimated from the individual studies.
#' @param form Logical indicating the scale of the data. If Log, then the original data are in logarithme scale.
#' @param type Logical indicating the method to be used. The default is risk indicating that risk estimate model should be used.
#' @param test Logical indicating the statistical method to be used. The default if FIXE for the fixe effect model.
#'
#' @return A list of a pooled result from the individual studies
#'
#'
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
#'  "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
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
#' priskfix(rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
#' type = "risk", test = "FIXE", form="nonLog")
#'
#'
#' priskfix(rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
#' type = "risk", test = "FIXE", form="Log")
#'
#'
#' @export
#'

priskfix <- function(rr, u, l, form = c("Log", "nonLog"),
                     type="risk", test="FIXE"){

  if(missing(form)){
    stop("Arg form should be Log or nonLog. Please precise")
  }else{

    if(form=="Log"){
      sd = (u-l)/(2*1.96)
      var = sd^2
      sum_num = sum(rr/var, na.rm = T)
      sum_den = sum(1/var, na.rm = T)
      rr_tot = sum_num/sum_den
      sd_tot = 1/(sqrt(sum_den))

      l_tot = rr_tot - 1.96*sd_tot # lower Confidence Interval of the lnRR
      u_tot = rr_tot + 1.96*sd_tot # Upper confidence interval of the lnRR

      ret = as.data.frame(cbind(type="Standard approach with fixed effect model",
                                rr_tot = exp(rr_tot),
                                sd_tot_lnRR = sd_tot,
                                l_tot = exp(l_tot),
                                u_tot = exp(u_tot)

      ))
      #class(ret) <- "metaan_rr"
      ret
    }else{
      if (form=="nonLog"){ # The data is not in log form
        # Then the data into log form

        len <- length(which(c(rr, u, l)<=0))

        if(len==0){

          rr <- log(rr)
          u <- log(u)
          l <- log(l)
          # Then run the code
          sd = (u-l)/(2*1.96)
          var = sd^2
          sum_num = sum(rr/var, na.rm = T)
          sum_den = sum(1/var, na.rm = T)
          rr_tot = sum_num/sum_den # Pooled RR on the log form (lnRR)
          sd_tot = 1/(sqrt(sum_den)) # standard deviation of lnRR

          l_tot = rr_tot - 1.96*sd_tot # lower Confidence Interval of the lnRR
          u_tot = rr_tot + 1.96*sd_tot # Upper confidence interval of the lnRR

          ret = as.data.frame(cbind(type="Standard approach with fixed effect model",
                                    rr_tot = exp(rr_tot),
                                    sd_tot_lnRR = sd_tot,
                                    l_tot = exp(l_tot),
                                    u_tot = exp(u_tot)

          ))
          #class(ret) <- "metaan_rr"
          ret

        }else{
          stop("ERROR: To compute a logarithme, x should be a positif numeric number" )
        }

      }

    }
  }
}
