#' @title Test of heterogeneity
#' @description Cochrane's Q statistic and I square computation in meta-analysis
#'
#' @param rr A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk reported from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk reported from the individual studies.
#' @param form Logical, indicating the scale of the data. If Log, then the original data are in logarithmic scale.
#'
#'
#' @export
#'
hetest <- function(rr, u, l, form = c("Log", "nonLog")){

  if(missing(form)){
    stop("Arg form should be Log or nonLog. Please precise")

  }else{

    if(form=="Log"){
      sd = (u-l)/(2*1.96)
      var = sd^2
      rr_tot1 = sum(rr/var, na.rm = T)/sum(1/var, na.rm = T)
      q = round(sum(((rr - rr_tot1)/sd)^2, na.rm = T), 2)
      k = length(rr)
      df = k - 1
      I = max(0 , round((1 - (df/q))*100, 2))

      Interpr_I <- ifelse(I<=25, "Low",
                          ifelse(I>25 & I<=75, "Medium",
                                 ifelse(I>75, "High", "Je ne sais pas")))

      res <- as.data.frame(cbind(Q=q, df=df, I_square=I, Interpretation=paste(Interpr_I, "Heterogeneity")))
      res

    }else{

      if(form=="nonLog"){# The data is not in log form
        # Then the data into log form

        len <- length(which(c(rr, u, l)<=0))

        if(len==0){

          rr <- log(rr)
          u <- log(u)
          l <- log(l)

          # Then run the code


          sd = (u-l)/(2*1.96)
          var = sd^2
          rr_tot1 = sum(rr/var, na.rm = T)/sum(1/var, na.rm = T)
          q = round(sum(((rr - rr_tot1)/sd)^2, na.rm = T), 2)
          k = length(rr)
          df = k - 1
          I = max(0 , round((1 - (df/q))*100, 2))

          Interpr_I <- ifelse(I<=25, "Low",
                              ifelse(I>25 & I<=75, "Medium",
                                     ifelse(I>75, "High", "Je ne sais pas")))

          res <- as.data.frame(cbind(Q=q, df=df, I_square=I, Interpretation=paste(Interpr_I, "Heterogeneity")))
          res


        }else{
          stop("ERROR: To compute a logarithme, x should be a positif numeric number" )
        }

      }
    }
  }
}

