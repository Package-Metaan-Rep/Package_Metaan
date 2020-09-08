#' @title Sensitivity analysis for excess relative risk (ERR) or excess odds ratio (EOR) estimates meta-analysis
#' @description Fixed or Random effect model with either the standard approach or the alternative one could be used for the sensitivity analysis computation. The risk estimate could be excess relative risk (ERR) or excess odds ratio (EOR).
#'
#' @param study A character vector of the name of the individual studies
#' @param err A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk estimated from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk estimated from the individual studies.
#' @param type Logical indicating the method to be used. The default is risk indicating that risk estimate model should be used.
#' @param d A numeric vector of the maximum dose reported from the individual studies.
#' @param test Logical indicating the statistical method to be used.
#' @param model Logical indicating which statistical model should be used the standard or the alternative model for the pooled risk estimate
#'
#'
#' @return A dataframe of a pooled result from the individual studies
#'
#'
#'
#'
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
#' "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
#' Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
#'  0.198, 1.012)
#' lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179,
#' 0.393, 0.078, 0.895)
#' upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312,
#' 0.996, 0.499, 1.145)
#' dose <- c(32.586, 15.257, 72.431, 6.528, 10.886, 11.659, 17.254, 20.312,
#' 10.996, 30.499, 41.145)
#'
#' donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci, dose))
#'
#' donne$Risk <- as.numeric(as.character(donne$Risk))
#' donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
#' donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
#' donne$dose <- as.numeric(as.character(donne$dose))
#'
#' exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
#' l=donne$lower_ci, type="excess", test = "FIXE", model = "standard")
#'
#' exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
#' l=donne$lower_ci, type="excess", test = "RANDOM", model = "standard")
#'
#' exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
#' l=donne$lower_ci, d=donne$dose, type="excess", test = "FIXE",
#'  model = "alternative")
#'
#' exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
#' l=donne$lower_ci, d=donne$dose, type="excess", test = "RANDOM",
#'  model = "alternative")
#'
#'
#' @export
#'
#'
exsens <- function(study, err, u, l, d=NULL,
                   type="excess", test = c("FIXE", "RANDOM"), model=c("standard", "alternative")){

  if(missing(test) | missing(model)){
    stop("Arguments test and model should be provided")
  }else{

    if(test=="FIXE" & model=="standard"){
      sensitivity <- NULL
      for (i in 1:length(study)) {
        df <- pexfix(err=err[-i], u=u[-i], l=l[-i], type="excess", test="FIXE")

        df <- cbind(study[i], df)
        sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
      }  # End of For loop
      colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
      class(sensitivity$RR_tot) <- "numeric"
      class(sensitivity$SE_log_RR) <- "numeric"
      class(sensitivity$Lower_CI) <- "numeric"
      class(sensitivity$Upper_CI) <- "numeric"
      #print(sensitivity)
      #print("STANDARD FIXE EFFECT MODEL EXCESS RISK ESTIMATE")
      message("STANDARD FIXED EFFECT MODEL EXCESS RISK ESTIMATE")

    }else{

      if(test=="FIXE" & model=="alternative"){
        if(is.null(d)){
          stop("With the alternative model, d should not be null. Provide a non-null value for d")

        }else{

          sensitivity <- NULL
          for (i in 1:length(study)) {
            df <- alpexfix(err=err[-i], u=u[-i], l=l[-i], d=d[-i], type="excess", test="FIXE")

            df <- cbind(study[i], df)
            sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
          }  # End of For loop
          colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
          class(sensitivity$RR_tot) <- "numeric"
          class(sensitivity$SE_log_RR) <- "numeric"
          class(sensitivity$Lower_CI) <- "numeric"
          class(sensitivity$Upper_CI) <- "numeric"
          #print(sensitivity)
          #print("ALTERNATIVE FIXE EFFECT MODEL EXCESS RISK ESTIMATE")
          message("ALTERNATIVE FIXED EFFECT MODEL EXCESS RISK ESTIMATE")

        }
      }
    }

    if(test=="RANDOM" & model=="standard"){
      sensitivity <- NULL
      for (i in 1:length(study)) {
        df <- pexrand(err=err[-i], u=u[-i], l=l[-i], type="excess", test="RANDOM")

        df <- cbind(study[i], df)
        sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
      }  # End of For loop
      colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
      class(sensitivity$RR_tot) <- "numeric"
      class(sensitivity$SE_log_RR) <- "numeric"
      class(sensitivity$Lower_CI) <- "numeric"
      class(sensitivity$Upper_CI) <- "numeric"
      #print(sensitivity)
      #print("STANDARD RANDOM EFFECT MODEL EXCESS RISK ESTIMATE")
      message("STANDARD RANDOM EFFECT MODEL EXCESS RISK ESTIMATE")

    }else{

      if(test=="RANDOM" & model=="alternative"){
        if(is.null(d)){
          stop("With the alternative model, d should not be null. Provide a non-null value for d")

        }else{

          sensitivity <- NULL
          for (i in 1:length(study)) {
            df <- alpexrand(err=err[-i], u=u[-i], l=l[-i], d=d[-i], type="excess", test="FIXE")

            df <- cbind(study[i], df)
            sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
          }  # End of For loop
          colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
          class(sensitivity$RR_tot) <- "numeric"
          class(sensitivity$SE_log_RR) <- "numeric"
          class(sensitivity$Lower_CI) <- "numeric"
          class(sensitivity$Upper_CI) <- "numeric"
          #print(sensitivity)
          #print("ALTERNATIVE RANDOM EFFECT MODEL EXCESS RISK ESTIMATE")
          message("ALTERNATIVE RANDOM EFFECT MODEL EXCESS RISK ESTIMATE")
        }

      }

    }

  }# End of else
  sensitivity

}



