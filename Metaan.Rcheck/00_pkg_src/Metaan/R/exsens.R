#' @title Sensitivity analysis for excess relative risk (ERR) or excess odds ratio (EOR) estimates meta-analysis
#' @description Fixed or Random effect model with either the standard approach or the alternative one could be used for the sensitivity
#'              analysis computation. The risk estimate could be excess relative risk (ERR) or excess odds ratio (EOR).
#'
#' @description In the sensitivity analysis, each individual study is removed one at a time and the summarized estimate is computed to access
#'              the effect of the removed study on the overall pooled estimate.
#'
#' @param study A vector (or column for dataframe, matrix) specifying the column reporting the author's name or the individual study's name
#' @param err A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk reported from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk reported from the individual studies.
#' @param type Logical, the type of risk estimate reported. The default is excess indicating that risk estimate reported from the individual studies are excess (e.g. ERR or EOR).
#' @param d A numeric vector of the maximum dose reported from the individual studies.
#' @param test Logical, indicating the statistical method to be used. The user have the choice between "FIXED" for the fixed effect model, and "RANDOM" for the random effect model.
#' @param model Logical, indicating which statistical model should be used. The user have the choice between "standard" for the standard approach, and alternative" for the alternative approach for combining the risk estimate.
#' @param conf.level Coverage for confidence interval
#'
#' @return Object of class "data.frame" that print the output from the exsens function. The following could be found from the output :
#'  - Study: Indication of the study removed from the pooled effect estimate
#'  - Effect: The pooled effect from the individual studies' estimate (ERR or EOR)
#'  - SE(Effect): The standard error of the pooled effect (see reference Richardson et al 2020 for more details)
#'  - Lower CI: The lower confidence interval bound of the pooled effect
#'  - Upper CI: The upper confidence interval bound of the pooled effect
#'
#' @author Kossi Abalo
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
#' l=donne$lower_ci, type="excess", test = "FIXED", model = "standard")
#'
#' exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
#' l=donne$lower_ci, type="excess", test = "RANDOM", model = "standard")
#'
#' exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
#' l=donne$lower_ci, d=donne$dose, type="excess", test = "FIXED",
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
exsens <- function(study, err, u, l, d=NULL, conf.level=0.95,
                   type="excess", test = c("FIXED", "RANDOM"),
                   model=c("standard", "alternative")){

  if (conf.level>1 & conf.level<100)
    conf.level<-conf.level/100


  if(missing(test) | missing(model)){
    stop("Arguments test and model should be provided")
  }else{

    if(test=="FIXED" & model=="standard"){
      sensitivity <- NULL
      for (i in 1:length(study)) {

        sens <- pexfix(err=err[-i], u=u[-i], l=l[-i],
                       type="excess", test="FIXED", conf.level=conf.level)
        df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
        df <- cbind(study[i], df)
        sensitivity <- rbind(df[ , 1:5], sensitivity)

        #sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
      }  # End of For loop
      colnames(sensitivity) <- c("Study", "ERR_tot", "SE_ERR", "Lower_CI", "Upper_CI")
      class(sensitivity$ERR_tot) <- "numeric"
      class(sensitivity$SE_ERR) <- "numeric"
      class(sensitivity$Lower_CI) <- "numeric"
      class(sensitivity$Upper_CI) <- "numeric"

      message("STANDARD FIXED EFFECT MODEL EXCESS RISK ESTIMATE")

    }else{

      if(test=="FIXED" & model=="alternative"){
        if(is.null(d)){
          stop("With the alternative model, d should not be null. Provide a non-null value for d")

        }else{

          sensitivity <- NULL
          for (i in 1:length(study)) {

            sens <- alpexfix(err=err[-i], u=u[-i], l=l[-i], d=d[-i], type="excess",
                             test="FIXED", conf.level=conf.level)
            df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
            df <- cbind(study[i], df)
            sensitivity <- rbind(df[ , 1:5], sensitivity)

            #sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
          }  # End of For loop
          colnames(sensitivity) <- c("Study", "ERR_tot", "SE_ERR", "Lower_CI", "Upper_CI")
          class(sensitivity$ERR_tot) <- "numeric"
          class(sensitivity$SE_ERR) <- "numeric"
          class(sensitivity$Lower_CI) <- "numeric"
          class(sensitivity$Upper_CI) <- "numeric"
          #print(sensitivity)
          #print("ALTERNATIVE FIXED EFFECT MODEL EXCESS RISK ESTIMATE")
          message("ALTERNATIVE FIXED EFFECT MODEL EXCESS RISK ESTIMATE")

        }
      }
    }

    if(test=="RANDOM" & model=="standard"){
      sensitivity <- NULL
      for (i in 1:length(study)) {

        sens <- pexrand(err=err[-i], u=u[-i], l=l[-i],
                        type="excess", test="RANDOM", conf.level=conf.level)
        df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
        df <- cbind(study[i], df)
        sensitivity <- rbind(df[ , 1:5], sensitivity)

        #sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
      }  # End of For loop
      colnames(sensitivity) <- c("Study", "ERR_tot", "SE_ERR", "Lower_CI", "Upper_CI")
      class(sensitivity$ERR_tot) <- "numeric"
      class(sensitivity$SE_ERR) <- "numeric"
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

            sens <- alpexrand(err=err[-i], u=u[-i], l=l[-i], d=d[-i],
                              type="excess", test="RANDOM", conf.level=conf.level)
            df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
            df <- cbind(study[i], df)
            sensitivity <- rbind(df[ , 1:5], sensitivity)

            #sensitivity <- rbind(df[ , c(1, 3:6)], sensitivity)
          }  # End of For loop
          colnames(sensitivity) <- c("Study", "ERR_tot", "SE_ERR", "Lower_CI", "Upper_CI")
          class(sensitivity$ERR_tot) <- "numeric"
          class(sensitivity$SE_ERR) <- "numeric"
          class(sensitivity$Lower_CI) <- "numeric"
          class(sensitivity$Upper_CI) <- "numeric"
          #print(sensitivity)
          #print("ALTERNATIVE RANDOM EFFECT MODEL EXCESS RISK ESTIMATE")
          message("ALTERNATIVE RANDOM EFFECT MODEL EXCESS RISK ESTIMATE")
        }

      }

    }

  }# End of else
  result = sensitivity
  names(result) = c("Study", "Effect", "SE(Effect)", "Lower CI", "Upper CI")
  result
}



