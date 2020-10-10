#' @title Sensitivity analysis for relative risk (or odds ratio) meta-analysis
#' @description Fixed or Random effect model could be used for the sensitivity analysis computation. The
#'  risk estimate could be e.g relative risk (RR), odds ratio (OR) or hazard ratio (HR).
#'
#' @description In the sensitivity analysis, each individual study is removed one at a time and the
#'  summarized estimate is computed to access the effect of the removed study on the overall pooled estimate.
#'
#'
#' @param study A vector (or column for dataframe, matrix) specifying the column reporting the author's name or the individual study's name
#' @param rr A numeric vector of the risk estimated from the individual studies
#' @param u A numeric vector of the upper bound of the confidence interval of the risk estimated from the individual studies.
#' @param l A numeric vector of the lower bound of the confidence interval of the risk estimated from the individual studies.
#' @param form Logical, indicating the scale of the data. If Log, then the original data are in logarithmic scale.
#' @param type Logical, indicating the method to be used. The default is risk indicating that risk estimate model should be used.
#' @param test Logical, indicating the statistical method to be used. The user have the choice between "FIXED" for the fixed effect model, and "RANDOM" for the random effect model.
#' @param conf.level Coverage for confidence interval
#'
#'
#'
#' @return Object of class "data.frame" that print the output from the risksens function. The following could be found from the output :
#'  - Study: Indication of the study removed from the pooled effect estimate
#'  - Effect: The pooled effect from the individual studies' estimate (RR, OR, or HR)
#'  - SE-Log(Effect): The standard error of the pooled effect (see reference Richardson et al 2020 for more details)
#'  - Lower CI: The lower confidence interval bound of the pooled effect
#'  - Upper CI: The upper confidence interval bound of the pooled effect
#'
#' @author Kossi Abalo
#'
#' @examples
#' study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm", "Madanapalle",
#' "UK", "South Africa", "Haiti", "Madras")
#' Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625, 0.198, 1.012)
#' lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393, 0.078, 0.895)
#' upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312, 0.996,
#' 0.499, 1.145)
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
#' risksens(study=donne$study, rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
#' type="risk", form="Log", test = "FIXED")
#'
#' risksens(study=donne$study, rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
#' type="risk", form="Log", test = "RANDOM")
#'
#' risksens(study=donne$study, rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
#' type="risk", form="nonLog", test = "FIXED")
#'
#' risksens(study=donne$study, rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
#' type="risk", form="nonLog", test = "RANDOM")
#'
#' @export
#'
risksens <- function(study, rr, u, l, form = c("Log", "nonLog"),
                     type="risk", test = c("FIXED", "RANDOM"), conf.level=0.95){


  if (conf.level>1 & conf.level<100)
    conf.level<-conf.level/100


  if(missing(test) | missing(form)){
    stop("Arguments test and form should be provided")
  }else{

    if(test=="FIXED" & form=="Log"){
      sensitivity <- NULL
      for (i in 1:length(study)) {

        sens <- priskfix(rr=rr[-i], u=u[-i], l=l[-i], form="Log",
                         type="risk", test="FIXED", conf.level=conf.level)

        df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
        df <- cbind(study[i], df)
        sensitivity <- rbind(df[ , 1:5], sensitivity)

      }  # End of For loop
      colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
      sensitivity$RR_tot<-as.numeric(as.character(sensitivity$RR_tot))
      sensitivity$SE_log_RR<-as.numeric(as.character(sensitivity$SE_log_RR))
      sensitivity$Lower_CI<-as.numeric(as.character(sensitivity$Lower_CI))
      sensitivity$Upper_CI<-as.numeric(as.character(sensitivity$Upper_CI))
      #print(sensitivity)
      #print("FIXED EFFECT MODEL RISK ESTIMATE ON LOG SCALE")
      message("FIXED EFFECT MODEL RISK ESTIMATE ON LOG SCALE")

    }else{

      if(test=="FIXED" & form=="nonLog"){
        sensitivity <- NULL
        for (i in 1:length(study)) {


          sens <- priskfix(rr=rr[-i], u=u[-i], l=l[-i],
                           form="nonLog", type="risk", test="FIXED", conf.level=conf.level)

          df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
          df <- cbind(study[i], df)
          sensitivity <- rbind(df[ , 1:5], sensitivity)

        }  # End of For loop
        colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
        sensitivity$RR_tot<-as.numeric(as.character(sensitivity$RR_tot))
        sensitivity$SE_log_RR<-as.numeric(as.character(sensitivity$SE_log_RR))
        sensitivity$Lower_CI<-as.numeric(as.character(sensitivity$Lower_CI))
        sensitivity$Upper_CI<-as.numeric(as.character(sensitivity$Upper_CI))
        #print(sensitivity)
        #print("FIXED EFFECT MODEL RISK ESTIMATE ON NON-LOG SCALE")
        message("FIXED EFFECT MODEL RISK ESTIMATE ON NON-LOG SCALE")

      }
    }



    if(test=="RANDOM" & form=="Log"){
      sensitivity <- NULL
      for (i in 1:length(study)) {

        sens <- priskran(rr=rr[-i], u=u[-i], l=l[-i], form="Log",
                         type="risk", test="RANDOM", conf.level=conf.level)
        df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
        df <- cbind(study[i], df)
        sensitivity <- rbind(df[ , 1:5], sensitivity)

      }  # End of For loop
      colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
      sensitivity$RR_tot<-as.numeric(as.character(sensitivity$RR_tot))
      sensitivity$SE_log_RR<-as.numeric(as.character(sensitivity$SE_log_RR))
      sensitivity$Lower_CI<-as.numeric(as.character(sensitivity$Lower_CI))
      sensitivity$Upper_CI<-as.numeric(as.character(sensitivity$Upper_CI))
      #print(sensitivity)
      #print("RANDOM EFFECT MODEL RISK ESTIMATE ON LOG SCALE")
      message("RANDOM EFFECT MODEL RISK ESTIMATE ON LOG SCALE")

    }else{

      if(test=="RANDOM" & form=="nonLog"){
        sensitivity <- NULL
        for (i in 1:length(study)) {


          sens <- priskran(rr=rr[-i], u=u[-i], l=l[-i],
                           form="nonLog", type="risk", test="RANDOM", conf.level=conf.level)
          df <- data.frame(t(matrix(unlist(sens), nrow=length(sens), byrow=T)))
          df <- cbind(study[i], df)
          sensitivity <- rbind(df[ , 1:5], sensitivity)


        }  # End of For loop
        colnames(sensitivity) <- c("Study", "RR_tot", "SE_log_RR", "Lower_CI", "Upper_CI")
        sensitivity$RR_tot<-as.numeric(as.character(sensitivity$RR_tot))
        sensitivity$SE_log_RR<-as.numeric(as.character(sensitivity$SE_log_RR))
        sensitivity$Lower_CI<-as.numeric(as.character(sensitivity$Lower_CI))
        sensitivity$Upper_CI<-as.numeric(as.character(sensitivity$Upper_CI))
        #print(sensitivity)
        #print("RANDOM EFFECT MODEL RISK ESTIMATE ON NON-LOG SCALE")
        message("RANDOM EFFECT MODEL RISK ESTIMATE ON NON-LOG SCALE")

      }

    }


  }# End of else
  result = sensitivity
  colnames(result) = c("Study", "Effect", "SE Log(Effect)", "Lower CI", "Upper CI")
  result

}# End of the function


