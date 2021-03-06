% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Estmeta.R
\name{estmeta}
\alias{estmeta}
\title{Meta-analysis of Beta (the parameters or coefficients estimated) from regression models}
\usage{
estmeta(Beta, u, l, test = c("FIXED", "RANDOM"), conf.level = 0.95)
}
\arguments{
\item{Beta}{A numeric vector of Beta (the parameters or coefficients) estimated from the individual studies}

\item{u}{A numeric vector of the upper bound of the confidence interval of the Beta reported from the individual studies.}

\item{l}{A numeric vector of the lower bound of the confidence interval of the Beta reported from the individual studies.}

\item{test}{Logical, indicating the statistical method to be used. "FIXED" for the fixed effect odel and "RANDOM" for the random effect model.}

\item{conf.level}{Coverage for confidence interval}
}
\value{
Object of class "metaan.ra". A list that print the output from the priskran function. The following could be found from the list :
\itemize{
\item rr_tot (Effect): The pooled effect from the individual studies' estimate (RR, OR, or HR)
\item sd_tot_lnRR (SE-Log(Effect)): The standard error of the pooled effect (see reference Richardson et al 2020 for more details)
\item l_tot (Lower CI): The lower confidence interval bound of the pooled effect (rr_tot)
\item u_tot (Upper CI): The upper confidence interval bound of the pooled effect (rr_tot)
\item Cochrane_stat (Cochran’s Q statistic): The value of the Cochrane's statistic of inter-study heterogeneity
\item Degree_freedom (Degree of Freedom): The degree of freedom
\item p_value (P-Value): The p-value of the statistic of Cochrane
\item I_square (Higgins’ and Thompson’s I^2 (\%)): I square value in percent (\%) indicating the amount of the inter-study heterogeneity
}
}
\description{
Fixed effect model or DerSimonian and Laird-based Random effect model for standard meta-analysis of Beta (the parameters or coefficients) estimated from regression models (e.g linear regression or generalised linear regression models).
}
\examples{
study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
"Madanapalle", "UK", "South Africa", "Haiti", "Madras")
beta<- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393,
0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254,
0.312, 0.996, 0.499, 1.145)

donne <- data.frame(cbind(study, beta, lower_ci, upper_ci))

donne$beta <- as.numeric(as.character(donne$beta))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))


estmeta(Beta=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, test="RANDOM")
estmeta(Beta=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, test="FIXED")



}
