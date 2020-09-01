## code to prepare `TUBERCULOSIS DATASET` dataset goes here

#usethis::use_data(DATASET, overwrite = TRUE)



study <- c("Canada", "Northern USA", "Chicago", "Georgia (Sch)","Puerto Rico",
           "Georgia (Comm)", "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
authors <- c("Ferguson & Simes", "Aronson", "Rosenthal et al.", "Comstock & Webster", "Comstck et al.",
             "Frimon-Moller et al", "Hart & Sutherland", "Coetzee & Berjak", "Vandeviere et al",
             "TB Prevention Trial")
years <- c(1933, 1935, 1941, 1947, 1949, 1950, 1950, 1950, 1965, 1965, 1968)

Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237,
          0.625, 0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179,
              0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312,
              0.996, 0.499, 1.145)
#dose <- c(32.586, 15.257, 72.431, 6.528, 10.886, 11.659, 17.254, 20.312,
#         10.996, 30.499, 41.145)

Tuberculosis <- data.frame(cbind(study, authors, years,
                                 Risk, lower_ci, upper_ci #, dose
                                 ))

Tuberculosis$Risk <- as.numeric(as.character(Tuberculosis$Risk))
Tuberculosis$upper_ci <- as.numeric(as.character(Tuberculosis$upper_ci))
Tuberculosis$lower_ci <- as.numeric(as.character(Tuberculosis$lower_ci))
#Tuberculosis$dose <- as.numeric(as.character(Tuberculosis$dose))


# on the log form

#Tuberculosis$ln_risk <- log(Tuberculosis$Risk)
#Tuberculosis$ln_lower_ci <- log(Tuberculosis$lower_ci)
#Tuberculosis$ln_upper_ci <- log(Tuberculosis$upper_ci)



rm(#dose,
  lower_ci, Risk, study, upper_ci, authors, years)

#usethis::use_data(Tuberculosis, compress = "xz", overwrite=TRUE)
