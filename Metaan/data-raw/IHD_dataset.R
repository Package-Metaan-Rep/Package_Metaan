# original data from empirical exmample

# Meta-analysis of epidemiological findings regarding
# associations between ischemic heart disease and exposure to low-level radiation

IHD = as.data.frame(rbind(
  c("Azizova et al. 2010",	0.12,	  0.051, 	0.186, 	5.92),
  c("Ivanov et al. 2006",	0.41,	  0.05, 	0.78, 	0.50),
  c("Lane et al. 2010",	0.15,	  -0.14, 	0.58, 	0.12),
  c("Laurent et al. 2010",	4.10,	  -2.9, 	13.7, 	0.60),
  c("Muirhead et al. 2009",	0.26,	  -0.05, 	0.61, 	0.40),
  c("Shimizu et al. 2010",	0.02,	  -0.1, 	0.15, 	4.00),
  c("Vrijheid et al. 2007",	-0.01,  -0.59, 	0.69, 	0.50),
  c("Yamada et al. 2004",	0.05,	  -0.05, 	0.16, 	4.00))
)

names(IHD) <- c("Study", "err", "lower_ci", "upper_ci", "max_dose")

IHD$err <- as.numeric(as.character(IHD$err))
IHD$lower_ci <- as.numeric(as.character(IHD$lower_ci))
IHD$upper_ci <- as.numeric(as.character(IHD$upper_ci))
IHD$max_dose <- as.numeric(as.character(IHD$max_dose))

#usethis::use_data(IHD, compress = "xz", overwrite=TRUE)

# Reported from :
# Little MP, Azizova TV, Bazyka D, et al. Systematic
# review and meta-analysis of circulatory disease from exposure to
# low-level ionizing radiation and estimates of potential population
# mortality risks. Environ Health Perspect 2012;120(11):1503-11.

# By :
# Richardson, D. B., Abalo, K., Bernier, M. O., Rage, E., Leuraud, K.,
# Laurier, D., ... & Little, M. P. (2020). Meta-analysis of published
# excess relative risk estimates. Radiation and Environmental Biophysics, 1-11.
