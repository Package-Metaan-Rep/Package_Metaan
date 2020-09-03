pkgname <- "Metaan"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "Metaan-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Metaan')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("alpexfix")
### * alpexfix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: alpexfix
### Title: Pooled excess risk estimate using the alternative fixed effect
###   model meta-analysis
### Aliases: alpexfix

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm", "Madanapalle",
"UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625, 0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312, 0.996, 0.499, 1.145)
dose <- c(32.586, 15.257, 72.431, 6.528, 10.886, 11.659, 17.254, 20.312, 10.996, 30.499,
41.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci, dose))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
donne$dose <- as.numeric(as.character(donne$dose))

alpexfix(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, d=donne$dose,
type = "excess", test = "FIXE")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("alpexfix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("alpexrand")
### * alpexrand

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: alpexrand
### Title: Pooled excess risk estimate using the alternative random effect
###   model meta-analysis
### Aliases: alpexrand

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm", "Madanapalle",
"UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625, 0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312, 0.996, 0.499, 1.145)
dose <- c(32.586, 15.257, 72.431, 6.528, 10.886, 11.659, 17.254, 20.312,
 10.996, 30.499, 41.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci, dose))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
donne$dose <- as.numeric(as.character(donne$dose))

alpexrand(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci, d=donne$dose,
type = "excess", test = "RANDOM")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("alpexrand", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exsens")
### * exsens

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exsens
### Title: Sensitivity analysis for excess relative risk (ERR) or excess
###   odds ratio (EOR) estimates meta-analysis
### Aliases: exsens

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
"Madanapalle", "UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
 0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179,
0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312,
0.996, 0.499, 1.145)
dose <- c(32.586, 15.257, 72.431, 6.528, 10.886, 11.659, 17.254, 20.312,
10.996, 30.499, 41.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci, dose))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))
donne$dose <- as.numeric(as.character(donne$dose))

exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
l=donne$lower_ci, type="excess", test = "FIXE", model = "standard")

exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
l=donne$lower_ci, type="excess", test = "RANDOM", model = "standard")

exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
l=donne$lower_ci, d=donne$dose, type="excess", test = "FIXE",
 model = "alternative")

exsens(study=donne$study, err=donne$Risk, u=donne$upper_ci,
l=donne$lower_ci, d=donne$dose, type="excess", test = "RANDOM",
 model = "alternative")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exsens", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pexfix")
### * pexfix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pexfix
### Title: Pooled excess risk estimate using the fixed effect model
###   meta-analysis
### Aliases: pexfix

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
"Madanapalle", "UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179,
0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312,
0.996, 0.499, 1.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))

pexfix(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
type = "excess", test = "FIXE")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pexfix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pexrand")
### * pexrand

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pexrand
### Title: Pooled excess risk estimate using the random effect model
###   meta-analysis
### Aliases: pexrand

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
"Madanapalle", "UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179,
0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312,
0.996, 0.499, 1.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))

pexrand(err=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
type = "excess", test = "RANDOM")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pexrand", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("priskfix")
### * priskfix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: priskfix
### Title: Pooled risk estimate using the fixed effect model meta-analysis
### Aliases: priskfix

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
 "Madanapalle", "UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393,
0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254,
0.312, 0.996, 0.499, 1.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))

# on the log form
donne$ln_risk <- log(donne$Risk)
donne$ln_lower_ci <- log(donne$lower_ci)
donne$ln_upper_ci <- log(donne$upper_ci)


priskfix(rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
type = "risk", test = "FIXE", form="nonLog")


priskfix(rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
type = "risk", test = "FIXE", form="Log")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("priskfix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("priskran")
### * priskran

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: priskran
### Title: Pooled risk estimate using the random effect model meta-analysis
### Aliases: priskran

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm",
"Madanapalle", "UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625,
0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393,
0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254,
0.312, 0.996, 0.499, 1.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))

# on the log form
donne$ln_risk <- log(donne$Risk)
donne$ln_lower_ci <- log(donne$lower_ci)
donne$ln_upper_ci <- log(donne$upper_ci)


priskran(rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
type = "risk", test = "RANDOM", form="nonLog")

priskran(rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
type = "risk", test = "RANDOM", form="Log")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("priskran", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("risksens")
### * risksens

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: risksens
### Title: Sensitivity analysis for relative risk meta-analysis
### Aliases: risksens

### ** Examples

study <- c("Canada", "Northern USA", "Chicago", "Georgia","Puerto", "Comm", "Madanapalle",
"UK", "South Africa", "Haiti", "Madras")
Risk <- c(0.205, 0.411, 0.254, 1.562, 0.712, 0.983, 0.804, 0.237, 0.625, 0.198, 1.012)
lower_ci <- c(0.086, 0.134, 0.149, 0.374, 0.573, 0.582, 0.516, 0.179, 0.393, 0.078, 0.895)
upper_ci <- c(0.486, 1.257, 0.431, 6.528, 0.886, 1.659, 1.254, 0.312, 0.996,
0.499, 1.145)

donne <- data.frame(cbind(study, Risk, lower_ci, upper_ci))

donne$Risk <- as.numeric(as.character(donne$Risk))
donne$upper_ci <- as.numeric(as.character(donne$upper_ci))
donne$lower_ci <- as.numeric(as.character(donne$lower_ci))

# on the log form
donne$ln_risk <- log(donne$Risk)
donne$ln_lower_ci <- log(donne$lower_ci)
donne$ln_upper_ci <- log(donne$upper_ci)

risksens(study=donne$study, rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
type="risk", form="Log", test = "FIXE")

risksens(study=donne$study, rr=donne$ln_risk, u=donne$ln_upper_ci, l=donne$ln_lower_ci,
type="risk", form="Log", test = "RANDOM")

risksens(study=donne$study, rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
type="risk", form="nonLog", test = "FIXE")

risksens(study=donne$study, rr=donne$Risk, u=donne$upper_ci, l=donne$lower_ci,
type="risk", form="nonLog", test = "RANDOM")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("risksens", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
