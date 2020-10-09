
# prepare and check package for cran release
# uses devtools,rhub package
library(devtools)
library(rhub)

##### user settings #####
release=FALSE # set to true when you are ready to release to CRAN



##### no user settings below here #####
# set wd to package directory
setdir <- function(){
    res = try(
        setwd(getSrcDirectory()[1]), silent = TRUE
    )
    if(class(res) == "try-error") setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    setwd("Metaan")
    message("Current directory")
    message(getwd())
}

setdir()

# create documentation for 
devtools::document()    # overwrites namespace file
devtools::missing_s3()  # checks for missing S3 objects

# pre-cran checks
devtools::spell_check()
devtools::check(cran=FALSE, run_dont_test = TRUE, args = "--timings") # also in Rhub with different ids (RHUB will send off to remote server)

# edit this file when there are comments that need to be given to CRAN (e.g. if first submit, or if responding to request for changes)
usethis::use_cran_comments(open=TRUE) 


# local check as CRAN
devtools::check(cran=TRUE) # also in Rhub with different ids (RHUB will send off to remote server)

# remote check on different platforms
platforms = c("windows-x86_64-devel", "ubuntu-gcc-release", "fedora-clang-devel", "macos-highsierra-release")
rhub::check_for_cran(email="antoine.kossi@hotmail.com", platforms = platforms[-1], show_status=FALSE) # from rhub, checks windows, ubuntu, fedora,mac
# results will be emailed to you in 20mins to several hours


# once pre-cran checks are good
if(release){
    message("CHECK THAT THE FOLLOWING EXIST AND ARE UP TO DATE: NEWS.md, cran-comments.md")
    devtools::build(binary=FALSE, vignettes = TRUE)
    if(rel) devtools::release() #alternative to manual submission
}


