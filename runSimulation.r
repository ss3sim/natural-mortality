###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author      : Johnson, Kelli Faye
####Contact     : kellifayejohnson@gmail.com
####Lastupdate  : 2014-07-11
####Purpose     : Simulation for Natural Mortality ICES Paper
####Packages    : ss3sim
####Inputs      : 
####Outputs     : 
####Remarks     : Character width == 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

###############################################################################
## Step 01
## Set the variable inputs
###############################################################################
## Set the working directory to the t storage space
main.dir      <- "c:/ss/natural-mortality"
cases.folder  <- "c:/ss/natural-mortality/caseFiles"

subDir <- "test"
totNum <- 100

installss3sim <- "github" #Can be "local" or "github"
install.need <- FALSE     #Can be TRUE of FALSE

###############################################################################
## Step 02
## Install packages
###############################################################################
if(install.need == TRUE) {
  if(installss3sim == "github") {
    devtools::install_github("ss3sim", username = "ss3sim", 
                             ref = "widebounds")
  }
  if(installss3sim == "local") {
    devtools::install("c:/ss/ss3sim")
  }
  
  install.packages(file.path(main.dir, "r4ss", "r4ss-read-only-mod"), 
                   repos = NULL, type = "source")
}

require(r4ss)
require(ss3sim)

###############################################################################
## Step 03
## Source functions
###############################################################################
my.functions <- dir(file.path(main.dir, "lib"),
                    full.names = TRUE)
source(my.functions)
###############################################################################
## Step 04
## Model location
###############################################################################
dir.create(file.path(main.dir, subDir), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(main.dir, subDir))

d <- system.file("extdata", package = "ss3sim")
  spp <- c("cod", "fla", "sar")
  spp.grid <- expand.grid(spp, c("om", "em"))
  models <- file.path(d, "models", apply(spp.grid, 1, paste, collapse = "-"))

scen.stan <- c(
  expand_scenarios(cases = list(D = 0, E = 0:3, F = c(11, 21, 31), 
                                M = c(150, 175, 190), R = 0), species = spp),
  expand_scenarios(cases = list(D = 0, E = 0:3, F = c(12, 22, 32), 
                                M = 200, R = 0), species = spp),
  expand_scenarios(cases = list(D = 0, E = 0:3, F = c(13, 23, 33), 
                                M = c(350, 375, 390), R = 0), species = spp))
                                             
scen.sens <- c(
  expand_scenarios(
  cases = list(D = 0, E = c(11:17, 21:27), F = c(12, 32),
               M = 200, R = 0), species = "cod"),
  expand_scenarios(
  cases = list(D = 0, E = c(11:17, 21:27), F = c(13, 33),
               M = 375, R = 0), species = "cod"))
scen.full <- c(scen.stan, scen.sens)

require(snowfall)
  sfInit(parallel=TRUE, cpus=3)
  sfCpus()
  sfExportAll()
  sfLibrary(ss3sim)
  sfLibrary(r4ss)

    sfSapply(x = sample(scen.full), 
             fun = run_individual, totNum = 250, 
             doiter = TRUE, bias_nsim = 10)
    sfSapply(x = sample(grep("E1-F2.-M3..-R0-fla", scen.full, value = TRUE)),
             fun = get_enough, num.need = 250,
             master.wd = file.path(main.dir, subDir), models, cases.folder)

badbiasruns <- which(scen.full %in% not_done()$badones$scenario)
notenufruns <- which(scen.full %in%
                     c("D0-E1-F13-M350-R0-fla", "D0-E1-F23-M350-R0-fla", "D0-E1-F33-M350-R0-fla",
                       "D0-E0-F13-M375-R0-sar", "D0-E1-F23-M390-R0-sar", "D0-E1-F33-M390-R0-sar",
                       "D0-E2-F33-M375-R0-sar"))

get_results_all(overwrite_files = FALSE, user_scenarios = scen.full[-c(badbiasruns, notenufruns)])
scalars <- read.csv(dir(pattern = "r.csv", full.names = TRUE)) 
ts <- read.csv(dir(pattern = "s.csv", full.names = TRUE))

write.table(not_done()$badones, "badRuns.txt")
write.table(ddply(scalars, .(E, F, M, species), summarize, 
                  poshess = sum(hessian), 
                  onbounds = sum(params_on_bound_em, na.rm = TRUE)),
            "parsOnBoundsWideBounds.txt")

################################################################################
################################################################################
# endOfFile