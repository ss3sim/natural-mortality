#' Obtain the desired number of ss3sim iterations with a invertible hessian
#'
#' Run a single scenario until the proper number have
#' converged, where convergence is an invertible hessian.
#' @author Kelli Faye Johnson
#' @param scen.use ss3sim scenario
#' @param num.need the total number of converged iterations needed
#' @param master.wd working directory where simulations are stored
#' @param models location of model folder
#' @param cases location of case folder
#' @export

get_enough <- function(scen.use, num.need = 250, 
                       master.wd = main.dir, 
                       models, cases){
  for(i in seq_along(scen.use)) {
    runMe <- scen.use[i]

    setwd(file.path(master.wd, runMe))
      hess.mainbias <- dir(pattern = "admodel.cov", 
                           recursive = TRUE)
      num.done <- length(hess.mainbias[-grep("bias", 
                                              hess.mainbias)])
      num.tried <- max(grep("([0-9])", dir()[-grep("csv", dir())]))
      next.run <- num.tried + 1

      print(paste("running scenario:", runMe, 
                  "==", num.done))
      flush.console()

    while(num.done < num.need){
      species <- rev(unlist(strsplit(runMe, "-")))[1]
      om <- grep(species, models, value = TRUE)[1]
      em <- grep(species, models, value = TRUE)[2]
      setwd(master.wd)
      run_ss3sim(iterations = next.run, scenarios = runMe,
                 bias_already_run = TRUE, 
                 hess_always = TRUE, show.output.on.console = FALSE,
                 case_folder = cases, 
                 om_model_dir = om, em_model_dir = em)
      good <- length(dir(file.path(master.wd, runMe, next.run, "em"), 
                         pattern = "admodel.cov"))
      num.done <- num.done + good
      next.run <- next.run + 1
    }
    setwd(master.wd)
  }
}
