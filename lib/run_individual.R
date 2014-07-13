#' run a single ss3sim scenario
#'
#' Run a single ss3sim scenario, while obtaining a specific number
#' of bias adjustment runs
#' @author Kelli Faye Johnson
#' @param runMe ss3sim scenario
#' @param totNum total number of iterations
#' @param criteria needed number of bias adjustment runs
#' @param cases location of case folder
#' @param bias_nsim number of initial bias ajustment runs to try
#' @param doiter logical, to run iterations or not
#' if FALSE only the bias adjustment runs are completed
#' @export
run_individual <- function(runMe, totNum = 100, criteria = 8,
                           cases = cases.folder, bias_nsim = 50,
                           doiter = TRUE){
  if(runMe %in% dir()) {
    } else{
  print(runMe)
  species <- rev(unlist(strsplit(runMe, "-")))[1]
  om <- grep(species, models, value = TRUE)[1]
  em <- grep(species, models, value = TRUE)[2]
  ## Perform bias adjusment runs
  # If less than 2 bias runs pass stop trying and label the scenario "FAILED"
  # If at least # set in "criteria" passed then label the scenario "PASSED"
  # else == delete original runs and run more 
  # run more until there are enough to qualify as "PASSED"  
  run_ss3sim(iterations = 1:1, scenarios = runMe,
             bias_adjust = TRUE, bias_nsim = bias_nsim, 
             bias_already_run = FALSE, 
             hess_always = TRUE, show.output.on.console = FALSE,
             case_folder = cases, 
             om_model_dir = om, em_model_dir = em)

  passBias <- length(dir(file.path(getwd(), runMe, "bias"), pattern = "ramp"))
  if(passBias >= criteria) converged <- "PASSED"
  if(passBias < criteria) converged <- "NO"
  if(passBias < criteria) {
              for(i in seq(100, 400, by = 50)) {
              # Determine how many bias adjustment runs to perform
              # based on the number passed in the initial trial
              runThisMany <- i
              if(bias_nsim > i) next
              unlink(file.path(getwd(), runMe), 
                     recursive = TRUE, force = TRUE)
              run_ss3sim(iterations = 1:1, scenarios = runMe,
                         bias_adjust = TRUE, bias_nsim = runThisMany,
                         bias_already_run = FALSE, 
                         hess_always = TRUE, show.output.on.console = FALSE,
                         case_folder = cases, 
                         om_model_dir = om, em_model_dir = em)
              passBias <- length(dir(file.path(getwd(), runMe, "bias"), 
                                     pattern = "ramp"))
              if(passBias >= criteria) converged <- "PASSED"
              if(passBias >= criteria) break
    }
  }
  if(converged == "PASSED" & doiter == TRUE) {
    run_ss3sim(iterations = seq(2, totNum), scenarios = runMe,
               bias_already_run = TRUE, 
               hess_always = TRUE, show.output.on.console = FALSE,
               case_folder = cases,
               om_model_dir = om, em_model_dir = em)
    }}
}
