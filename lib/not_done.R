#' determine which ss3sim scenarios are running or were ran
#'
#' @author Kelli Faye Johnson
#' @param scen.use a vector of possible scenarios
#' @param my.wd where to look for simulations
#' @export

not_done <- function(scen.use = scen.full,
                     my.wd = "t:/kfjohns/test") {
  setwd(my.wd)
  number <- array(dim = length(scen.use))
  for(i in seq(scen.use)){
    subtract <- length(dir(file.path(getwd(), scen.use[i]), pattern = "csv"))
    total <- length(dir(file.path(getwd(), scen.use[i])))
    number[i] <- total - subtract - 1
  }

  num <- length(scen.use)
  data <- data.frame(scenario = numeric(num),
                     species  = character(num),
                     num.bias = numeric(num),
                     tot.bias = numeric(num),
                     warning  = logical(num), stringsAsFactors = FALSE)

  for(i in seq(scen.use)){
    data$scenario[i] <- scen.use[i]
    data$num.bias[i] <- length(dir(file.path(getwd(), scen.use[i], "bias"), 
                                             pattern = "ramp"))
    data$tot.bias[i] <- suppressWarnings(max(as.numeric(dir(file.path(getwd(),
                                                                      scen.use[i], 
                                                                      "bias"))), 
                                         na.rm = TRUE))
    data$warning[i] <- ifelse(file.exists(file.path(getwd(), 
                                                    scen.use[i], 
                                                    "bias/WARNINGS.TXT")) == TRUE,
                              TRUE, FALSE)
    data$species[i] <- rev(unlist(strsplit(scen.use[i], "-")))[1]
  }

  data.2 <- subset(data, num.bias < 8)
  return(list(info = data, numbers = number, badones = data.2))
}
