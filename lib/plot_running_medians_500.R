## This file is for generating plots of running medians for scalar quantities to
## visually determine if 100 iterations is enough.

## started 11/14/2013 by Cole

## THIS FILE IS FOR THE SCENARIOS WITH EXTRA REPLICATES (500)
library(ggplot2)
library(cumplyr)
library(plyr)
library(reshape2)

## read in the data and drop the scenarios that we don't want to look at, plus
## the extra columns which will just slow the calcs down
scalars <- read.csv("Result Files/500iterations_scalar.csv")
scalars <- transform(scalars,
                     Q_om=exp(scalars$LnQ_base_2_Survey_om),
                     Q_em=exp(scalars$LnQ_base_2_Survey_em),
                     R0_em=exp(scalars$SR_LN_R0_em),
                     R0_om=exp(scalars$SR_LN_R0_om),
                     uniqueID= paste0(scenario, "-", replicate)
                     )
## relative error calcs
scalars <-
    transform(scalars,
              R0=(R0_em-R0_om)/R0_om,
              SR_sigmaR=(SR_sigmaR_em-SR_sigmaR_om)/SR_sigmaR_om,
              NatM=(NatM_p_1_Fem_GP_1_em-NatM_p_1_Fem_GP_1_om)/
                      NatM_p_1_Fem_GP_1_om,
              Linf=(L_at_Amax_Fem_GP_1_em-L_at_Amax_Fem_GP_1_om)/
                      L_at_Amax_Fem_GP_1_om,
              VBK= (VonBert_K_Fem_GP_1_em-VonBert_K_Fem_GP_1_om)/
                      VonBert_K_Fem_GP_1_om,
              CVyoung=(CV_young_Fem_GP_1_em-CV_young_Fem_GP_1_om)/
                      CV_young_Fem_GP_1_om,
              CVold=(CV_old_Fem_GP_1_em-CV_old_Fem_GP_1_om)/
                      CV_old_Fem_GP_1_om,
              Sel1Fishery=(SizeSel_1P_1_Fishery_em-SizeSel_1P_1_Fishery_om)/
                     SizeSel_1P_1_Fishery_om,
              Sel2Fishery=(SizeSel_1P_2_Fishery_em-SizeSel_1P_2_Fishery_om)/
                     SizeSel_1P_2_Fishery_om,
              Sel1Survey=(SizeSel_2P_1_Survey_em-SizeSel_2P_1_Survey_om)/
                     SizeSel_2P_1_Survey_om,
              Sel2Survey=(SizeSel_2P_2_Survey_em-SizeSel_2P_2_Survey_om)/
                     SizeSel_2P_2_Survey_om,
              Q=(Q_em-Q_om)/Q_om,
              TAC=(Catch_endyear_em - Catch_endyear_om)/Catch_endyear_om
              )
scalars <- subset(scalars, select = c("scenario", "replicate", "NatM","R0", "Q", "TAC"))
scalars.long <- melt(scalars, c("scenario", "replicate"))
table(scalars$scenario)
table(scalars$replicate)

## Make some quick plots showing RE by scenario
g <- ggplot(scalars.long, aes(x=replicate, y=value, color=scenario))+ylim(-1,1)
g+geom_line()+facet_wrap("variable")
ggsave("Plots/RE_by_replicate.png", width=12, height=5, units="in")

## Now make running MARE calculations. Takes a while to run these calcs!!!
my.median <- function(x) {sapply(1:length(x), function(i) {median(x[1:i])})}
test <- ddply(scalars, .(scenario), .fun=summarize,
              replicate2=1:length(replicate),
              MARE_M=my.median(abs(NatM)),
              MARE_R0=my.median(abs(R0)),
              MARE_Q=my.median(abs(Q)),
              MARE_TAC=my.median(abs(TAC)))
scalars.mares <- melt(test, c("scenario", "replicate2"))

## Plot them
g <- ggplot(data=scalars.mares, aes(x=replicate2, y=value, colour=scenario))+ylim(0,1)+
    geom_vline(xintercept=100) + ylab("MARE") +xlab("Replicate")
g+geom_line(lwd=1)+facet_wrap(.(variable))
ggsave("Plots/cumulative_MAREs.png", width=12, height=5, units="in")

## ------------------------------------------------------------
## OLD DONT USE THIS BELOW




## ## first try at doing this, kusing a pacakge. decided to redo it manually to
## ## make sure it worked the way I thought it did, this is above here
## scalars.med <- cumddply(data=scalars, c("scenario"),
##                         "replicate", func=
##                         function(df) {with(df, cbind(
##                             mymare=my.median(abs(NatM)),
##                             MARE_M=median(abs(NatM)),
##                             MARE_R0=median(abs(R0)),
##                             MARE_Q=median(abs(Q)),
##                             MARE_TAC=median(abs(TAC)),
##                             meanM=mean(NatM),
##                             meanR0=mean(R0),
##                             meanQ=mean(Q), meanTAC=mean(TAC)))})
## scalars.med <- within(scalars.med, {
##     replicate <- as.numeric(replicate)
##     scenario <- as.factor(scenario)})
## str(scalars.med)

## ## make plots


## library(ggplot2)
## g <- ggplot(data=scalars.med, aes(x=replicate, colour=scenario))+ylim(0,1)
## g+geom_line(aes(y=MARE_M))
## g+geom_line(aes(y=MARE_R0))
## g+geom_line(aes(y=MARE_Q))
## g+geom_line(aes(y=MARE_TAC))

## g+geom_line(aes(y=meanM))
## scalars.med.sar <- droplevels(scalars.med[grep("sar", scalars.med$scenario),])
## table(scalars.med.sar$scenario)
## g <- ggplot(scalars.med.sar, aes(x=replicate, y=NatM, color=scenario)) +
##     geom_line(width=.5)
## g
## ggsave("medians_sar_NatM.png", g, width=5, height=10)

## ## Check that this function is working
## xx <- droplevels(subset(scalars, scenario=="D0-E0-F11-M190-R0-cod",
##                         select=c(TAC, replicate)))
## medians <-

## dim(xx)

