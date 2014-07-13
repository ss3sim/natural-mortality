## This file is for generating plots of running medians for scalar quantities to
## visually determine if 100 iterations is enough.

## started 11/14/2013 by Cole

## THIS FILE IS FOR ALL SCENARIOS
library(ggplot2)
library(cumplyr)
library(plyr)
library(reshape2)

## read in the data and drop the scenarios that we don't want to look at, plus
## the extra columns which will just slow the calcs down
scalars <- read.csv("Result Files/scalars_12-18.csv")
names(scalars)
scalars <- droplevels(subset(scalars, F.num %in% c("F11", "F12", "F13")) )
scalars <- subset(scalars, select = c("scenario", "replicate", "species",
                               "NatM","R0", "Q", "TAC"))
scalars.long <- melt(scalars, c("scenario", "replicate", "species"))


## Make some quick plots showing RE by scenario
g <- ggplot(scalars.long, aes(x=replicate, y=value, group=scenario))+ylim(-2,2)
g+geom_point(alpha=.3, size=.5)+facet_grid(species~variable) + ylab("Relative Error")
ggsave("Plots/RE_by_replicate_all.png", width=10, height=7, units="in")

## Now make running MARE calculations. Takes a while to run these calcs!!!
my.median <- function(x) {sapply(1:length(x), function(i) {median(x[1:i])})}
test <- ddply(scalars, .(scenario), .fun=summarize,
              replicate2=1:length(replicate),
              MARE_M=my.median(abs(NatM))-median(abs(NatM)),
              MARE_R0=my.median(abs(R0))-median(abs(R0)),
              MARE_Q=my.median(abs(Q))-median(abs(Q)),
              MARE_TAC=my.median(abs(TAC))-median(abs(TAC)))
scalars.mares <- melt(test, c("scenario", "replicate2"))

## Plot them
g <- ggplot(data=scalars.mares, aes(x=replicate2, y=value, group=scenario))+ylim(-1,1)+
    geom_vline(xintercept=100) + ylab("Centered MARE") +xlab("Replicate")
g+geom_line(lwd=.5, alpha=.3)+facet_wrap(.(variable))
ggsave("Plots/cumulative_MAREs_all.png", width=9, height=5, units="in")
