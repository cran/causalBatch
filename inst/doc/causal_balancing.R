## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
require(causalBatch)
require(ggplot2)
require(tidyr)
n = 300

## -----------------------------------------------------------------------------
plot.covars <- function(Xs, Ts, title="", xlabel="Covariate", 
                        ylabel="Density") {
  data.frame(Batch=factor(Ts, levels=c(0, 1)), Covariate=Xs) %>%
    ggplot(aes(x=Covariate, group=Batch, color=Batch)) +
      geom_rug() +
      geom_histogram(aes(fill=Batch), binwidth=0.1, position="identity",
                     alpha=0.5) +
      labs(title=title, x=xlabel, y=ylabel) +
      scale_x_continuous(limits=c(-1, 1)) +
      scale_color_manual(values=c(`0`="#bb0000", `1`="#0000bb"), 
                         name="Group/Batch") +
      scale_fill_manual(values=c(`0`="#bb0000", `1`="#0000bb"), 
                         name="Group/Batch") +
      theme_bw()
}

## ----fig.width=5, fig.height=3------------------------------------------------
sim.low <- cb.sims.sim_linear(n=n, unbalancedness=2)
plot.covars(sim.low$Xs, sim.low$Ts, title="Sample covariate values")

## ----fig.width=5, fig.height=3------------------------------------------------
vm.retained <- cb.align.vm_trim(sim.low$Ts, sim.low$Xs)
plot.covars(sim.low$Xs[vm.retained], sim.low$Ts[vm.retained],
            title="Sample covariate values (after VM)")

## ----fig.width=5, fig.height=3------------------------------------------------
kway.retained <- cb.align.kway_match(sim.low$Ts, data.frame(Covar=sim.low$Xs),
                                   match.form="Covar")
plot.covars(sim.low$Xs[kway.retained], sim.low$Ts[kway.retained],
            title="Sample covariate values (after K-way matching)")

