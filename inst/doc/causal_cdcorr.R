## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
require(causalBatch)
require(ggplot2)
require(tidyr)
n = 200

## ----eval=FALSE---------------------------------------------------------------
#  vignette("cb.simulations", package="causalBatch")

## -----------------------------------------------------------------------------
# a function for plotting a scatter plot of the data
plot.sim <- function(Ys, Ts, Xs, title="", 
                     xlabel="Covariate",
                     ylabel="Outcome (1st dimension)") {
  data = data.frame(Y1=Ys[,1], Y2=Ys[,2], 
                    Group=factor(Ts, levels=c(0, 1), ordered=TRUE), 
                    Covariates=Xs)
  
  data %>%
    ggplot(aes(x=Covariates, y=Y1, color=Group)) +
    geom_point() +
    labs(title=title, x=xlabel, y=ylabel) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_color_manual(values=c(`0`="#bb0000", `1`="#0000bb"), 
                       name="Group/Batch") +
    theme_bw()
}

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_sigmoid(n=n, eff_sz=1, unbalancedness=1.5)

plot.sim(sim$Ys, sim$Ts, sim$Xs, title="Sigmoidal Simulation")

## -----------------------------------------------------------------------------
result <- cb.detect.caus_cdcorr(sim$Ys, sim$Ts, sim$Xs, R=100)

## -----------------------------------------------------------------------------
print(sprintf("p-value: %.4f", result$Test$p.value))

## -----------------------------------------------------------------------------
# compute distance matrix for outcomes
DY = dist(sim$Ys)

## -----------------------------------------------------------------------------
result <- cb.detect.caus_cdcorr(DY, sim$Ts, sim$Xs, distance=TRUE, R=100)

