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

## ----eval=FALSE---------------------------------------------------------------
#  vignette("cb.detect.caus_cdcorr", package="causalBatch")

## -----------------------------------------------------------------------------
result <- cb.detect.caus_cdcorr(sim$Ys, sim$Ts, sim$Xs, R=100)
print(sprintf("p-value: %.4f", result$Test$p.value))

## -----------------------------------------------------------------------------
cor.sim <- cb.correct.caus_cComBat(sim$Ys, sim$Ts, data.frame(Covar=sim$Xs), 
                                   match.form="Covar")

## ----fig.width=5, fig.height=3------------------------------------------------
plot.sim(cor.sim$Ys.corrected, cor.sim$Ts, cor.sim$Xs$Covar,
         title="Sigmoidal Simulation (causal cComBat corrected)")

## -----------------------------------------------------------------------------
result.cor <- cb.detect.caus_cdcorr(cor.sim$Ys.corrected, cor.sim$Ts,
                                    cor.sim$Xs$Covar, R=100)
print(sprintf("p-value: %.4f", result.cor$Test$p.value))

## -----------------------------------------------------------------------------
Xs.2covar <- data.frame(Covar1=sim$Xs, Covar2=runif(n))

## -----------------------------------------------------------------------------
cor.sim <- cb.correct.caus_cComBat(sim$Ys, sim$Ts, Xs.2covar, 
                                   match.form="Covar1 + Covar2")

## -----------------------------------------------------------------------------
Xs.3covar <- cbind(data.frame(Cat.Covar=factor(rbinom(n, size=1, 0.5))), 
                   Xs.2covar)

## -----------------------------------------------------------------------------
match.args <- list(method="nearest", exact="Cat.Covar", replace=FALSE, 
                   caliper=0.1)
cor.sim <- cb.correct.caus_cComBat(sim$Ys, sim$Ts, Xs.3covar, 
                                   match.form="Covar1 + Covar2 + Cat.Covar",
                                   match.args=match.args)

