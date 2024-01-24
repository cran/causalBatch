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

# a function for plotting a scatter plot of the data, along with
# lines denoting the expected outcome (per covariate level)
plot.sim.expected <- function(Ys, Ts, Xs, Ytrue, Ttrue, Xtrue, title="",
                           xlabel="Covariate",
                           ylabel="Outcome (1st dimension)") {
  data = data.frame(Y1=Ys[,1], Y2=Ys[,2], 
                    Group=factor(Ts, levels=c(0, 1), ordered=TRUE), 
                    Covariates=Xs)
  data.true = data.frame(Y1=Ytrue[,1], Y2=Ytrue[,2], 
                         Group=factor(Ttrue, levels=c(0, 1), ordered=TRUE), 
                         Covariates=Xtrue)
  
  data %>%
    ggplot(aes(x=Covariates, y=Y1, color=Group)) +
    geom_point(alpha=0.5) +
    labs(title=title, x=xlabel, y=ylabel) +
    geom_line(data=data.true, aes(group=Group), linewidth=1.2) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_color_manual(values=c(`0`="#bb0000", `1`="#0000bb"), 
                       name="Group/Batch") +
    theme_bw()
}

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_sigmoid(n=n)

plot.sim(sim$Ys, sim$Ts, sim$Xs, title="Sigmoidal Simulation")

## -----------------------------------------------------------------------------
print(sprintf("Covariate Overlap: %.3f", sim$Overlap))

## ----fig.width=5, fig.height=3------------------------------------------------
plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Sigmoidal Simulation")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_sigmoid(n=n, unbalancedness = 3)

plot.sim(sim$Ys, sim$Ts, sim$Xs, 
         title="Sigmoidal Simulation (Imbalanced Covariates)")

## ----fig.width=5, fig.height=3------------------------------------------------
plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Sigmoidal Simulation")

## -----------------------------------------------------------------------------
print(sprintf("Covariate Overlap: %.3f", sim$Overlap))

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_sigmoid(n=n, unbalancedness = 1/3)

plot.sim(sim$Ys, sim$Ts, sim$Xs, 
         title="Sigmoidal Simulation (Imbalanced Covariates)")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_linear(n=n)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Linear Simulation")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_linear(n=n, pi = 0.25)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Linear Simulation (Imbalanced Groups/Batches)")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_impulse(n=n)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Impulse Simulation")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_impulse(n=n, nbreaks=5)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Impulse Simulation")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_impulse_asycov(n=n, unbalancedness = 1)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Impulse Simulation")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_impulse_asycov(n=n, unbalancedness = 4)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Impulse Simulation (Asymmetric Covariates)")

## ----fig.width=5, fig.height=3------------------------------------------------
sim = cb.sims.sim_impulse_asycov(n=n, unbalancedness = 1/5)

plot.sim.expected(sim$Ys, sim$Ts, sim$Xs,
                  sim$Ytrue, sim$Ttrue, sim$Xtrue,
                  title="Impulse Simulation (Asymmetric Covariates)")

