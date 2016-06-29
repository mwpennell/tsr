## # Consumer resource dynamics with TSR
## library(tsr)

## *Currently something weird is up with the installation. Need to fix. For now I'll just source all the functions directly*
## devtools::install_github("richfitz/odin")
library(odin)
source("../R/summary.R")
source("../R/ode.R")
source("../R/pars.R")

## ## Standard consumer resource models (no mass or temperature dependency)

## Using default parameter values from Gilbert et al.

## ## Type 1 functional response

mod_1 <- cr_ode_1()
tt <- seq(0, 200, length.out=100)
out_1 <- mod_1$run(tt)
plot(out_1, ylab = "biomass", mfrow=c(1, 2), which=1, main="Resource")
plot(out_1, ylab = "biomass", mfrow=NULL, which=2, main="Consumer")

## As a check we want to make sure that it returns a logistic growth if consumer is absent

mod_1_noc <- cr_ode_1(C0=0)
out_1_noc <- mod_1_noc$run(tt)
plot(out_1_noc, ylab = "biomass", mfrow=c(1, 2), which=1, main="Resource")
plot(out_1_noc, ylab = "biomass", mfrow=NULL, which=2, main="Consumer")

## ## Type 2 functional response

mod_2 <- cr_ode_2()
out_2 <- mod_2$run(tt)
plot(out_2, ylab = "biomass", mfrow=c(1, 2), which=1, main="Resource")
plot(out_2, ylab = "biomass", mfrow=NULL, which=2, main="Consumer")

## ## Type 3 functional response
mod_3 <- cr_ode_3(R0=30, C0=30)
out_3 <- mod_3$run(tt)
plot(out_3, ylab = "biomass", mfrow=c(1, 2), which=1, main="Resource")
plot(out_3, ylab = "biomass", mfrow=NULL, which=2, main="Consumer")


## ## Temperature dependency a la Gilbert et al.

## Get parameters as a function of temperature
temps <- seq(5, 30, by=0.05)
pars <- lapply(temps, function(x) pars_temp(x))

## Here we are only going to consider Type 1 functional responses

## ### BCR as a function of temperature (using analytical equilibrium values)
bcr <- sapply(pars, function(x) bcr_eqm_1(m=x$m, a=x$a, e=0.15, K=x$K))

## standardize to 15 decrees and plot
bcr <- bcr / bcr[which(temps == 15)]
plot(temps, bcr, xlab="temperature")

## ok, this is looking alright

## ### Consumer:resource biomass at equilibrium
biom_con <- sapply(pars, function(x)
    biomass_con_1(m=x$m, a=x$a, e=0.15, K=x$K, r=x$r))

biom_res <- sapply(pars, function(x)
    biomass_res_1(m=x$m, a=x$a, e=0.15))

biom_eqm <- biom_con/biom_res
plot(temps, biom_eqm, xlab="temperature", ylab="c:r biomass")

## *This is the wrong shape*

