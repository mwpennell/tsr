#' Calculate bcr at equilbrium
#' Type I Functional Response
#' @export bcr_eqm_1
#'
bcr_eqm_1 <- function(m, a, e, K)
    K * a * e / m

#' Equilibrium biomass for resource
#' Type I Functional Response
#' @export biomass_res_1
#'
biomass_res_1 <- function(m, a, e)
    m / (e * a)

#' Equilibrium biomass for consumer
#' Type I Functional Response
#' @export biomass_con_1
#'
biomass_con_1 <- function(m, a, e, r, K)
    r / a * (1 - m / (e * a * K))
