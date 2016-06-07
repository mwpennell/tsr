#' Function for getting mass dependent paramaters
#' Constants from DeLong 2015
#' @export pars_mass
#'
pars_mass <- function(M){
    list(r=5.25 * M^(-0.2),
         K=5.83*10^8 * M^(-0.81),
         a=1.2*10^-6 * M^1,
         m=5.62 * M^(-0.29),
         e=2.16 * M^(-0.5))
}

#' Function for getting temp dependent parameters
#' Standardized to some arbitrary value (here 15C)
#'
#' @export pars_temp
#'
pars_temp <- function(t, stnd=15){
    const <- temp_const()
    stnd  <- temp_stnd()
    r <- r_temp(t, const, t_std=stnd, value_std=stnd$r)
    K <- K_temp(t, const, t_std=stnd, value_std=stnd$K)
    m <- m_temp(t, const, t_std=stnd, value_std=stnd$m)
    a <- a_temp(t, const, t_std=stnd, value_std=stnd$a)
    list(r=r,K=K,m=m,a=a)
}

temp_const <- function(){
    list(eb=0.32,
         em=0.65,
         es=0.9,
         ev_c=0.46,
         ev_r=0.46,
         k=8.62*10^-5,
         v_c=1,
         v_r=1)}

temp_stnd <- function(){
    list(r=2,
         K=100,
         m=0.6,
         a=1)}

r_temp <- function(t, x, t_std, value_std){
    r0 <- value_std / exp(-x$eb/(x$k * (273.15 + t_std)))
    r0 * exp(-x$eb/(x$k * (273.15 + t)))
}

K_temp <- function(t, x, t_std, value_std){
    K0 <- value_std * exp(x$eb / (x$k + (273.15 + t_std))) / exp(x$es / (x$k + (273.15 + t_std)))
    K0 / exp(x$eb / (x$k + (273.15 + t))) * exp(x$es / (x$k + (273.15 + t)))
}

m_temp <- function(t, x, t_std, value_std){
    m0 <- value_std / exp(-x$em/(x$k * (273.15 + t_std)))
    m0 * exp(-x$em/(x$k * (273.15 + t)))
}

a_temp <- function(t, x, t_std, value_std){
    a0 <- value_std / sqrt(x$v_c * exp(-2 * x$ev_c/(x$k * (273.15 + t_std))) +
                           x$v_r * exp(-2 * x$ev_r/(x$k + (273.15 + t_std))))
    a0 * sqrt(x$v_c * exp(-2 * x$ev_c/(x$k * (273.15 + t))) +
              x$v_r * exp(-2 * x$ev_r/(x$k + (273.15 + t))))
}

