## Set up constants

temp_constants <- function(){
    list(eb=0.32,
         em=0.65,
         es=0.9,
         ev_c=0.46,
         ev_r=0.46,
         k=8.62*10^-5,
         v_c=1,
         v_r=1)}

## Converting from standardized temps

temp_param <- function(t, t_std=15, r_std=2, K_std=100,  m_std=0.6, a_std=1){
    const <- temp_constants()
    r <- r_temp(t, const, t_std=t_std, value_std=r_std)
    K <- K_temp(t, const, t_std=t_std, value_std=K_std)
    m <- m_temp(t, const, t_std=t_std, value_std=m_std)
    a <- a_temp(t, const, t_std=t_std, value_std=a_std)
    list(r=r,K=K,m=m,a=a)
}

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
