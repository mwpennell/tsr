bcr_type1_K_only <- function(t, t_std=15, K_std=100, m_std=0.6, a_std=1, e=0.15){
    K <- temp_param_K_only(t, t_std, K_std)
    e * a_std * K / m_std
}



bcr_type1 <- function(t, t_std=15, r_std=2, K_std=100,  m_std=0.6, a_std=1, e=0.15){
    p <- temp_param(t, t_std, r_std, K_std, m_std, a_std)
    e * p$a * p$K / p$m
}
