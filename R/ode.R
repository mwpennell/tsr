#' CR model
#' Type I Functional Response
#' @export cr_ode_1
#'
cr_ode_1 <- odin({
    deriv(R) <- r * R * (1 - R/K) - a * R * C
    deriv(C) <- e * a * R * C - m * C
    initial(R) <- R0
    initial(C) <- C0

    R0 <- user(10)
    C0 <- user(10)
    K <- user(100)
    r <- user(2)
    a <- user(1)
    e <- user(0.15)
    m <- user(0.6)
}, verbose=FALSE)


#' CR model
#' Type II Functional Response
#' @export cr_ode_2
#'
cr_ode_2 <- odin({
    deriv(R) <- r * R * (1 - R/K) - a/(1 + a * h * R) * R * C
    deriv(C) <- e * a /(1 + a * h * R) * R * C - m * C
    initial(R) <- R0
    initial(C) <- C0

    R0 <- user(10)
    C0 <- user(10)
    K <- user(100)
    r <- user(2)
    a <- user(1)
    e <- user(0.15)
    m <- user(0.6)
    h <- user(0.1)
}, verbose=FALSE)


#' CR model
#' Type III Functional Response
#' @export cr_ode_3
#'
cr_ode_3 <- odin({
    deriv(R) <- r * R * (1 - R/K) - a/(1 + a * h * R^2) * R^2 * C
    deriv(C) <- e * a /(1 + a * h * R^2) * R^2 * C - m * C
    initial(R) <- R0
    initial(C) <- C0

    R0 <- user(10)
    C0 <- user(10)
    K <- user(100)
    r <- user(2)
    a <- user(1)
    e <- user(0.15)
    m <- user(0.6)
    h <- user(0.1)
}, verbose=FALSE)
