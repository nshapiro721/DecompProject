get_alpha <- function(pmr, t) -log(pmr)/t

get_mass_pct_remaining_at_t <- function(alpha, t) exp(-alpha*t)

mor_eq <- function(x) exp(-0.0281*x)
pin_eq <- function(x) exp(-0.0109*x)
phrag_eq <- function(x) exp(-0.0121*x)
