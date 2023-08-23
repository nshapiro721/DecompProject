get_alpha <- function(pmr, t) -log(pmr) / t

get_mass_pct_remaining_at_t <- function(alpha, t) exp(-alpha * t)
