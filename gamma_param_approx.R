l0 <- 0
u0 <- 1.44
p0 <- 0.975
center0 <- "mode"

f <- function(a, m, l, u, center = "mode", p = 0.99) {
  if (center == "mode") b <- (a - 1) / m else
  if (center == "mean") b <- a / m
  pgamma(u, shape = a, rate = b) -
  pgamma(l, shape = a, rate = b) - p
}
a <- uniroot(f, interval = c(2, 4), m = m0,
               l = l0, u = u0,
               center = center0, p = p0)$root
if (center0 == "mode") b <- (a - 1) / m0 else b <- a / m0