l0 <- 0
u0 <- 400
p0 <- 0.999
m0 <- 43.83
center0 <- "mode"

f <- function(a, m, l, u, center = "mode", p = 0.99) {
  if (center == "mode") b <- (a - 1) / m else
  if (center == "mean") b <- a / m
  pgamma(u, shape = a, rate = b) -
  pgamma(l, shape = a, rate = b) - p
}
a <- uniroot(f, interval = c(1, 5), m = m0,
               l = l0, u = u0,
               center = center0, p = p0)$root
if (center0 == "mode") b <- (a - 1) / m0 else b <- a / m0