f <- function(a, m, l, u, center = "mode", p = 0.99) {
  if (center == "mode") b <- (a - 1) / m else
  if (center == "mean") b <- a / m
  pgamma(u, shape = a, rate = b) -
  pgamma(l, shape = a, rate = b) - p
}

# Model 1

l0 <- 0
u0 <- 150
p0 <- 0.999
m0 <- d1$x[d1$y == max(d1$y)]
center0 <- "mode"

a <- uniroot(f, interval = c(1, 5), m = m0,
               l = l0, u = u0,
               center = center0, p = p0)$root
if (center0 == "mode") b <- (a - 1) / m0 else b <- a / m0
cat("mode =", m0, "; a =", a, "; b = ", b)

# Model 2
l0 <- 0
u0 <- 150
p0 <- 0.999
m0 <- d2$x[d2$y == max(d2$y)]
center0 <- "mode"

a <- uniroot(f, interval = c(1, 5), m = m0,
             l = l0, u = u0,
             center = center0, p = p0)$root
if (center0 == "mode") b <- (a - 1) / m0 else b <- a / m0
cat("mode =", m0, "; a =", a, "; b = ", b)

# Model 3
l0 <- 0
u0 <- 400
p0 <- 0.999
m0 <- d3$x[d3$y == max(d3$y)]
center0 <- "mode"

a <- uniroot(f, interval = c(1, 5), m = m0,
             l = l0, u = u0,
             center = center0, p = p0)$root
if (center0 == "mode") b <- (a - 1) / m0 else b <- a / m0
cat("mode =", m0, "; a =", a, "; b = ", b)