def.par <- par(no.readonly = TRUE)
margins <- par("mar")

#--------------------------------------------------------------
# Conjugate Priors

pdf("~/Desktop/proj_imgs/final_set/cp_plots.pdf", width = 12, height = 9)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(mar = par("mar") + c(0, 2, 0, 0))

plot(1,0.01, ann = FALSE, axes = FALSE,
     xlim = c(0, 160), ylim = c(0, 0.05), 
     bty = "n", type = "n")
hist(dc.filt.m1$Kill_Rate, breaks = 30, freq = FALSE, axes = FALSE, ann = FALSE, add = TRUE, border = "grey75", col = "grey95")
lines(d1, col = "violetred", lwd = 1.5)
axis(1, at = seq.int(0, 160, 40))
axis(2, at = seq.int(0, 0.05, 0.01))

## Plot theoretical gamma distribution
curve(dgamma(x, shape = alpha.m1, rate = beta.m1), from = 0, to = 160, add = TRUE, lty = 2, col = "slateblue")
#legend("top", legend = paste(c("Mean =", "Mode =", "Variance ="), c(g.mean, g.mode, g.var)), bty = "n")
legend("top", legend = c(paste("Gamma(", alpha.m1, ", ", beta.m1,")", sep = ""), "KDE"), 
       col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 1", xlab = expression(theta), ylab = "Density")

plot(1,0.01, ann = FALSE, axes = FALSE,
     xlim = c(0, 160), ylim = c(0, 0.05), 
     bty = "n", type = "n")
hist(dc.filt.m2$Kill_Rate, breaks = 30, freq = FALSE, axes = FALSE, ann = FALSE, add = TRUE, border = "grey75", col = "grey95")
lines(d2, col = "violetred", lwd = 1.5)
axis(1, at = seq.int(0, 160, 40))
axis(2, at = seq.int(0, 0.05, 0.01))

## Plot theoretical gamma distribution
curve(dgamma(x, shape = alpha.m2, rate = beta.m2), from = 0, to = 160, add = TRUE, lty = 2, col = "slateblue")
#legend("top", legend = paste(c("Mean =", "Mode =", "Variance ="), c(g.mean, g.mode, g.var)), bty = "n")
legend("top", legend = c(paste("Gamma(", alpha.m2, ", ", beta.m2,")", sep = ""), "KDE"), 
       col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 2", xlab = expression(theta), ylab = "Density")

par(mar = margins)
plot(1,0.01, ann = FALSE, axes = FALSE,
     xlim = c(0, 400), ylim = c(0, 0.02), 
     bty = "n", type = "n")
hist(dc.filt.m3$Kill_Rate, freq = FALSE, axes = FALSE, ann = FALSE, add = TRUE, border = "grey75", col = "grey95")
lines(d3, col = "violetred", lwd = 1.5)
axis(1, at = seq.int(0, 400, 100))
axis(2, at = seq.int(0, 0.02, 0.005))

## Plot theoretical gamma distribution
curve(dgamma(x, shape = alpha.m3, rate = beta.m3), from = 0, to = 400, add = TRUE, lty = 2, col = "slateblue")
#legend("top", legend = paste(c("Mean =", "Mode =", "Variance ="), c(g.mean, g.mode, g.var)), bty = "n")
legend("top", legend = c(paste("Gamma(", alpha.m3, ", ", beta.m3,")", sep = ""), "KDE"), 
       col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 3", xlab = expression(theta), ylab = "Density")

dev.off()
par(def.par)
par(mar = margins)

#--------------------------------------------------------------
# Posterior Predictive Distribution

pdf("~/Desktop/proj_imgs/final_set/ppd_plots.pdf", width = 12, height = 9)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))


## Model 1
t.new <- seq.int(1.5, 3, 0.5)
beta.ppd.m1 <- beta.post.m1/t.new
post.preds <- mapply(dnbinom, x = 0:35, MoreArgs = list(size = rep(alpha.post.m1, length(t.new)), prob = (beta.ppd.m1/(beta.ppd.m1 + 1))))
x.modes.m1 <- apply(post.preds, 1, order, decreasing = TRUE)[1, ]
modes.m1 <- post.preds[matrix(c(1:4, x.modes.m1), nrow = 4)]
post.preds[post.preds < 1e-05] <- NA
col.vec <- c("violetred", "slateblue", "#d95f02", "#1b9e77")

par(mar = par("mar") + c(0, 2, 0, 0))
plot(1, 1, ann = FALSE, axes = FALSE, type = "n", lty = 3, col = "grey50", bty = "n", xlim = c(0, 35), ylim = c(0, 0.16))
for (i in 1:4) {
  lines(0:35, post.preds[i, ], pch = 20, col = col.vec[i], type = "b", lwd = 0.5)
  lines(rep(x.modes.m1[i] - 1, 2), c(-0.1, modes.m1[i]), col = "grey50", lty = 3)
}
axis(1, seq.int(0,35,5), col = "gray50")
axis(2, seq.int(0,0.16,0.04), col = "gray50")
title(main = "Model 1", xlab = expression(tilde(y)), ylab = expression(paste("Pr(", tilde(y), " | y)", sep = "")))
legend(25, 0.16, title = "Movie Duration (hours)", legend = t.new, col = col.vec, pch = 20, ncol = 2, bty = "n")
#text(x.modes.m1 + 3, modes.m1 + 0.04, labels = round(modes.m1, 3), pos = 4)
#arrows(x.modes.m1 - 1, modes.m1, x.modes.m1 + 3, modes.m1 + 0.04, length = 0.1, code = 1)
points(x.modes.m1 - 1, rep(0, length(x.modes.m1)), pch = 4, col = "black")


## Model 2
beta.ppd.m2 <- beta.post.m2/t.new
post.preds <- mapply(dnbinom, x = 0:140, MoreArgs = list(size = rep(alpha.post.m2, length(t.new)), prob = (beta.ppd.m2/(beta.ppd.m2 + 1))))
x.modes.m2 <- apply(post.preds, 1, order, decreasing = TRUE)[1, ]
modes.m2 <- post.preds[matrix(c(1:4, x.modes.m2), nrow = 4)]
post.preds[post.preds < 1e-05] <- NA
col.vec <- c("violetred", "slateblue", "#d95f02", "#1b9e77")

plot(1, 1, ann = FALSE, axes = FALSE, type = "n", lty = 3, col = "grey50", bty = "n", xlim = c(0, 140), ylim = c(0, 0.06))
for (i in 1:4) {
  lines(0:140, post.preds[i, ], pch = 20, col = col.vec[i], type = "b", lwd = 0.5)
  lines(rep(x.modes.m2[i] - 1, 2), c(-0.1, modes.m2[i]), col = "grey50", lty = 3)
}
axis(1, seq.int(0,160,40), col = "gray50")
axis(2, seq.int(0,0.06,0.02), col = "gray50")
title(main = "Model 2", xlab = expression(tilde(y)), ylab = expression(paste("Pr(", tilde(y), " | y)", sep = "")))
#legend("topright", title = "Movie Duration (hours)", legend = t.new, col = col.vec, pch = 20, ncol = 2, bty = "n")
#text(x.modes.m2 + 3, modes.m2 + 0.04, labels = round(modes.m2, 3), pos = 4)
#arrows(x.modes.m2 - 1, modes.m2, x.modes.m2 + 3, modes.m2 + 0.04, length = 0.1, code = 1)
points(x.modes.m2 - 1, rep(0, length(x.modes.m2)), pch = 4, col = "black")

beta.ppd.m3 <- beta.post.m3/t.new
post.preds <- mapply(dnbinom, x = 0:600, MoreArgs = list(size = rep(alpha.post.m3, length(t.new)), prob = (beta.ppd.m3/(beta.ppd.m3 + 1))))
x.modes.m3 <- apply(post.preds, 1, order, decreasing = TRUE)[1, ]
modes.m3 <- post.preds[matrix(c(1:4, x.modes.m3), nrow = 4)]
post.preds[post.preds < 1e-05] <- NA
col.vec <- c("violetred", "slateblue", "#d95f02", "#1b9e77")


## Model 3
par(mar = margins)
plot(1, 1, ann = FALSE, axes = FALSE, type = "n", lty = 3, col = "grey50", bty = "n", xlim = c(150, 600), ylim = c(0, 0.024))
for (i in 1:4) {
  lines(0:600, post.preds[i, ], pch = 20, col = col.vec[i], type = "b", lwd = 0.5)
  lines(rep(x.modes.m3[i] - 1, 2), c(-0.1, modes.m3[i]), col = "grey50", lty = 3)
}
axis(1, seq.int(0,600,100), col = "gray50")
axis(2, seq.int(0,0.024,0.006), col = "gray50")
title(main = "Model 3", xlab = expression(tilde(y)), ylab = expression(paste("Pr(", tilde(y), " | y)", sep = "")))
#legend("topright", title = "Movie Duration (hours)", legend = t.new, col = col.vec, pch = 20, ncol = 2, bty = "n")
#text(x.modes.m3 + 3, modes.m3 + 0.04, labels = round(modes.m3, 3), pos = 4)
#arrows(x.modes.m3 - 1, modes.m3, x.modes.m3 + 3, modes.m3 + 0.04, length = 0.1, code = 1)
points(x.modes.m3 - 1, rep(0, length(x.modes.m3)), pch = 4, col = "black")
dev.off()

par(def.par)


#--------------------------------------------------------------
# Posterior Predictive Checking

pdf("~/Desktop/proj_imgs/final_set/ppc_plots.pdf", width = 12, height = 9)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

par(mar = par("mar") + c(0, 2, 0, 0))
## Var/Mean Ratio
h <- hist(sim.ratio, breaks = 100, right = FALSE, freq = FALSE, ann = FALSE, col = "#1f78b4", border = "#a6cee3", xlim = c(0,8), ylim = c(0,1))
lines(rep(obs.ratio, 2), c(0, max(h$density)), col = "#e31a1c", lwd = 1.5)
mtext(paste("p-value:", round(pval.ratio, 2)), side = 3, cex = 0.75)
text(obs.ratio, max(h$density), round(obs.ratio, 2), pos = 3)
title(main = "Var/Mean Ratio", line = 1)
title(xlab = expression(paste("T(y, ", theta, ")", sep = "")), ylab = "Density")

## Minimum
h <- hist(sim.min, right = FALSE, freq = FALSE, ann = FALSE, col = "#1f78b4", border = "#a6cee3", xlim = c(0,25), ylim = c(0,0.20))
lines(rep(obs.min, 2), c(0, max(h$density)), col = "#e31a1c", lwd = 1.5)
mtext(paste("p-value:", round(pval.min, 2)), side = 3, cex = 0.75)
text(obs.min, max(h$density),  obs.min, pos = 3, offset = 0.5)
title(main = "Minimum", line = 1)
title(xlab = expression(paste("T(y, ", theta, ")", sep = "")), ylab = "Density")

par(mar = margins)
## Maximum
h <- hist(sim.max, right = FALSE, freq = FALSE, ann = FALSE, col = "#1f78b4", border = "#a6cee3", xlim = c(0,40), ylim = c(0, 0.16))
lines(rep(obs.max, 2), c(0, max(h$density)), col = "#e31a1c", lwd = 1.5)
mtext(paste("p-value =", round(pval.max, 2)), side = 3, cex = 0.75)
text(obs.max, max(h$density), obs.max, pos = 3)
title(main = "Maximum", line = 1)
title(xlab = expression(paste("T(y, ", theta, ")", sep = "")), ylab = "Density")

dev.off()
par(def.par)


#--------------------------------------------------------------
# Posterior distribution

pdf("~/Desktop/proj_imgs/final_set/posteriors.pdf", width = 12, height = 6)

par(mfrow = c(1,3))
## Model 1
t.sum.m1 <- sum(qt.m1$hours)
y.sum.m1 <- sum(qt.m1$body.count)
alpha.post.m1 <- alpha.m1 + y.sum.m1
beta.post.m1 <- beta.m1 + t.sum.m1
curve(dgamma(x, shape = alpha.post.m1, rate = beta.post.m1), from = 0, to = 20, lty = 1, col = "violetred", ann = FALSE, bty = "n", ylim = c(0,0.8))
curve(dgamma(x, shape = alpha.m1, rate = beta.m1), from = 0, to = 20, lty = 2, col = "slateblue", add = TRUE)
#legend("topright", inset = 0.05, legend = c("Prior", "Posterior"), col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 1", xlab = expression(theta), ylab = "Density")

## Model 2
t.sum.m2 <- sum(qt.m2$hours)
y.sum.m2 <- sum(qt.m2$body.count)
alpha.post.m2 <- alpha.m2 + y.sum.m2
beta.post.m2 <- beta.m2 + t.sum.m2
curve(dgamma(x, shape = alpha.post.m2, rate = beta.post.m2), 
      from = 0, to = 100, n = 500, lty = 1, col = "violetred", ann = FALSE, bty = "n", ylim = c(0, 0.2))
curve(dgamma(x, shape = alpha.m2, rate = beta.m2), 
      from = 0, to = 100, n = 500, lty = 2, col = "slateblue", add = TRUE)
legend("top", inset = 0.05, legend = c("Prior", "Posterior"), col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 2", xlab = expression(theta), ylab = "Density")

## Model 3
t.sum.m3 <- sum(qt.m3$hours)
y.sum.m3 <- sum(qt.m3$body.count)
alpha.post.m3 <- alpha.m3 + y.sum.m3
beta.post.m3 <- beta.m3 + t.sum.m3
curve(dgamma(x, shape = alpha.post.m3, rate = beta.post.m3), 
      from = 0, to = 300, n = 1000, lty = 1, col = "violetred", ann = FALSE, bty = "n", ylim = c(0,0.06))
curve(dgamma(x, shape = alpha.m3, rate = beta.m3), 
      from = 0, to = 300, n = 1000, lty = 2, col = "slateblue", add = TRUE)
#legend("topright", inset = 0.05, legend = c("Prior", "Posterior"), col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 3", xlab = expression(theta), ylab = "Density")
par(mfrow = c(1,1))
dev.off()


#--------------------------------------------------------------
# Sensitivity to priors

pdf("~/Desktop/proj_imgs/final_set/jeffreys.pdf", width = 12, height = 6)


par(mfrow = c(1,3))
curve(dgamma(x, shape = y.sum.m1 + 0.5, rate = t.sum.m1), 
      from = 0, to = 8, 
      lty = 1, col = "violetred", ann = FALSE, bty = "n", xlim = c(0,8), ylim = c(0,0.8))
curve(dgamma(x, shape = alpha.post.m1, rate = beta.post.m1), 
      from = 0, to = 8, 
      lty = "36", lwd = 2, col = "slateblue", add = TRUE)
#legend("topright", legend = c("Posterior (Conjugate Prior)", "Posterior (Noninformative Prior)"), col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 1", xlab = expression(theta), ylab = "Density")


curve(dgamma(x, shape = y.sum.m2 + 0.5, rate = t.sum.m2), 
      from = 15, to = 40, 
      lty = 1, col = "violetred", ann = FALSE, bty = "n", xlim = c(15,40), ylim = c(0,0.2))
curve(dgamma(x, shape = alpha.post.m2, rate = beta.post.m2), 
      from = 15, to = 40, 
      lty = "36", lwd = 2, col = "slateblue", add = TRUE)
legend("top", legend = c("Conjugate", "Noninformative"), col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 2", xlab = expression(theta), ylab = "Density")


curve(dgamma(x, shape = y.sum.m3 + 0.5, rate = t.sum.m3), 
      from = 120, to = 200, 
      lty = 1, col = "violetred", ann = FALSE, bty = "n", xlim = c(120,200), ylim = c(0,0.06))
curve(dgamma(x, shape = alpha.post.m3, rate = beta.post.m3), 
      from = 120, to = 200, 
      lty = "36", lwd = 2, col = "slateblue", add = TRUE)
#legend("topright", legend = c("Posterior (Conjugate Prior)", "Posterior (Noninformative Prior)"), col = c("slateblue", "violetred"), lty = c(2,1), bty = "n")
title(main = "Model 3", xlab = expression(theta), ylab = "Density")
par(mfrow = c(1,1))

dev.off()