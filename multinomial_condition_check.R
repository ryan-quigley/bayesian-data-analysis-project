data.m1 <- c(11,7,4,13,6,18)
llk.check <- rmultinom(n = 100000, prob = rep(1/6, 6), size = 59)


count.sort <- 0
count.true <- 0
for (i in 1:100000) {
	if (all(sort(llk.check[, i]) == sort(data))) {
		count.sort <- count.sort + 1
		print(llk.check[, i])
	}
	if (all(llk.check[, i] == data)) {
		count.true <- count.true + 1
	}
}
count.sort
count.true

data.sorted <- sort(data)
f <- function(x) all(sort(x) == data.sorted)
( test <- sum(apply(llk.check, 2, f)) )


llk.vec <- llk.check
dim(llk.vec) <- NULL
llk.vec <- as.factor(llk.vec)
ct <- table(llk.vec)

margins <- par("mar")

pdf("~/Desktop/proj_imgs/final_set/mult_draw.pdf", width = 12, height = 9)
par(mar = par("mar") + c(0, 2, 0, 0))
plot(llk.vec)
plot(llk.vec[llk.vec %in% data.m1], add = TRUE, col = "violetred")
legend("topright", inset = 0.05, legend = "Observed Body Count", fill = "violetred", bty = "n")
title(main = c("100,000 Multinomial Draws", "59 bodies 6 movies"), xlab = "Bin Count", ylab = "Frequency")
dev.off()

par(mar = margins)


