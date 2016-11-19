dw.10 <- read.csv("~/desktop/dw_10.csv", header = TRUE, colClasses = c("factor", "integer"))
dw.11 <- read.csv("~/desktop/dw_11.csv", header = TRUE, colClasses = c("factor", "integer"))

barplot(dw.10$count, names.arg = dw.10$x)
barplot(dw.11$count, names.arg = dw.11$x)