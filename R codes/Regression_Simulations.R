# Code for Figure 2
a = 0.5
b = 1
sigma = 0.05
set.seed(78)
n_vals = seq(5, 10000, by = 5)

# vertical
a_hat_v = numeric(length = length(n_vals))
b_hat_v = numeric(length = length(n_vals))
# horizontal
a_hat_h = numeric(length = length(n_vals))
b_hat_h = numeric(length = length(n_vals))
# orthogonal
a_hat_o = numeric(length = length(n_vals))
b_hat_o = numeric(length = length(n_vals))


for (i in 1:length(n_vals)) {
   n = n_vals[i]
   x = seq(0, 1, length.out = n)
   y = (a + b*x) + rnorm(n = n, mean = 0, sd = sqrt(sigma))
   sxx = sum((x - mean(x))^2)
   syy = sum((y - mean(y))^2)
   sxy = sum((x - mean(x))*(y - mean(y)))
   # vertical
   b_hat_v[i] = sxy/sxx
   a_hat_v[i] = mean(y) - b_hat_v[i]*mean(x)
   # horizontal
   b_hat_h[i] = (syy/sxy)^(1)
   a_hat_h[i] = mean(y) - b_hat_h[i]*mean(x)
   # orthogonal
   b_hat_o[i] = (syy - sxx + sqrt((syy - sxx)^2 + 4 * sxy^2)) / (2 * sxy)
   a_hat_o[i] = mean(y) - b_hat_o[i]*mean(x)
}
par(mfrow = c(2, 3))
plot(n_vals, a_hat_h, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main =expression(widehat(a["n"])^"h"))
abline(h = a, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("a = ", a), col = "red", lwd = 3, 
       lty = 2, bty = "n")
plot(n_vals, a_hat_v, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main =expression(widehat(a["n"])^"v"))
abline(h = a, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("a = ", a), col = "red", lwd = 3, 
       lty = 2,  bty = "n")
plot(n_vals, a_hat_o, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = , 
     main = expression(widehat(a["n"])^"o"))
abline(h = a, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("a = ", a), col = "red", lwd = 3, 
       lty = 2, bty = "n")
plot(n_vals, b_hat_h, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main = expression(widehat(b["n"])^"h"))
abline(h = b, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("b = ", b), col = "red", lwd = 3, 
       lty =2, bty = "n")
plot(n_vals, b_hat_v, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main =expression(widehat(b["n"])^"v"))
abline(h = b, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("b = ", b), col = "red", lwd = 3, 
       lty = 2, bty = "n")
plot(n_vals, b_hat_o, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "",
     main =expression(widehat(b["n"])^"o"))
abline(h = b, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("b = ", b), col = "red", lwd = 3, 
       lty = 2, bty = "n")

# code for figure 3
a = 0.5
b = 1
sigma = 0.05
set.seed(123)
n_vals = seq(5, 10000, by = 5)
a_hat_h = numeric(length = length(n_vals))
b_hat_h = numeric(length = length(n_vals))
for (i in 1:length(n_vals)) {
   n = n_vals[i]
   y = seq(0, 1, length.out = n)
   x = (-(a/b)+ (y/b)) + rnorm(n = n, mean = 0, sd = sqrt(sigma))
   sxx = sum((x - mean(x))^2)
   syy = sum((y - mean(y))^2)
   sxy = sum((x - mean(x))*(y - mean(y)))
   b_hat_h[i] = (syy/sxy)
   a_hat_h[i] = mean(y) - b_hat_h[i]*mean(x)
}
par(mfrow = c(1, 2))
plot(n_vals, a_hat_h, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main =expression(widehat(a["n"])^"h"))
abline(h = a, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("a = ", a), col = "red", lwd = 3, 
       lty = 2, bty = "n")
plot(n_vals, b_hat_h, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main = expression(widehat(b["n"])^"h"))
abline(h = b, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("b = ", b), col = "red", lwd = 3, 
       lty = 2, bty = "n")

# code for figure 4
a = 0.5
b = 1
sigma_delta = 0.05
sigma_eps = 0.05
set.seed(143)
n_vals = seq(5, 10000, by = 5)
a_hat_o = numeric(length = length(n_vals))
b_hat_o = numeric(length = length(n_vals))

for (i in 1:length(n_vals)) {
   n = n_vals[i]
   zeta = seq(0, 1, length.out = n)
   x = zeta + rnorm(n = n, mean = 0, sd = sqrt(sigma_delta))
   y = a + b*zeta + rnorm(n = n, mean = 0,  sd = sqrt(sigma_eps))
   sxx = sum((x - mean(x))^2)
   syy = sum((y - mean(y))^2)
   sxy = sum((x - mean(x))*(y - mean(y)))
   b_hat_o[i] = (syy - sxx + sqrt((syy - sxx)^2 + 4 * sxy^2)) / (2 * sxy)
   a_hat_o[i] = mean(y) - b_hat_o[i]*mean(x)
}
par(mfrow = c(1, 2))
plot(n_vals, a_hat_o, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "", 
     main = expression(widehat(a["n"])^"o"))
abline(h = a, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("a = ", a), col = "red", lwd = 3, 
       lty = 2, bty = "n")
plot(n_vals, b_hat_o, type = "l", col = "darkgrey", lwd = 2, 
     xlab = "sample size (n)", ylab = "",
     main =expression(widehat(b["n"])^"o"))
abline(h = b, col = "red", lwd = 3, lty = 2)
legend("topright", legend = paste("b = ", b), col = "red", lwd = 3, 
       lty = 2, bty = "n")
