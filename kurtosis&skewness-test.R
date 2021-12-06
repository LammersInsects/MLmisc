#for kurtosis
kurtosis.test <- function (x) {
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  kurt <- (m4/s4) - 3
  sek <- sqrt(24/length(x))
  totest <- kurt/sek
  pvalue <- pt(totest,(length(x)-1))
  print(pvalue)
  print('Both these tests are one-tailed, so you will need to multiply the p-value by 2 to become two-tailed. If your p-value become larger than one you will need to use 1-kurtosis.test() instead of kurtosis.test.')
}

#for skewness
skew.test <- function (x) {
  m3 <- sum((x-mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  skew <- m3/s3
  ses <- sqrt(6/length(x))
  totest <- skew/ses
  pt(totest,(length(x)-1))
  pval <- pt(totest,(length(x)-1))
  print(pval)
  print('Both these tests are one-tailed, so you will need to multiply the p-value by 2 to become two-tailed. If your p-value become larger than one you will need to use 1-kurtosis.test() instead of kurtosis.test.')
  }

#Both these tests are one-tailed,
#so you'll need to multiply the p-value by 2 to become two-tailed.
#If your p-value become larger than one
#you'll need to use 1-kurtosis.test() instead of kurtosis.test.
