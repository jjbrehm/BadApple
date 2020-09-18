# produces table of first differences,
#  - input is betaobj, brange, base, test
#  - calculates a baseline effect at "base"
#  - swaps out each element of the test vector into "base", calculate test effect for that element

# I just don't know why this isn't up. grumble

effs_beta <- function(betaobj, brange=c(0,1), base, test) {
  baseline <- mean_beta(betaobj, base, brange=brange)

  rval <- rep(NA, length(test))

  testvals <- base
  for (i in 1:length(test)) {
    testvals[i] <- test[i]
    rval[i] <- mean_beta(betaobj, testvals, brange=c(-1,1))
    testvals <- base
  }

  rval-rep(baseline, length(test))
}
