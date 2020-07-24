# calculate the mean of the beta density given coefficients and testvalues

mean_beta <- function(betaobj, testvals, brange=c(0,1)) {
 nu <- betaobj$coef_nu
 omega <- betaobj$coef_omega

 xb_nu <- nu %*% testvals
 xb_omega <- omega %*% testvals

 rval <- exp(xb_nu)/(exp(xb_nu) + exp(xb_omega))
 rval*(brange[2]-brange[1])+brange[1]
}
