## beta via MLE in R

#' Title
#'
#' @param fmu formula object
#' @param method character string
#' @param data dataset
#' @param brange vector
#'
#' @export
#'
betamle <-function(fmu,method='BFGS', data, brange){

    attach(data, warn.conflicts=FALSE)

    Fmu <- terms(fmu)
    X.vars <- as.character(attr(Fmu, "variables"))[c(-1, -2)]
    X.data <- model.matrix(fmu)

    Y.var <- as.character(attr(Fmu, "variables"))[2]
    Y.data <- model.frame(fmu)[,1]

    setrange <- function(z, brange) {
      t <- (z-brange[1])/(brange[2]-brange[1])
      t[which(t == 0)] <- .001
      t[which(t == 1)] <- .999
      t
    }

    dv <- setrange(Y.data, brange)
    nx <- ncol(X.data)

    neglnl<-function(theta){
	    nu <- theta[1:nx]
	    omega <- theta[(nx+1):(2*nx)]

	    X <- X.data


	    p1 <-exp(as.vector(X%*%nu))
	    p2 <- exp(as.vector(X%*%omega))

	    -sum(lgamma(p1 + p2) + (p1-1)*log(dv) + (p2-1)*log(1-dv) - lgamma(p1) - lgamma(p2) )
	   }

    result <- optim(rep(0,2*nx), neglnl, hessian=T, method="BFGS")

    coef<-result$par
    coef_nu <- coef[1:nx]
    coef_omega <- coef[(nx+1):(2*nx)]
    var<-solve(result$hessian)
    var_nu <- var[1:nx]
    var_omega <- var[(nx+1):(2*nx)]
    se<-sqrt(diag(var))
    se_nu <- se[1:nx]
    se_omega <- se[(nx+1):(2*nx)]
    zscore<-coef/se
    zscore_nu <- zscore[1:nx]
    zscore_omega <- zscore[(nx+1):(2*nx)]
    pvalue<-2.0*(1.0-pnorm(abs(zscore)))
    pscore_nu <- pvalue[1:nx]
    pscore_omega <- pvalue[(1+nx):(2*nx)]

    res <- cbind(coef_nu, se_nu, zscore_nu, pscore_nu,
                 coef_omega, se_omega, zscore_omega, pscore_omega)
	  colnames(res) <- c("Nu Coef.","Nu SE","Nu Z","Nu p", "Omega Coef", "Omega SE", "Omega Z", "Omega p")
	  rownames(res) <- c("constant",X.vars)

    cat("\n",paste("Beta MLE Model of",Y.var),"\n")

	  print(round(res,2))
    if (print(result$convergence==0))
	    cat("\nLikelihood:",-result$value,"\n")
    else
	    cat("\nDid NOT converge\n")

	  rval <- list("coef"=coef,
	               "coef_nu"=coef_nu,
	               "coef_omega"=coef_omega,
	               "var"=var,
	               "var_nu"=var_nu,
	               "var_omega"=var_omega,
	               "results" = res,
	               "Likelihood"=-result$value)
	  rval
  }

