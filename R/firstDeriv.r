#' @name firstDeriv
#' @title First Derivative Function(s)
#' @param mod  The gam model.
#' @param n   Prediction increments.
#' @keywords internal
#' @export

firstDeriv <- function(mod, n) {

    if(isTRUE(all.equal(class(mod), "list")))
    
    mod <- mod$gam
    eps <- 1e-7
    alpha <- .05
    m.terms <- attr(terms(mod), "term.labels")
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                       function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms

    X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
    newD <- newD + eps
    X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
    Xp <- (X1 - X0) / eps
    Xp.r <- NROW(Xp)
    Xp.c <- NCOL(Xp)
    ## dims of bs
    bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
    ## number of smooth terms
    t.labs <- attr(mod$terms, "term.labels")
    nt <- length(t.labs)
    ## list to hold the derivatives
    lD <- vector(mode = "list", length = nt)
    names(lD) <- t.labs
    for(i in seq_len(nt)) 
	{
        Xi <- Xp * 0
        want <- grep(t.labs[i], colnames(X1))
        Xi[, want] <- Xp[, want]
        df <- Xi %*% coef(mod)
        df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
        lD[[i]] <- list(deriv = df, se.deriv = df.sd)
    	}
    class(lD) <- "firstDeriv"
    lD$gamModel <- mod
    lD$eps <- eps
    lD$eval <- newD - eps
    return(lD)
}
