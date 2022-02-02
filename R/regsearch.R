regsearch <-
  function(fDT,
           dependent,
           independent,
           minvar = 1,
           maxvar,
           family,
           topN = 0,
           interactions = FALSE,
           multi = FALSE) {

  if (!requireNamespace(c("parallel", "pbapply"), quietly = TRUE)) {
    multi <- FALSE
    stop("Packages \"parallel\" and \"pbapply\" must be installed.",
         call. = FALSE)
  }

  if(multi) { clust <- makeCluster(detectCores()) }
  # Get every combination of the independent variables
  varNames <- ifelse(sapply(fDT, is.factor), paste0(names(fDT), "_"), names(fDT))
  if(interactions) {
    independent <- sort(independent)
    print("Gathering variables...")
    independent <- c(pbapply(cl = clust, do.call(rbind,
                                                 combn(independent, 2, simplify = FALSE)),
                             1,
                             function(x) { paste(x, collapse = "*")}),
                     independent)

    print("Assembling regresions...")
    combs <- rbindlist(pblapply(cl = clust,
                                minvar:maxvar,
                                function(x) {
                                  data.table(t(combn(independent, x))) }),
                       fill = TRUE)
  } else {
    independent <- sort(independent)

    print("Assembling regresions...")
    combs <- rbindlist(pblapply(minvar:maxvar,
                                function(x) {
                                  data.table(t(combn(independent, x))) }),
                       fill = TRUE)
  }

  # add column for Intercept and AIC
  reg <- data.frame(matrix(data = NA, nrow = 0, ncol = length(varNames) + 4))
  colnames(reg) <- c("aic", "rSquare", "warn", "X.Intercept.",
                     gsub("\\*", ".", varNames))
  reg <- reg[-1,]

  print("Creating regressions...")
  if (multi) {
    clusterExport(clust, c("combs"), envir = environment())
    forms <- pbapply(cl = clust, combs, 1, function(x) {
      comb <- as.character(x)
      comb <- paste(paste("route", "~"),
                    paste("+", comb[!is.na(comb)], collapse = " "),
                    collapse = " ")
      comb
    })
  } else {
    forms <- pbapply(combs, 1, function(x) {
      comb <- as.character(x)
      comb <- paste(paste("route", "~"),
                    paste("+", comb[!is.na(comb)], collapse = " "),
                    collapse = " ")
      comb
    })
  }

  summFunc <- function(x) {
    summ <- summary(glm(formula = as.formula(x), data = fDT, family = family))
    coefs <- t(summ$coefficients[,4])
    names(coefs) <- gsub(":", ".", names(coefs))
    summ <- data.frame("aic" = summ$aic,
                       "rSquare" = round(1 - (summ$deviance / summ$null.deviance), 5),
                       "warn" = FALSE,
                       coefs)
    rbindlist(list(reg, summ), fill = TRUE)
  }

  print("Running regressions...")
  if(multi) {
    clusterEvalQ(clust, library(data.table))
    clusterExport(clust, c("forms", "family", "fDT"), envir = environment())
    regs <- rbindlist(pblapply(cl = clust, forms, summFunc), fill = TRUE)
  } else {
    regs <- rbindlist(pblapply(forms, summFunc), fill = TRUE)
  }


  regs <- data.table("formula" = forms, regs)
  setnames(regs, "X.Intercept.", "xIntercept")
  regs$rowMeans <- rowMeans(regs[,!c("formula", "aic", "rSquare", "warn", "xIntercept")], na.rm = TRUE)
  regs$rank <- regs$rSquare / regs$rowMeans
  regs <- regs[order(desc(rank)), !c("rank", "rowMeans")]

  if(multi) { stopCluster(clust) }

  if (topN > 0) {
    sapply(regs[1:topN, formula], function(x) {
      print(summary(glm(
        formula = as.formula(x),
        data = fDT,
        family = family
      )))
    })
  }

  return(regs)
}
