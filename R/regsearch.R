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
  print("Gathering variables...")
  if(interactions) {
    independent <- sort(independent)
    independent <- c(pbapply(cl = clust, do.call(rbind,
                                                 combn(independent, 2, simplify = FALSE)),
                             1,
                             function(x) { paste(x, collapse = "*")}),
                     independent)

    combs <- rbindlist(pbsapply(cl = clust, minvar:maxvar,
                                function(x) {
                                  data.table(do.call(rbind,
                                                     combn(independent,
                                                           x,
                                                           simplify = FALSE)))
                                }),
                       fill = TRUE)
  } else {
    independent <- sort(independent)

    combs <- rbindlist(pbsapply(minvar:maxvar,
                                function(x) {
                                  data.table(do.call(rbind,
                                                     combn(independent,
                                                           x,
                                                           simplify = FALSE)))
                                }),
                       fill = TRUE)
  }

  # add column for Intercept and AIC
  reg <- data.table(matrix(data = "", nrow = 1, ncol = length(independent) + 4))
  colnames(reg) <- c("aic", "rSquare", "warn", "X.Intercept.",
                     gsub("\\*", ".", independent))

  print("Creating regressions...")
  if (multi) {
    clusterExport(clust, c("combs"), envir = environment())
    forms <- pbapply(cl = clust, combs, 1, function(x) {
      comb <- as.character(x)
      as.formula(gsub(" \\+$", "",
                      paste(
                        paste(dependent, "~"),
                        paste(comb[!is.na(comb)], "+", collapse = " "),
                        collapse = " "
                      )))

    })
  } else {
    forms <- pbapply(combs, 1, function(x) {
      comb <- as.character(x)
      as.formula(gsub(" \\+$", "",
                      paste(
                        paste(dependent, "~"),
                        paste(comb[!is.na(comb)], "+", collapse = " "),
                        collapse = " "
                      )))
    })
  }
  forms <- as.character(unlist(forms, recursive = TRUE))
  # return(forms)
  # if(multi) {
  #   forms <- forms[!parSapplyLB(clust, forms, function(x) {
  #     any(duplicated(unlist(strsplit(gsub("[~\\+\\*]", "", x), "  "))))
  #     })]
  # } else {
  #   forms <- forms[!sapply(forms, function(x) {
  #     any(duplicated(unlist(strsplit(gsub("[~\\+\\*]", "", x), "  "))))
  #     })]
  # }

  summFunc <- function(x) {
    summ <- summary(glm(formula = as.formula(x), data = fDT, family = family))
    coefs <- t(summ$coefficients[,4])
    names(coefs) <- gsub(":", ".", names(coefs))
    summ <- data.frame("aic" = summ$aic,
                       "rSquare" = round(1 - (summ$deviance / summ$null.deviance), 5),
                       "warn" = FALSE,
                       coefs)
    data.frame(rbindlist(list(reg, summ), fill = TRUE))
  }

  print("Running regressions...")
  if(multi) {
    clusterEvalQ(clust, library(data.table))
    clusterExport(clust, c("forms", "family", "fDT"), envir = environment())
    regs <- pbsapply(cl = clust, forms, summFunc)
  } else {
    regs <- pbsapply(forms, summFunc)
  }

  print("Cleaning the output...")
  regs <- data.table(t(regs))
  if(multi) {
    regs <- data.frame("formula" = forms, pblapply(cl = clust, regs, function(x) {
      as.numeric(gsub("[c\\(\", \\)]", "", x))
    }))
  } else {
    regs <- data.frame("formula" = forms, pblapply(regs, function(x) {
      as.numeric(gsub("[c\\(\", \\)]", "", x))
    }))
  }
  regs <- data.table(regs)
  regs$rank <-
    regs$rSquare / rowMeans(regs[,!c("formula", "aic", "rSquare", "warn", "X.Intercept.")], na.rm = TRUE)
  regs <- regs[order(desc(rank)), !c("rank")]

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
