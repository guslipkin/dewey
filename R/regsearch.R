#' An exhaustive search regression built on base R
#'
#' @export
#'
#' @param data A `data.frame` that contains a dependent variable and the
#'   independent variables.
#' @param dependent The dependent variable for the regression.
#' @param independent A vector of independent variables to be used. These must
#'   match the column names from `data`. These can also include interaction
#'   terms made from column names from `data`. This allows for specific
#'   interaction terms to be used, rather than every possible interaction as is
#'   done with `interactions = TRUE`.
#' @param minvar (Optional) The minimum number of independent variables to be
#'   used in the regression. Defaults to 1.
#' @param maxvar The maximum number of independent variables to be used in the
#'   regression. Must be equal to or less than the number of independent
#'   variables. If interaction terms are used, they count as one independent
#'   variable.
#' @param family The type of regression. Passed to `glm`. See
#'   \code{\link[stats:glm]{glm}} for more information.
#' @param topN (Optional) The number of top results to be printed upon run
#'   completion. Defaults to 0.
#' @param interactions (Optional) A boolean indicating whether or not
#'   interaction terms should be used. Defaults to `FALSE`.
#' @param multi (Optional) A boolean indicating whether or not multithreading
#'   should be used. Defaults to `FALSE`. It is highly recommended to use
#'   multithreading.
#'
#' @return Returns a `data.table` of information on the regressions run. The
#'   resulting data.table is sorted in descending order by the rSquare divided
#'   by the mean p-value. This is generally reliable in pushing quality
#'   regressions to the top of the list. \item{`formula`}{The regression formula
#'   used.} \item{`aic`}{The aic for the regression.} \item{`rSquare`}{The
#'   calculated r-square for the regression.} \item{`warn`}{Currently unused.}
#'   \item{independent}{Each variable column contains the p-values for that
#'   variable or interaction term in a given regression.}
#'
#' @import data.table pbapply parallel
#' @importFrom stats as.formula formula glm
#' @importFrom utils combn
#'
#' @examples
#' # Creating dummy data
#' dt <- data.frame("dependent" = sample(c(0, 1), 100, replace = TRUE),
#' "ind_1" = runif(100, 0, 1),
#' "ind_2" = runif(100, 0, 1),
#' "ind_3" = runif(100, 0, 1),
#' "ind_4" = runif(100, 0, 1))
#'
#' # Without interaction terms and multithreading
#' ## Two top results
#' regsearch(dt, "dependent", c("ind_1", "ind_2", "ind_3", "ind_4"),
#' 1, 4, "binomial", 2)
#' ## No top results
#' regsearch(dt, "dependent", c("ind_1", "ind_2", "ind_3", "ind_4"),
#' 1, 4, "binomial", FALSE, FALSE)
#'
#' # With interaction terms and multithreading
#' regsearch(dt, "dependent", c("ind_1", "ind_2", "ind_3", "ind_4"),
#' 1, 4, "binomial", TRUE, TRUE)
regsearch <- function(data,
                      dependent, independent,
                      minvar = 1, maxvar,
                      family,
                      topN = 0,
                      interactions = FALSE,
                      multi = FALSE) {

  # warnings for missing variables in function call
  if(missing(interactions)) {
    warning("Missing 'interactions' argument. Defaulting to FALSE.")
    interactions <- FALSE
    }
  if(missing(multi)) {
    warning("Missing 'multi' argument. Defaulting to FALSE.")
    multi <- FALSE
    }

  # stop if `interactions` or `multi` are not logical
  if(!interactions %in% c(TRUE, FALSE) | !multi %in% c(TRUE, FALSE))
    stop("Arguments 'interactions' and 'multi' must be logical TRUE/FALSE")

  # stop if `parallel` or `pbapply` are not installed
  # `parallel` should always be installed beacuse it is part of base R installs
  if (!requireNamespace(c("parallel", "pbapply"), quietly = TRUE)) {
    stop("Packages \"parallel\" and \"pbapply\" must be installed.",
         call. = FALSE)
  }

  maxvarTest <- length(independent) + ifelse(interactions, choose(length(independent), 2), 0)
  if(maxvar > maxvarTest) {
    warning(paste("`maxvar` is greater than the number of independent variables. Setting maxvar to",
                  maxvarTest, collapse = ""))
    maxvar <- maxvarTest
  }

  # create the cluster for multithreading
  if(multi) { clust <- makeCluster(detectCores()) }

  # Get every combination of the independent variables
  # append underscores to factor variables so that when the regression is run,
  #   they will show as `factor_1`, `factor_2`, and so on
  varNames <- ifelse(sapply(data, is.factor), paste0(names(data), "_"), names(data))
  # create and sort the list of independent variables
  if(interactions) {
    independent <- sort(independent)
    # create a vector that includes every possible combination of
    #   interaction terms and the independent variables
    print("Gathering variables...")
    if (multi) {
      independent <- c(unlist(pblapply(cl = clust,
                                       combn(independent, 2, simplify = FALSE),
                                       function(x) { paste(x, collapse = "*") }),
                              recursive = FALSE),
                       independent)
    } else {
      print("WARNING: Using interaction terms without multithreading may take a very long time")
      independent <- c(unlist(pblapply(combn(independent, 2, simplify = FALSE),
                                       function(x) { paste(x, collapse = "*") }),
                              recursive = FALSE),
                       independent)
    }
  } else {
    independent <- sort(independent)
  }

  # create the formulas that will be used for the regressions
  # when an interaction term is encountered, any superfluous terms are dropped
  #   i.e. `a*b + a + b` is dropped in favor of `a*b` since `+ a + b` is implied
  print("Assembling regresions...")
  if(multi) {
    combs <- pblapply(cl = clust, minvar:maxvar,
                      function(x) {
                        y <- combn(independent, x, simplify = FALSE)
                        lapply(y, function(x) {
                          x <- x[!is.na(x)]
                          expanded <-
                            paste0(unlist(strsplit(x[grepl("\\*", x)], "\\*")),
                                   "")
                          notExpanded <-
                            paste0(x[!grepl("\\*", x)], "")
                          if (sum(notExpanded %in% expanded) == 0)
                            return(x)
                          else
                            return(NULL)
                        })
                      })
  } else {
    combs <- pblapply(minvar:maxvar,
                      function(x) {
                        y <- combn(independent, x, simplify = FALSE)
                        lapply(y, function(x) {
                          x <- x[!is.na(x)]
                          expanded <-
                            paste0(unlist(strsplit(x[grepl("\\*", x)], "\\*")),
                                   "")
                          notExpanded <-
                            paste0(x[!grepl("\\*", x)], "")
                          if (sum(notExpanded %in% expanded) == 0)
                            return(x)
                          else
                            return(NULL)
                        })
                      })
  }
  combs <- unlist(combs, recursive = FALSE)
  print(paste("Creating", length(combs), "formulas. Please be patient, this may take a while."))

  # create a base data.frame that has all independent variables
  # `*` in interaction terms is converted to a `.`
  reg <- data.frame(matrix(data = NA, nrow = 0, ncol = length(varNames) + 4))
  colnames(reg) <- c("aic", "rSquare", "warn", "X.Intercept.",
                     gsub("\\*", ".", varNames))

  # convert the list of independent variables into full regression formulas
  print("Creating regressions...")
  if (multi) {
    clusterExport(clust, c("combs"), envir = environment())
    forms <- pblapply(cl = clust, combs, function(x) {
      if (is.null(x))
        return(NULL)
      paste(paste(dependent, "~"),
            paste("+", x[!is.na(x)], collapse = " "),
            collapse = " ")
    })
  } else {
    forms <- pblapply(combs, function(x) {
      if (is.null(x))
        return(NULL)
      paste(paste(dependent, "~"),
            paste("+", x[!is.na(x)], collapse = " "),
            collapse = " ")
    })
  }
  forms <- unlist(forms)
  print(paste("Running", length(forms), "regressions. Please be patient, this may take a while."))

  # takes a character vector or formula and runs the requested regression
  # returns a single row data.table representing the results of that regression
  summFunc <- function(x) {
    options(warn = 0)
    summ <- summary(glm(formula = as.formula(x), data = data, family = family))
    # warn <- tryCatch({ names(last.warning) },
    #                  warning = function(e) {""},
    #                  error = function(e) {""})
    coefs <- t(summ$coefficients[,4])
    names(coefs) <- gsub(":", ".", names(coefs))
    summ <- data.frame("aic" = summ$aic,
                       "rSquare" = round(1 - (summ$deviance / summ$null.deviance), 5),
                       # "warn" = warn,
                       "warn" = NA,
                       coefs)
    rbindlist(list(reg, summ), fill = TRUE)
  }

  # runs the list of regressions and builds a data.table with the results of
  #   every regression
  print("Running regressions...")
  if(multi) {
    clusterEvalQ(clust, library(data.table))
    clusterExport(clust, c("forms", "family", "data"), envir = environment())
    regs <- rbindlist(pblapply(cl = clust, forms, summFunc), fill = TRUE)
  } else {
    regs <- rbindlist(pblapply(forms, summFunc), fill = TRUE)
  }

  # attaches the formulas to the data.table
  regs <- data.table("formula" = forms, regs)
  # renames the `X.Intercept.` column to something easier to work with
  setnames(regs, "X.Intercept.", "xIntercept")
  # ranks the results based on my arbitrary system
  regs$rowMeans <- rowMeans(regs[,!c("formula", "aic", "rSquare", "warn", "xIntercept")], na.rm = TRUE)
  regs$rank <- regs$rSquare / regs$rowMeans
  regs <- regs[order(rank, decreasing = TRUE), !c("rank", "rowMeans")]

  # stops the cluster
  if(multi) { stopCluster(clust) }

  # prints the `topN` number of regressions
  # because regressions themselves are not stored, they are run again
  if (topN > 0) {
    sapply(regs[1:topN, formula], function(x) {
      print(summary(glm(
        formula = as.formula(x),
        data = data,
        family = family
      )))
    })
  }

  # return the final data.table
  return(regs)
}
