\name{regsearch}
\alias{regsearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regression Search
}
\description{
This is an implementation of a global search regression. It leverages the `apply` family of functions for faster processing and uses `glm` for running the regression.
}
\usage{
regsearch(fDT,
         dependent,
         independent,
         minvar = 1,
         maxvar,
         family,
         interactions = FALSE,
         multi = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fDT}{
A `data.frame` that contains a dependent variable and the independent variables
}
  \item{dependent}{
The dependent variable for the regression.
}
  \item{independent}{
A vector of independent variables to be used. These must match the column names from `fDT`.
}
  \item{minvar}{
(Optional) The minimum number of independent variables to be used in the regression. Defaults to 1.
}
  \item{maxvar}{
The maximum number of independent variables to be used in the regression. Must be equal to or less than the number of independent variables.

If interaction terms are used, they count as one independent variable.
}
  \item{family}{
The type of regression. Passed to `glm`. See \code{\link[stats:glm]{glm}} for more information.
}
  \item{interactions}{
A boolean indicating whether or not interaction terms should be used. Defaults to `FALSE`.
}
  \item{multi}{
A boolean indicating whether or not multithreading should be used. Defaults to `FALSE`.
It is highly recommended to use multithreading.
}
}
\details{

}
\value{
Returns a `data.table` of information on the regressions run.
The resulting data.table is sorted in descending order by the rSquare divided by the mean p-value. This is generally reliable in pushing quality regressions to the top of the list.
\item{`formula` }{The regression formula used}
\item{`aic` }{The aic for the regression}
\item{`rSquare` }{The calculated r-square for the regression}
\item{`warn` }{Currently unused}
\item{variables }{Each variable column contains the p-values for that variable or interaction term in a given regression}
%% ...
}
\references{
\url{http://fmwww.bc.edu/RePEc/bocode/g/GlobalSearchRegression.pdf}
}
\author{
Gus Lipkin <gus.lipkin@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  This is a wrapper for \code{\link[stats:glm]{glm}}.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fDT, dependent, independent, minvar, maxvar, family,
    interactions = FALSE, multi = FALSE)
{
    if (multi) {
        clust <- makeCluster(detectCores())
    }
    print("Gathering variables...")
    if (interactions) {
        independent <- sort(independent)
        independent <- c(pbapply(cl = clust, do.call(rbind, combn(independent,
            2, simplify = FALSE)), 1, function(x) {
            paste(x, collapse = "*")
        }), independent)
        combs <- rbindlist(pbsapply(cl = clust, minvar:maxvar,
            function(x) {
                data.table(do.call(rbind, combn(independent,
                  x, simplify = FALSE)))
            }), fill = TRUE)
    }
    else {
        independent <- sort(independent)
        combs <- rbindlist(pbsapply(minvar:maxvar, function(x) {
            data.table(do.call(rbind, combn(independent, x, simplify = FALSE)))
        }), fill = TRUE)
    }
    reg <- data.table(matrix(data = "", nrow = 1, ncol = length(independent) +
        4))
    colnames(reg) <- c("aic", "rSquare", "warn", "X.Intercept.",
        gsub("\\*", ".", independent))
    print("Creating regressions...")
    if (multi) {
        clusterExport(clust, c("combs"), envir = environment())
        forms <- pbapply(cl = clust, combs, 1, function(x) {
            comb <- as.character(x)
            as.formula(gsub(" \\+$", "", paste(paste(dependent,
                "~"), paste(comb[!is.na(comb)], "+", collapse = " "),
                collapse = " ")))
        })
    }
    else {
        forms <- pbapply(combs, 1, function(x) {
            comb <- as.character(x)
            as.formula(gsub(" \\+$", "", paste(paste(dependent,
                "~"), paste(comb[!is.na(comb)], "+", collapse = " "),
                collapse = " ")))
        })
    }
    forms <- as.character(unlist(forms, recursive = TRUE))
    summFunc <- function(x) {
        summ <- summary(glm(formula = as.formula(x), data = fDT,
            family = family))
        coefs <- t(summ$coefficients[, 4])
        names(coefs) <- gsub(":", ".", names(coefs))
        summ <- data.frame(aic = summ$aic, rSquare = round(1 -
            (summ$deviance/summ$null.deviance), 5), warn = FALSE,
            coefs)
        data.frame(rbindlist(list(reg, summ), fill = TRUE))
    }
    print("Running regressions...")
    if (multi) {
        clusterEvalQ(clust, library(data.table))
        clusterExport(clust, c("forms", "family", "fDT"), envir = environment())
        regs <- pbsapply(cl = clust, forms, summFunc)
    }
    else {
        regs <- pbsapply(forms, summFunc)
    }
    print("Cleaning the output...")
    regs <- data.table(t(regs))
    if (multi) {
        regs <- data.frame(forms, pblapply(cl = clust, regs,
            function(x) {
                as.numeric(gsub("[c\\(\", \\)]", "", x))
            }))
    }
    else {
        regs <- data.frame(forms, pblapply(regs, function(x) {
            as.numeric(gsub("[c\\(\", \\)]", "", x))
        }))
    }
    regs <- data.table(regs)
    regs$rank <- regs$rSquare/rowMeans(regs[, !c("forms", "aic",
        "rSquare", "warn", "X.Intercept.")], na.rm = TRUE)
    if (multi) {
        stopCluster(clust)
    }
    return(regs[order(desc(rank))])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ iteration }
\keyword{ regression }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
