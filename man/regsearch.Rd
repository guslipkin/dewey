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
regsearch(data,
         dependent,
         independent,
         minvar = 1,
         maxvar,
         family,
         topN = 0,
         interactions = FALSE,
         multi = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A `data.frame` that contains a dependent variable and the independent variables
}
  \item{dependent}{
The dependent variable for the regression.
}
  \item{independent}{
A vector of independent variables to be used. These must match the column names from `data`.

These can also include interaction terms made from column names from `data`. This allows for specific interaction terms to be used, rather than every possible interaction as is done with `interactions = TRUE`.
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
  \item{topN}{
(Optional) The number of top results to be printed upon run completion. Defaults to 0.
}
  \item{interactions}{
(Optional) A boolean indicating whether or not interaction terms should be used. Defaults to `FALSE`.
}
  \item{multi}{
(Optional) A boolean indicating whether or not multithreading should be used. Defaults to `FALSE`.
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
## The function is currently defined as
function (fDT, dependent, independent, minvar, maxvar, family, topN = 0,
    interactions = FALSE, multi = FALSE)
# Creating dummy data
dt <- data.frame("dependent" = sample(c(0, 1), 100, replace = TRUE),
                 "ind_1" = runif(100, 0, 1),
                 "ind_2" = runif(100, 0, 1),
                 "ind_3" = runif(100, 0, 1),
                 "ind_4" = runif(100, 0, 1))

# Without interaction terms and multithreading
## Two top results
regsearch(dt, dependent, c("ind_1", "ind_2", "ind_3", "ind_4"),
          1, 4, "binomial", 2)
## No top results
regsearch(dt, dependent, c("ind_1", "ind_2", "ind_3", "ind_4"),
          1, 4, "binomial", FALSE, FALSE)

# With interaction terms and multithreading
regsearch(dt, dependent, c("ind_1", "ind_2", "ind_3", "ind_4"),
          1, 4, "binomial", TRUE, TRUE)
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
