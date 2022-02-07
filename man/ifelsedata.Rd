\name{ifelsedata}
\alias{ifelsedata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
If-Else for Data
}
\description{
Compare dataframes at the cell level and get a dataframe where only cells that fit the test are accepted.
}
\usage{
ifelsedata(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a `data.frame` of the same dimensions as `y` }
  \item{y}{ a `data.frame` of the same dimensions as `x` }
  \item{arg}{ a logical test such as `==` or `>` in string format }
}
\details{
There is no reason for this function to exist, but I wanted to make it and so I did.
}
\value{
Returns a data.frame of the smallest size by rows and columns.

The cells returned are from `x` if the test passes and `NA` if it does not pass.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Gus Lipkin <gus.lipkin@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
This uses \code{\link[base:ifelse]{ifelse}} in the back.
}
\examples{
d1 <- data.frame(matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10))
d2 <- data.frame(matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10))

# test for equality
ifelsedata(d1, d2, "==")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
