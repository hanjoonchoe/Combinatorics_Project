\name{King_Tableaux and DeConcini_Tableaux}
\alias{Functions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{

Generating King or DeConcini Tableaux
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
K_tab <- King_Generator(Shape,Range)
D_tab <- DeConcini_Generator(Shape,Range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Shape}{

  Determine the shape of a tableau

}
  \item{Range}{

  Determine a range of a set filling the tableau

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

1. Shape <- c(3,2,2)
2. Range <- 3 (i.e. {-3,-2,-1,1,2,3})
3. K_tab <- King_Generator(Shape,Range) or DeConcini_Generator(Shape,Range)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

