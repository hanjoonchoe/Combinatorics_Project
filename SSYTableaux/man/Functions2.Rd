\name{Bijection from DeConcini to King tableaux}
\alias{Functions2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
D2K_Bijection(D_tab,Range)
D2K_Gen(D_tab,Range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D2K_Bijection}{
  Bijection from DeConcini tableau to King tableau
}
  \item{D2K_Gen}{
  Bijection DeConcini tableaux to King tableaux
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

1. Shape <- c(3,2,1)
2. Range <- 3 (i.e. {-3,-2,-1,1,2,3})
3. D_tab <- DeConcini_Generator(Shape,Range)
4. D2K_tab <- D2K_Bijection(D_tab[[1]],Range) or D2K_Gen(D_tab,Range)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
