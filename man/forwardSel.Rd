% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featuresel.R
\name{forwardSel}
\alias{forwardSel}
\title{forwardSel}
\usage{
forwardSel(
  X,
  Y,
  algo = "rf",
  nmax = 5,
  nmax2 = nmax,
  cv = 1,
  classi = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{X:}{input dataset}

\item{Y:}{output dataset}

\item{algo:}{see the options of \link{pred}. If more than one algorithm is mentioned, a blocking selection is carried out}

\item{nmax:}{number of top returned features}

\item{nmax2:}{number of forward selection steps}

\item{classi:}{if TRUE, classification problem else regression}

\item{back:}{if TRUE, backward reordering based on linear regression}

\item{cv:}{number of cross-validation folds (if \code{cv=1} no cross-validation)}

\item{verbose:}{if TRUE it prints out the selected variables and associated accuracy (MSE if regression and Misclassification error if classification)}
}
\value{
Indices of \code{nmax} top ranked features
}
\description{
Wrapper feature selection based on forward selection and a generic predictor
}
\details{
forwardSel

Wrapper feature selection based on forward selection and a generic predictor
}
\examples{
## regression example
N<-100
n<-5
neff<-3
R<-regrDataset(N,n,neff,0.1,seed=0)
X<-R$X
Y<-R$Y
real.features<-R$feat
ranked.features<-forwardSel(X,Y,nmax=3)

## classification example
N<-100
n<-5
neff<-3
R<-regrDataset(N,n,neff,0.1,seed=1)
X<-R$X
Y<-factor(R$Y>mean(R$Y))
## it creates a binary class output
real.features<-R$feat
ranked.features<-forwardSel(X,Y,nmax=3,classi=TRUE,cv=3)


## classification example with blocking
N<-100
n<-5
neff<-3
R<-regrDataset(N,n,neff,0.1,seed=1)
X<-R$X
Y<-factor(R$Y>mean(R$Y))
## it creates a binary class output
real.features<-R$feat
ranked.features<-forwardSel(algo=c("rf","lda"), X,Y,nmax=3,classi=TRUE,cv=3)
}
\references{
Handbook \emph{Statistical foundations of machine learning} available in \url{https://tinyurl.com/sfmlh}. For
blocking see \emph{Bontempi G. A blocking strategy to improve gene selection for classification of gene expression data. IEEE/ACM Trans Comput Biol Bioinform. 2007 Apr-Jun;4(2):293-300.}
}
\author{
Gianluca Bontempi  \email{Gianluca.Bontempi@ulb.be}
}
