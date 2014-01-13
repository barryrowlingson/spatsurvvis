\name{survmap}
\alias{survmap}
\title{Map survival data}
\usage{
  survmap(spp, ss)
}
\arguments{
  \item{spp}{a spatial points data frame}

  \item{ss}{a Surv object}
}
\value{
  a ggplot object
}
\description{
  Map survival data
}
\details{
  Produce a map of survival times showing censoring
}
\examples{
require(sp)
require(survival)
d = data.frame(
  x=runif(40)*1.5,
  y = runif(40),
  age=as.integer(20+30*runif(40)),
  sex = sample(c("M","F"),40,TRUE)
)
coordinates(d)=~x+y
d$surv = Surv(as.integer(5+20*runif(40)),runif(40)>.9)
survmap(d, d$surv)
}
\author{
  Barry S Rowlingson
}

