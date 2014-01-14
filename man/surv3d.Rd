\name{surv3d}
\alias{surv3d}
\title{Spatial Survival Plot in 3D}
\usage{
  surv3d(spp, ss, lwd = 2, lcol = "black", lalpha = 1,
    pstyle = "point", psize = c(20, 10),
    pcol = c("red", "black"), ptext = c("X", ""),
    palpha = 1, title = "Spatial Survival",
    basegrid = FALSE, baseplane = FALSE)
}
\arguments{
  \item{spp}{A spatial points data frame}

  \item{ss}{A Surv object (with right-censoring)}

  \item{lwd}{Line width for stems}

  \item{lcol}{Line colour for stems}

  \item{lty}{Line style for stems}

  \item{pstyle}{Point style "point" or "text"}

  \item{psize}{Vector of length 2 for uncensored/censored
  points size}

  \item{pcol}{Vector of length 2 for uncensored/censored
  points colours}

  \item{ptext}{Vector of length 2 for uncensored/censored
  text characters}

  \item{title}{Main title for plot}

  \item{basegrid}{add a grid at t=0}

  \item{baseplane}{add a plane at t=0}
}
\value{
  nothing
}
\description{
  Do a 3d plot of spatial survival data
}
\details{
  Uses rgl graphics to make a spinny zoomy plot
}
\author{
  Barry S Rowlingson
}

