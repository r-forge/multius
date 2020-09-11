\name{nem-terms}
\alias{nem-terms}

\title{List of terms for nem function}

\description{The list of terms (functions) that can be used in function \code{nem}. Put all the selected terms in the list object.}

\arguments{
  \item{network}{network of class \code{matrix}}
  \item{actor}{a single value indicates the unit for which the netwrok statistics are computed}
}

\section{Basic mechanisms}{
  \itemize{
    \item \code{mutuality}
    \item \code{popularity} the relative in-degree
    \item \code{diff.popularity} the squared difference in the relative in-degree between the actor and all the others
    \item \code{assortativity}

    \item \code{OTPtransitivity} (aka \emph{outgoing-two-path})
    \item \code{OSPtransitivity} (aka \emph{outgoing-shared-partner})
    \item \code{ISPtransitivity} (aka \emph{incoming-shared-partner})
    \item \code{ITPtransitivity} (aka \emph{incoming-two-path})

    \item \code{Hi} hierachical position in the network defined through domain proximity (how many units can reach the ego in any number of steps by following directed links)
    \item \code{Hi_2} hierachical position in the network defined through domain proximity (how many units can reach the ego in two (or less) steps by following directed links)
        \item \code{Hi_2} hierachical position in the network defined through domain proximity (how many units can reach the ego in three (or less) steps by following directed links)

    \item \code{diff.Hi} difference in hierarchical positions in the network defined through domain proximity (see \code{Hi})
    \item \code{diff.Hi_2} difference in hierarchical positions in the network defined through domain proximity (see \code{Hi_2})
    \item \code{diff.Hi_3} difference in hierarchical positions in the network defined through domain proximity (see \code{Hi_3})

    \item \code{Di} the distance of the path between the actor and all the others

    \item \code{asymtransitivity}: the number of common friends (two units are friends if they share reciprocated tie)
    \item \code{transitivity}: the number of common friends (two units are friends if they share a tie which can be in any direction - the network is symmetrized before the number of common firends is calculated)
  }
}

\section{Covariate-related mechanisms}{
  \itemize{
    \item \code{sq.diff (covariate)} the squared difference in the value of a covariate between the actor and all the others
    \item \code{pos.relative.value (covariate)} the normalized value of a covariate (must be positive)
  }
}
