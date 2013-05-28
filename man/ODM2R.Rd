\name{ODM2R}
\alias{ODM2R}
\title{ ODM2R }
\description{
ODM2R transforms ODM file into source code for R data frame "odmdata".\cr
Input: File in CDISC ODM format (Version 1.3; see example)\cr
Output: R source code for data frame "odmdata" with ODM attributes (see example).
}
\usage{ ODM2R(ODMfile="") }
\arguments{
  \item{ODMfile}{
   Name of ODM file for conversion.\cr
   File format: ODM version 1.3, see example. If omitted, file.choose()-dialog is started
  }
}

\references{
   Automatic conversion of ODM format with semantic annotation (manuscript in preparation)
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
ODM2R(ODMfile=file.path(path.package("ODMconverter"),"extdata","ODM-Test_S.0000.xml"))
}
\keyword{ ODM }
\keyword{ UMLS }
\keyword{ xlsx }
