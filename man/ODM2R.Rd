\name{ODM2R}
\alias{ODM2R}
\title{ ODM2R }
\description{
ODM2R transforms ODM file into source code for R data frame "odmdata".\cr
Clinical data (if available) is converted into source code for R data frame "clindata"\cr
Input: File in CDISC ODM format (Version 1.3; see example)\cr
Output: R source code for data frame "odmdata" (optional: data frame "clindata") with ODM attributes (see example).
}
\usage{ ODM2R(ODMfile="", Form_OID="", IG_OID="") }
\arguments{
  \item{ODMfile}{
   Name of ODM file for conversion.\cr
   File format: ODM version 1.3, see example. If omitted, file.choose()-dialog is started
  }
  \item{Form_OID}{
   optional argument: OID of form for conversion.\cr
  }
  \item{IG_OID}{
   optional argument: OID of item group for conversion.\cr
  }
}

\references{
   Dugas M, Dugas-Breit S. Integrated data management for clinical studies: automatic transformation of data models with semantic annotations for principal investigators, data managers and statisticians.\cr
   PLoS One. 2014 Feb 28;9(2):e90492
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
ODM2R(ODMfile=file.path(path.package("ODMconverter"),"extdata","ODM-Test_S.0000.xml"))
}
\keyword{ ODM }
\keyword{ UMLS }
\keyword{ xlsx }
