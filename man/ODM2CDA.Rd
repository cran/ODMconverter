\name{ODM2CDA}
\alias{ODM2CDA}
\title{ ODM2CDA }
\description{
ODM2CDA transforms an ODM file into CDA format (clinical document architecture). \cr
Input: CDISC ODM format (Version 1.3; see examples)\cr
Output: CDA file (see example).
}
\usage{ ODM2CDA( ODMfile="", withUMLS=T ) }
\arguments{
  \item{ODMfile}{
   Name of ODM file for conversion.\cr
   File format: ODM version 1.3, see example. If omitted, file.choose()-dialog is started
  }
  \item{withUMLS}{
   Flag indicating whether ODM file contains UMLS codes.\cr
  }
}
\references{
   ODM2CDA and CDA2ODM: An approach to convert documentation forms between EDC and EHR systems (manuscript in preparation)
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
ODM2CDA(ODMfile=file.path(path.package("ODMconverter"),"extdata","ODM-Test_S.0000.xml"))
}
\keyword{ ODM }
\keyword{ CDA }
