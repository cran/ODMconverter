\name{CDA2ODM}
\alias{CDA2ODM}
\title{ CDA2ODM }
\description{
CDA2ODM transforms a CDA (clinical document architecture) file into ODM format.\cr
Input: CDA file with CDA header and CDA body (see example from www.elga.gv.at).\cr
Output: CDISC ODM format (Version 1.3; see example)\cr
}
\usage{ CDA2ODM( CDAfile="" ) }
\arguments{
  \item{CDAfile}{
   Name of CDA file for conversion.\cr
   If omitted, file.choose()-dialog is started.
  }
}
\references{
   ODM2CDA and CDA2ODM: An approach to convert documentation forms between EDC and EHR systems (manuscript in preparation)
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
CDA2ODM(CDAfile=file.path(path.package("ODMconverter"),"extdata","CDA_letter_basic.xml"))
}
\keyword{ ODM }
