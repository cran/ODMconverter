\name{R2ODM}
\alias{R2ODM}
\title{ R2ODM }
\description{
R2ODM transforms R data frame "odmdata" into ODM file.\cr
Input: R data frame with ODM attributes (see example).\cr
Output: File in CDISC ODM format (Version 1.3; see example)\cr
}

\usage{ R2ODM(odmdata) }
\arguments{
  \item{odmdata}{
   R data frame for conversion (see example).\cr
  }
}
\references{
   Automatic conversion of ODM format with semantic annotation (manuscript in preparation)
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
   \dontrun{ source(file.path(path.package("ODMconverter"),"extdata","ODM-Test_S.0000.xml.R")) 
   R2ODM(odmdata) }
}
\keyword{ ODM }
\keyword{ UMLS }
\keyword{ xlsx }
