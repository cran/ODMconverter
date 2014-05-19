\name{office2ODM}
\alias{office2ODM}
\title{ office2ODM }
\description{
office2ODM transforms an office file into ODM format. Supported formats are csv-files, txt-files (tab-separated) and xlsx-files (Microsoft Excel).\cr
Input: Office file with study header, itemgroups, items and codelists, including semantic annotation (see example).
Output: CDISC ODM format (Version 1.3; see example)\cr
}
\usage{ office2ODM( officefile="" ) }
\arguments{
  \item{officefile}{
   Name of office file for conversion.\cr
   File format: csv, txt (tab-separated) or xlsx (Microsoft excel), see example. If omitted, file.choose()-dialog is started
  }
}
\references{
   Dugas M, Dugas-Breit S. Integrated data management for clinical studies: automatic transformation of data models with semantic annotations for principal investigators, data managers and statisticians.\cr
   PLoS One. 2014 Feb 28;9(2):e90492
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
office2ODM(officefile=file.path(path.package("ODMconverter"),"extdata","ODM-Test.xlsx"))
}
\keyword{ ODM }
\keyword{ UMLS }
\keyword{ xlsx }
