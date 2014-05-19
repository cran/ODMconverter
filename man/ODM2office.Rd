\name{ODM2office}
\alias{ODM2office}
\title{ ODM2office }
\description{
ODM2office transforms an ODM file into office format. Supported formats are csv-files, txt-files (tab-separated) and xlsx-files (Microsoft Excel).\cr
Input: CDISC ODM format (Version 1.3; see examples)\cr
Output: Office file with study header, itemgroups, items and codelists, including semantic annotation (see example).
}
\usage{ ODM2office( ODMfile="", filetype="" ) }
\arguments{
  \item{ODMfile}{
   Name of ODM file for conversion.\cr
   File format: ODM version 1.3, see example. If omitted, file.choose()-dialog is started
  }
  \item{filetype}{
   Office file format for output.\cr
   Valid formats: csv, txt and xlsx.\cr
  }
}
\references{
   Dugas M, Dugas-Breit S. Integrated data management for clinical studies: automatic transformation of data models with semantic annotations for principal investigators, data managers and statisticians.\cr
   PLoS One. 2014 Feb 28;9(2):e90492
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
ODM2office(ODMfile=file.path(path.package("ODMconverter"),"extdata","ODM-Test_S.0000.xml"), 
filetype="xlsx")
}
\keyword{ ODM }
\keyword{ UMLS }
\keyword{ xlsx }
