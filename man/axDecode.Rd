% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axDecode.R
\name{axDecode}
\alias{axDecode}
\title{Decrypt the code generated for the subject hour confirmations}
\usage{
axDecode(code, key)
}
\arguments{
\item{code}{Code on the subject hour confirmations generated by the
\link{genSubjHconf} function. The code must be entered \strong{without}
line breaks.}

\item{key}{Private key object which has to be used to decypt the code.}
}
\value{
Returns a decrypted string.
}
\description{
The function decrypts the code generated by the \link{genSubjHconf} function.
It can be used to verify the code on the subject hour confirmations against
manipulation.
}
\examples{
\dontrun{
# Decode the code printed on the pdf (code must be in one line!)
load("/my/working/directory/key_323032312d30312d31352031363a34303a3232.rda")
# or load(paste0("/my/working/directory/",whichKey("2021−01−15 16:40:22")))
axDecode("oHtDit5N34y/s+uhOviVKWq/7QGRyQ/M4dP+txAnxpWRcoS...",key)
}

}
\author{
Axel Zinkernagel \email{zinkernagel@uni-landau.de}
}
