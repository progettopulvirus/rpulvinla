% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creaCalendario.R
\name{creaCalendario}
\alias{creaCalendario}
\title{Tibble con colonne anno mese giorno}
\usage{
creaCalendario(annoI, annoF)
}
\arguments{
\item{annoI}{anno di inizio del calendario}

\item{annoF}{anno di fine del calendario}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
Crea un tibble con le colonne yy (anno), mm (mese), dd (giorno) con yy che va dal primo gennaio di annoI al 31 dicembre di annoF
}
\examples{
creaCalendario(1961,1990)

}
