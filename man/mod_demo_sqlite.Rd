% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_demo_sqlite.R
\name{mod_demo_sqlite}
\alias{mod_demo_sqlite}
\alias{demo_sqlite_setup_ui}
\alias{demo_sqlite_setup_server}
\title{Demo SQLite Database Module}
\usage{
demo_sqlite_setup_ui(id)

demo_sqlite_setup_server(id)
}
\arguments{
\item{id}{The module namespace}
}
\value{
\emph{demo_sqlite_setup_ui}:
\item{tagList}{The Demo SQLite Setup UI}
\emph{demo_sqlite_setup_server}:
\item{reactiveValues}{
\itemize{
\item{moduleName}: A string, containing the module moniker.
\item{moduleType}: A string, with the module type (what does it do?)
\item{setup_ui}: The module setup ui function
\item{is_connected}: A string, with module connection status. Valid statuses are
'yes' or 'no'.
\item{db_con}: A \link[DBI:dbConnect]{DBI::dbConnect} object, containing the user configured
connection information.
}}
}
\description{
This module will create an in memory SQLite database with demo data from
the CMS 2008-2010 Data Entrepreneursâ€™ Synthetic Public Use File (DE-SynPUF)
from OHDSI. It will allow you to preview the functionality of ReviewR
if you do not have access to a database of patient information.

This module consists of the following components:
\subsection{Module UI function}{

\itemize{
\item{\code{demo_sqlite_setup_ui}}: A uiOutput that allows users to connect to
the demonstration database.
}
}

\subsection{Module Server function}{

\itemize{
\item{\code{demo_sqlite_setup_server}}: The logic that creates the demonstration
SQLite database and returns a \code{\link[DBI:dbConnect]{DBI::dbConnect()}} object used to connect
to the demo database.
}
}
}
\keyword{internal}
