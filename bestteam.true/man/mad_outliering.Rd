% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mad_outliering.R
\name{mad_outliering}
\alias{mad_outliering}
\title{MAD Outliering}
\usage{
mad_outliering(dt, age_group_set_id, gbd_round_id, byvars, outlier_val)
}
\arguments{
\item{dt}{A data.table of data to be run through MAD outliering; typically a bundle or crosswalk version.}

\item{age_group_set_id}{The age_group_set_id for the get_age_metadata function, used to calculate age weights.}

\item{gbd_round_id}{The gbd_round_id for the get_age_metadata function, used to calculate age weights.}

\item{byvars}{A character string including the unique data identifier column names from dt.}

\item{outlier_val}{The value used for MAD outliering.}
}
\value{
A data.table with updated outliers, identified through MAD outliering, and noted by a value of 1 in "is_outlier" column and a comment in "note_modeler".
}
\description{
MAD Outliering
}
