% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{uv_process_catconf}
\alias{uv_process_catconf}
\alias{uv_process}
\alias{uv_process.numeric}
\alias{uv_process.matrix}
\alias{uv_response}
\title{category confusion process generator}
\usage{
uv_process_catconf(
  Part = 1,
  Stim = 1,
  stim_humLik = 0.5,
  cond_prtime = 1000,
  part_info_uptake = 1,
  part_reluctance = 5,
  part_shock = 1,
  part_pace = 25,
  part_cert_face = 2,
  full_table = T,
  resp_only = F
)

uv_process(...)

\method{uv_process}{numeric}(par, ...)

\method{uv_process}{matrix}(mat, ...)

uv_response(mat, ...)
}
\arguments{
\item{Part}{participant}

\item{Stim}{stimulus}

\item{stim_humLik}{human likeness}

\item{cond_prtime}{presentation time}

\item{part_info_uptake}{info uptake}

\item{part_reluctance}{reluctance}

\item{part_shock}{shock}

\item{part_pace}{pace}

\item{part_cert_face}{certainty}

\item{full_table}{full table}

\item{resp_only}{only response}
}
\value{
process table
}
\description{
category confusion process generator
}
