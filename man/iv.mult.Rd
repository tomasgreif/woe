\name{iv.mult}
\alias{iv.mult}
\title{Calculate Information Value for defined columns in data frame}
\usage{
  iv.mult(df, y, summary = F, vars = NULL)
}
\arguments{
  \item{df}{data frame with at least two columns}

  \item{y}{column (integer) with binary outcome}

  \item{summary}{Only total information value for variable
  is returned when summary is TRUE. Output is sorted by
  information value, starting with highest value.}

  \item{vars}{List of variables. If not specified, all
  character variables will be used}
}
\description{
  Calculates information value for defined columns in given
  data frame. Columns can have numeric or character type
  (including factors).
}
\details{
  Information Value (IV) is concept used in risk management
  to assess predictive power of variable. IV is defined as:
  WoE (Weight of Evidence) is defined as:
}
\examples{
iv.mult(german_data,"gbbin")
iv.mult(german_data,"gbbin",T)
iv.mult(german_data,"gbbin",T,c("ca_status","housing","job","mob")) # str(german_data)
iv.mult(german_data,"gbbin",vars=c("ca_status","housing","job","mob","gb"))
iv.mult(german_data,vars=c('ca_status','mob','credit_history','purpose',
                           'credit_amount','savings','present_employment_since',
                           'status_sex','installment_rate_income','other_debtors',
                           'present_residence_since','property','age','other_installment',
                           'housing','existing_credits','job','liable_maintenance_people',
                           'telephone','foreign_worker'),
                           "gbbin", summary=T)
}

