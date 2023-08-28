\name{Impute}
\alias{Impute}
\title{Utilizes Some Function to Impute Missing Values}
\usage{
Impute(data, method = "random")
}
\description{
`method`
Can decide between "mean", "median", and "random" to impute data.
"random": Takes a random draw to impute missing values of data.
"median": Utilizes median for imputing numeric columns. For factors, the most common factor is utilized (random draw if ties).
"mean": Utilizes mean for imputing numeric columns. For factors, the most common factor is utilized (random draw if ties).

}
\examples{
Impute()
}
