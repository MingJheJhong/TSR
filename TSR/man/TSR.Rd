\name{TenStepsReturn}
\alias{TenStepsReturn}
\title{TenStepsReturn}
\usage{
TenStepsReturn(size, row, col, test = 1000, discount = 0.9)
}
\arguments{
\item{size}{Size of the environment, it is a square matrix with random number\{-1, 0, 1\}.}

\item{row}{Row of the start point.}

\item{col}{Column of the start point.}

\item{test}{The counts of testing.}

\item{discount}{The discount rate for rewards every step.}
}
\description{
To find the maximum of ten-step return after testing at a square matrix with random number\{-1, 0, 1\} where 0, 1 are rewards and the point -1 can not pass through.
}
\value{
N_left:The numbers of times moving to the left at that point. \n
N_up:The numbers of times moving up at that point. \n
N_right:The numbers of times moving to the right at that point. \n
N_down:The numbers of times moving downward at that point. \n
Environment:The environment with reward 0 and 1, and the point -1 can not passthrough. \n
Path:The passed points. \n
Return:The maximum of ten-step return after testing.
}
\examples{
example = TenStepsReturn(5, 3, 2)
# The environment is a 5*5 square matrix The start point is 3rd row and 2nd
# column.  Testing 1000 times.  Discount = 0.9.
example  # Result
}
