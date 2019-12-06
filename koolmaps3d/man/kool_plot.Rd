\name{kool_plot}
\alias{kool_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a KOOL MAP! (Creates 3D heat map and movie)
}
\description{
Function that takes output from dual_matrix function and returns a static 3D plot or movie
}
\usage{
kool_plot(data_matrix, movie)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_matrix}{
 Matrix from dual_matrix function.
}
  \item{movie}{ Location to save movie leave blank for static image = ""
}
}
\details{
Function
}
\value{
A KOOL MAP movie or image
}
\references{
None
}
\author{
The HELL BOY GEORGE MICHAEL JORDAN CREEDS
}



\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data_matrix, movie)
{
    df <- reshape::melt(data_matrix)
    df$value <- as.numeric(df$value)
    p <- ggplot2::ggplot(df, aes(x = X1, y = X2)) + ggplot2::geom_tile(aes(fill = value),
        color = "white") + ggplot2::coord_equal() + ggplot2::scale_fill_viridis_c(NULL,
        option = "plasma") + ggplot2::theme_minimal() + ggplot2::geom_abline(intercept = 0,
        slope = 1, color = "white", size = 2) + ggplot2::theme(axis.title = element_blank(),
        legend.position = "bottom", axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))
    if (movie == "") {
        return(p)
    }
    else {
        rayshader::plot_gg(p, width = 5, height = 5)
        rayshader::render_movie(movie, frames = 600)
    }
  }
}

