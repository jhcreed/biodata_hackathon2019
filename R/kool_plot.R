kool_plot <- function(data_matrix, movie="koolmaps.mp4"){
  df <- reshape::melt(data_matrix)
  df$value <- as.numeric(df$value)
  
  p <- ggplot2::ggplot(df, aes(x = X1, y = X2)) + 
    ggplot2::geom_tile(aes(fill = value), color = "white") + 
    ggplot2::coord_equal() +
    ggplot2::scale_fill_viridis_c(NULL, option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "white", size = 2) + 
    ggplot2::theme(axis.title = element_blank(),
          legend.position = "bottom", 
          axis.ticks = element_blank(), 
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  if(movie=="") {
    return(p)
  } else {
    
    rayshader::plot_gg(p, width = 5, height = 5)
    
    rayshader::render_movie(movie, frames = 600) 
    
  }
  

}