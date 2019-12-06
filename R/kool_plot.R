kool_plot <- function(data_matrix, movie="koolmaps.mp4", pop1, pop2, title){
  df <- reshape::melt(data_matrix)
  df$value <- as.numeric(df$value)
  
  p <- ggplot2::ggplot(df, aes(x = as.character(X1), y = as.character(X2))) + 
    ggplot2::geom_tile(aes(fill = value), color = "gray70", size = 0.1) + 
    ggplot2::coord_equal(clip = "off") +
    # scale_y_discrete("Japanese", position = "right") +
    # scale_x_discrete("CEU", position = "top") +
    ggplot2::labs(x = pop1, y = pop2, title = title) +
    ggplot2::scale_fill_viridis_c(NULL, option = "C") +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "white", size = 1) + 
    ggplot2::theme(legend.position = "bottom", 
          plot.title = element_text(size = 19),
          axis.title = element_text(size = 17),
          axis.ticks = element_blank(), 
          axis.text = element_blank())
  
  if(movie=="") {
    return(p)
  } else {
    
    rayshader::plot_gg(p, width = 5, height = 5)
    
    rayshader::render_movie(movie, frames = 600) 
    
  }
  

}