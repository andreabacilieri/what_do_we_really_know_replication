#' Sets the figure dimensions to avoid scaling in LaTeX

#' @param width A float or string. A float specifying the document width in points or a string of predefined document type ('thesis', 'beamer' or 'pnas')
#' @param fraction A float in (0, 1] specifying the fraction of the width that the figure should occupy
#' 
#' @return fig_size A list with two elements specifying the dimensions of the figure in inches, (width, height)


set_size <- function(width, fraction = 1){

  if (width == 'thesis'){
    width_pt <- 426.79135}
  else if(width == 'beamer'){
      width_pt <- 307.28987
      }
  else if(width == 'pnas'){
      width_pt <- 246.09686
      }
  else{
      width_pt <- width
      }
    
    # width of figure (in pts)
    fig_width_pt <- width_pt * fraction
    # convert from pt to inches
    inches_per_pt <- 1 / 72.27
    
    # golden ratio to set aesthetic figure height
    golden_ratio <- (5^.5 - 1) / 2
    
    # figure width in inches
    fig_width_in <- fig_width_pt * inches_per_pt
    # figure height in inches
    fig_height_in <- fig_width_in * golden_ratio
    
    fig_size <- list(fig_width_in, fig_height_in)
    
    return(fig_size)
}