#' Create one dataframe with all timepoints
#'
#' @param ... A list of files to read in. These should already be clean and all files need to include the same columns in the same order
#' It's important as well that the files are inserted in order that they were created, i.e. the earliest timepoint should be first, second
#' timepoint should be second, etc.
#'
#' @return A single dataframe that lists all samples for each timepoint.
#' @export
#'
#' @examples
create_time_data <- function(...){
  # must have same columns in files
  complete <- data.frame()
  x <- list(...)
  for(i in 1:length(x)){
    # read in the file
    cur_tp <- read_xlsx(x[[i]])
    # add column indicating which period the data is from
    cur_tp["period"] <- i
    # add current read-in data to the complete dataframe
    complete <- complete %>%
      rbind(cur_tp)
  }
  return(complete)
}


#' Visualize trends over time
#'
#' @param df A dataframe with multiple timepoints
#' @param x The column that will provide the x axis with data. In the case
#' of this function, it should be some measure of time.
#' @param y Column that will provide the y axis with data.
#' @param id_col Column that indicates the grouping factor for connecting
#' points
#' @param color_col Optional column to determine how the plot should be colored
#' @param filter_str Optional argument that allows the dataframe to be filtered
#' within the function - provide the desired filtering string here
#' @param filter_col Column being filtered within the dataframe. If filter_str
#' is provided, this also must be present
#'
#' @return A single dataframe that lists all samples for each timepoint.
#' @export
#'
#' @examples
plot_time_data <- function(df, x, y, id_col, color_col = NULL,
                           filter_str = NULL, filter_col = NULL){
  # treat incoming variables as column names
  x <- as.name(x)
  y <- as.name(y)
  id_col <- as.name(id_col)

  # if the filtering arugments are supplied...
  if(!is.null(filter_col) & !is.null(filter_str)){
    filter_col <- as.name(filter_col)
    # filter!
    df <- df %>%
      filter({{filter_col}} == {{filter_str}})
  }

  # if the color column is not supplied...
  if(is.null(color_col)){
    # id column will be used for color
    color_col <- id_col
  } else{
    # otherwise, color aes will be supplied by user
    color_col <- as.name(color_col)
  }

  # plot it!
  ggplot(df, aes(x = {{x}}, y = {{y}},
                 group = as.factor({{id_col}}),
                 color = as.factor({{color_col}}))) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x = paste(x),
         y = paste(y),
         color = paste(color_col))
}
