# pacman::p_load(tidyverse, mosaic, magrittr, readxl, car, ggsignif, paletteer, rlang)
require(usethis)
usethis::use_package("ggsignif")
usethis::use_package("rlang")
# usethis::use_package("tidyverse")
usethis::use_package("mosaic")
usethis::use_package("magrittr")
usethis::use_package("readxl")
usethis::use_package("car")
# ctrl + opt + shift + r to generate documentation outline

#' Check group size
#'
#' In order for the other functions to be accurate,
#' the size of each group needs to be greater than 1.
#'
#' @param df Dataframe containing groups being tested
#' @param group_col Column in data containing groups
#' @param id_col Column in data containing unique IDs
#'
#' @return Cleaned dataframe with groups removed that don't meet
#' the criteria of group size > 1.
#' @export
#'
#' @examples
check_groups <- function(df, group_col, id_col) {
  # If a group only has 1 observation, it's not worth using for the
  # statistical tests in other functions
  rid <- df %>%
    group_by({{group_col}}) %>%
    dplyr::summarize(count = length(unique({{id_col}}))) %>%
    filter(count < 2) %>%
    select({{group_col}})

  # if the previous code selected any groups...
  if(nrow(rid) != 0){
    # filter for those groups
    dat <- df %>%
      filter(! {{group_col}} %in% rid[[1]])

    cat(paste(rid, "removed due to insufficient sample size\n"))
  } else{
    # otherwise, no changes to dataframe
    dat <- df
  }

  return(dat)
}

#' Between groups testing
#'
#' This function takes a dataframe and uses a
#' kruskal-wallis test to determine if groups
#' come from the same distribution or not.
#'
#' @param df Dataframe containing groups being tested
#' @param x Independent variable for kruskal-wallis test
#' @param y Dependent variable for kruskal-wallis test
#' @param filter_col Column filtered on
#' @param filter_str Filtering condition
#'
#' @return List containing kruskal-wallis results and visualization
#' @export
#'
#' @examples
group_test <- function(df, x, y, id_col,
                       # TODO: change x and y to groups and values
                       filter_col = NULL,
                       filter_str = NULL) {
  # Incoming variables are converted to names so they can interact with df
  x <- as.name(x)
  y <- as.name(y)
  id_col <- as.name(id_col)

  # if the filtering arguments are not null...
  if(!is.null(filter_col) & !is.null(filter_str)){
    filter_col <- as.name(filter_col)
    # filter!
    dat <- df %>%
      filter({{filter_col}} == {{ filter_str }} &
               !is.na({{ x }}))
    } else if(!is.null(filter_str) & is.null(filter_col)){
      stop(simpleError("Missing filter_col"))
      } else if(is.null(filter_str) & !is.null(filter_col)){
        stop(simpleError("Missing filter_str"))
        } else{
          # otherwise, dat is the same as df and filter_str is created for
          # plot purposes
          dat <- df
          filter_str <- "Plot"
          }

  # check group sizes
  dat <- check_groups(dat, {{x}}, {{id_col}})

  # check to make sure check_groups didn't remove all the data
  if(nrow(dat) == 0){
    cat("Sample sizes were not large enough to run group comparisons\n")
    } else if(length(unique(dat[[x]])) < 2){
      cat("Not enough groups to run comparison\n")
      } else{
        # H_O: All groups are from the same distribution
        # H_A: At least one group's distribution is
        # stochastically different from the others
        test <- kruskal.test(as.formula(paste(y, "~", x)), data = dat)

        # plot it
        plot <- ggplot(dat, aes(as.factor({{x}}), {{y}}, group = {{ x }})) +
          geom_boxplot() +
          # stat_summary(fun = mean, pch = 4) +
          theme_bw() +
          labs(title = paste(filter_str),
               x = str_to_title(paste({{x}})))

        return(list(test, plot))
      }
  }

#' Pairwise Comparisons
#'
#' Do all the pairwise comparisons in one swoop
#'
#' @param df Dataframe doing comparisons on
#' @param values Numeric column of df
#' @param groups Factor or character column of df
#' @param p_value Value between 0 and 1 indicating level of significance
#' @param id_col Column of unique IDs used to check group sizes
#' @param filter_str Optional string supplied if filtering within function
#' @param filter_col Optional column supplied if filtering within function.
#' Must be present if filter_str is supplied
#'
#'
#' @return Returns a matrix object
#' @export
#'
#' @examples
pair_test <- function(df, values, groups, p_value,
                      id_col, filter_str = NULL,
                      filter_col = NULL) {

  # so the column names will be treated as names in df
  groups <- as.name(groups)
  id_col <- as.name(id_col)

  # create empty list to add results to
  final <- list()

  # if the filtering arguments are present
  if(!is.null(filter_str) & !is.null(filter_col)){
    # treat argument as a name
    filter_col <- as.name(filter_col)

    # filter!
    df1 <- df %>%
      filter({{filter_col}} %in% {{filter_str}})
    } else if(!is.null(filter_str) & is.null(filter_col)){
      stop(simpleError("Missing filter_col"))
      } else if(is.null(filter_str) & !is.null(filter_col)){
        stop(simpleError("Missing filter_str"))
        } else {
          # otherwise, no change to df
          df1 <- df
          }

  # make sure groups are all large enough to test
  dat <- check_groups(df1, {{groups}}, {{id_col}})

  # check that there is still enough data to test
  if(nrow(dat) == 0){
    cat("Sample sizes were not large enough to run pairwise comparisons\n")
  } else if(length(unique(dat[[groups]])) < 2){
    cat("Not enough groups to run comparison\n")
  } else{

    # get list of groups
    group_list <- unique(dat[[groups]])

    # Perform pairwise Mann-Whitney U tests

    # create empty nxn matrix for all p-values
    results <- matrix(nrow = length(group_list), ncol = length(group_list))
    rownames(results) <- group_list
    colnames(results) <- group_list

    # create empty list for significant comparisons
    sig <- list()

    # insert results from wilcoxon test into the matrix created above
    for (i in 1:(length(group_list) - 1)) {
      for (j in (i + 1):length(group_list)) {
        # create the groups that are being compared
        group1 <- as.numeric(dat[[values]][dat[[groups]] == group_list[i]])
        group2 <- as.numeric(dat[[values]][dat[[groups]] == group_list[j]])

        # run the test
        test_result <- wilcox.test(x = group1, y = group2)

        # insert results
        results[i, j] <- test_result$p.value
        results[j, i] <- test_result$p.value


      if (!is.na(test_result$p.value)){
        # if the results are significant...
        if(test_result$p.value < p_value){

          # add to list of significant values
          sig <- sig %>% append(paste0(group_list[i], ",", group_list[j]))
          }
        } else{
        print("Comparison not valid bc of NAs")
      }
    }

    # Print the results

    }
    final <- final %>%
      append(list("p.values" = results,
                  "significant" = sig))
  }
  return(final)

}

#' Loop the whole thing
#'
#' @param df Dataframe containing biological data in long format
#' @param col Column of observations being tested
#' @param x Grouping column
#' @param y Numeric column used for kruskal-wallis and wilcoxon tests
#' @param p_value Numeric value between 0 and 1 determining significance
#' @param id_col Column with unique ids
#' @param filter_str Optional string to filter dataframe by -
#' col will be filtered by it
#' @param display_all TRUE/FALSE Default is FALSE. When FALSE, only the significant
#' results will be displayed
#'
#' @return Returns named list of results
#' @export
#'
#' @examples
analyze <- function(df, col, x, y, p_value, id_col,
                    filter_str = NULL, display_all = FALSE){
  final_results <- list()
  the_matrix <- matrix()

  # need to convert the string to a name from the dataframe
  col <- as.name(col)
  x <- as.name(x)
  y <- as.name(y)
  id_col <- as.name(id_col)

  # get the unique observations of object being tested -
  # the df will be filtered for each of these observations
  # and then tested for that unique observation
  if(!is.null(filter_str)){
    name_list <- filter_str
  } else{
    name_list <- unique(df[[col]])
  }
  # go through all the unique observations
  for (a in name_list) {
    # print(a)
    # if filtering arguments are present...
    if(!is.null(filter_str)){
      # declare filter string
      filt <- filter_str
      } else {
        # otherwise, the filtering string will be whatever
        # observation the loop is on
        filt <- a
      }

    # test if treatment groups are all the same
    te <- group_test(df = df, x = x, y = y, id_col = {{id_col}},
                       filter_str = filt, filter_col = {{col}})

    if(!is.null(te)){

        # if there's a difference...
        if (te[[1]]$p.value < .05) {
          # TODO: change .05 to variable?
          # TODO: add check to make sure te pval isn't NA
            # run pairwise tests
            l <- df %>%
              pair_test(values = y, groups = x, p_value = p_value,
                        id_col = {{id_col}}, filter_col = {{col}},
                        filter_str = filt)

            if(any(l[[1]] < p_value, na.rm = TRUE) | isTRUE(display_all)){
              # show the pairwise comparison results

              plt <- te[2][[1]]

              w <- 1
              heights <- list()
              for (i in 1:length(l$significant)){
                h <- w*max(plt$data[[y]])
                w <- w + .1
                heights <- append(heights, h)
              }

              plt_fin <- plt +
                ggsignif::geom_signif(mapping = aes(group = as.factor({{x}})),
                            comparisons = lapply(l[2][[1]],
                                                 function(st) unlist(str_split(st, ","))),
                            map_signif_level = c("*" = .05),
                            y_position = unlist(heights))

              final_results <- final_results %>%
                append(list("Name" = a,
                          "Kruskall.Wallis" = te[1],
                          "Plot" = plt_fin,
                          "Pairwise" = l))
              } else{
                print(paste("No significant pairwise comparisons for ", a))
              }
          } else {  # if no difference between treatment groups...
            print(a)
            print("No significant difference")
          }
        } else {
          print("No comparisons run")
        }
      }
  return(final_results)
  }

#' Plot barcharts for overall comparison
#'
#' @param df Dataframe
#' @param x It should be a column of categorical data
#' @param y Column for the y axis - numeric
#' @param group Group for color - categorical
#'
#' @return list of plots
#' @export
#'
#' @examples
plot_overall <- function(df, x, y, group){
  x <- as.name(x)
  y <- as.name(y)
  group <- as.name(group)

  a <- df %>%
    group_by({{x}}, {{group}}) %>%
    summarize(avg = mean({{y}}, na.rm = TRUE)) %>%
    ggplot(aes(x = {{x}}, y = avg,
               fill = as.factor({{group}}),
               group = as.factor({{group}}))) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = paletteer_d("ggsci::category20_d3")) +
    labs(title = str_to_title(paste({{x}}, {{y}}, "Across Groups")),
         x = str_to_title(paste({{x}})),
         y = str_to_title(paste("Average", {{y}}, "(%)")),
         fill = str_to_title(paste({{group}}))) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5),
          axis.text.x = element_text(angle = 45,
                                     vjust = .35))

  b <- df %>%
    group_by({{x}}, {{group}}) %>%
    summarize(avg = mean({{y}}, na.rm = TRUE)) %>%
    ggplot(aes(x = as.factor({{group}}), y = avg, fill = {{x}}, group = {{x}})) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = paletteer_d("ggsci::category20_d3")) +
    labs(title = str_to_title(paste0(x, " ", y, " Across ", group, "s")),
         x = str_to_title(paste({{group}})),
         y = str_to_title(paste("Average", {{y}}, "(%)")),
         fill = str_to_title(paste({{x}}))) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5),
          panel.grid.minor = element_blank())
          # axis.text.x = element_text(angle = 45, vjust = .25))

  print(a)
  print(b)

  # TODO: Need to return the plots in a way that they can be tweaked with additional
  # aruments. Right now it's not going to happen though.

}

