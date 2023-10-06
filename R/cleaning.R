pacman::p_load(rlang)

#' Calcuate glycan component percentages
#'
#' @param df The dataframe
#' @param glycan The specific binary glycan component column
#' @param id_col The column that holds the unique ids of samples
#'
#' The dataframe fed into this function must include the columns `Area`,
#' `adj_area`, `total_area`, `adj_tot_area`.
#'
#' @return A dataframe with a new column for the abundance of glycan component
#' @export
#'
#' @examples

get_percs <- function(data, glycan, id_col){
  # this function calculates the relative
  # abundance of each glycan in the sample
  glycan <- as.name(glycan)
  cl_name <- paste0(rlang::get_expr(glycan), "_perc")
  id_col <- as.name(id_col)

  # adjust for any species with double glycans by using doubled
  # areas. The peaks are a 1 to 1 ratio, so we double to account
  # for both components. Most commonly, this is S or G (they might
  # be the only cases)
  if(str_detect(paste(glycan), "2")){
    a = as.name("adj_area")
    b = as.name("adj_tot_area")
  } else {
    a = as.name("Area")
    b = as.name("total_area")
  }


  df <- data %>%
    # group by sample and glycan so the area is calculated for both
    # the allele without the glycan and with
    group_by({{id_col}}, {{glycan}}) %>%
    # if the glycan is present, add the area of those alleles, then divide by
    # total area to get percentage.
    mutate(!!cl_name := if_else({{glycan}} != 0,
                                round((sum({{a}})/{{b}})*100, 2),
                                # if the glycan is NOT present, take the
                                # percentage calulated and subtract from 100
                                100 - round((sum({{a}})/{{b}})*100, 2)
    )
    )

}

#' Calculate total relative abundance for glycan component
#'
#' @param df Dataframe including columns `adj_area` and `adj_tot_area`
#' @param glycan Glycan component that total abundance is being calculated
#' @param unique_id Column containing unique id for each sample
#' @param species Column containing the different glycan species
#'
#' @return The dataframe used in the function with an additional column with
#' the total abundance of glycan components that have both 1 or 2 component
#' options.
#' @export
#'
#' @examples

get_tots <- function(df, glycan, unique_id, species){
  # calculate totals for those
  # with 1 and 2 component options
  # species is the species column

  glycan <- as.name(glycan)
  unique_id <- as.name(unique_id)
  cl <- paste0(rlang::get_expr(glycan), "_perc")
  cl_name <- paste0("tot_", glycan)
  species <- as.name(species)


  dat <- df %>%
    group_by({{unique_id}}) %>%
    # if the glycan species contains the glycan, add
    # the adjusted areas(because if we're using this
    # function, it should be because there's G and G2)
    # and then divide by the adjusted total area to
    # get the relative abundance according to adjusted
    # totals
    mutate("tot_{{glycan}}" := sum(adj_area[
      str_detect({{species}}, paste(glycan)) &
        !str_detect({{species}}, "0")])/adj_tot_area*100)

  return(dat)
}
