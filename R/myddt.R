#' @title my ddt
#' @description subsets ddt by species, plots the species, gives relative
#' frequency table of river, and creates two csv files of ddt before and after subsetting
#'
#' @param df the ddt df
#' @param spec the chosen species
#'
#' @import dplyr
#' @import ggplot2
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @return plot of the given species, gives relative
#' frequency table of river, and creates two csv files of ddt before and after subsetting
#'
#'
#' @export
#'
#'@examples
#'\dontrun{
#'myddt(df = ddt, spec  = "CCATFISH")}
#'
#'
myddt <- function(df, spec){
  RIVER <-  WEIGHT <- LENGTH <- NULL

  #subset ddt by species
  df %>%
    filter(ddt$SPECIES == {{spec}}) -> data

  #write files after subsetting
  write.csv(data, paste('LvsWfor',spec,'.csv', sep=''))

  #create plot length vs weight for given species with
  #quadratic curve and point color corresponding with river
  plot = ggplot(dat, aes(x = ddt$WEIGHT, y = ddt$LENGTH)) +
    geom_point(aes(color = ddt$RIVER)) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
    labs(title="Madison Quiroga",
         subtitle=paste("Length vs Weight of", spec),
         x="Weight",
         y="Length")


  list('plot' = plot,
       'dataframe' = df,
       'subsetted dataframe' = dat,
       'relative frequency of river table' = table(df$RIVER)/length(df$RIVER))
}


