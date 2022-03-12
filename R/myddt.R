
#' myddt function
#'
#' @param df datqframe
#' @param spec chosen species
#'
#' @return a plot of the chosen species, the sf before and after subsetting,
#' and a relative frequency table of rivers
#' @export
#'
#' @examples
#' /dontrun{
#' myddt(df = ddt, specie = "CCATHFISH)
#' }
myddt <- function(df, species){
  RIVER <-  WEIGHT <- LENGTH <- NULL

  #subset ddt by species
  df %>% filter(ddt$SPECIES == {{species}}) -> data

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


