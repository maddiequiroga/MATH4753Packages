#' myddt() for project 1
#'
#' @param df ddt datafrane
#' @param x length
#' @param y weight
#' @param SPECIES chosen species
#' @param col rivers
#'
#' @return a scatter plot, two csv files, and a relative frequency table
#
#' @examples
#' \dontrun{
#' myddt(ddt, "LENGTH", "WEIGHT", "CCATFISH", "RIVER")}
myddt <- function(df, x, y, SPECIES, col){
  WEIGHT <-  LENGTH <- RIVER <- NULL


  dat <- df %>% filter(SPECIES=={{SPECIES}})
  write.csv(dat, paste('LvsWfor',SPECIES,'.csv', sep=''))

   g <- ggplot(df, aes_string(x=x,y=y)) +
    geom_point(aes_string(color = col )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") + ggtitle("Madison Quiroga")
  print(g)

  list('dataframe' = df,
       'subsetted dataframe' = dat,
       'relative frequency of river table' = table(df$RIVER)/length(df$RIVER))
}
