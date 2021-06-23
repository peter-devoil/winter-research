library(tidyverse)

renamer <- function (name) {
  if (name == "pickup") {
    return("1")
  }
  if (name == "suv") {
    return("2")
  }
  if (name == "minivan") {
    return("3")
  }
  if (name == "2seater") {
    return("4")
  }
  if (name == "midsize") {
    return("5")
  }
  if (name == "subcompact") {
    return("6")
  }
  if (name == "compact") {
    return("7")
  }
  return(name)
}
RENAMER = Vectorize(renamer, "name")
half <- function(x) (x / 2)


  tibble(data.frame(map_at(tibble(data.frame(map_at(mpg, "class", RENAMER))), "hwy", half))) %>%
  mutate(class = fct_reorder(class, hwy, .fun='median')) %>%
  ggplot(mapping = aes(x=reorder(class, hwy), y=hwy, fill=class)) + #fill = class totally unnecessary
  geom_violin(show.legend = FALSE)+
  geom_hline(yintercept = 12) +
  xlab("Days from today")+
  ylab("Soil temperature (degrees C)")+
  ggtitle("Probability Densities of temperature for each day", "Cheesed from the mpg dataframe")
  

