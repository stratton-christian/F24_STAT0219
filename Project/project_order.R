# project order
## groups
groups <- list(
  g1 = c("An", "Hedavam", "Charlotte"),
  g2 = c("Eujin", "Bryce"),
  g3 = "Laura",
  g4 = c("Logan", "Kate"),
  g5 = "Carly",
  g6 = c("Anna", "Lucas", "Liz"),
  g8 = "Isaac R",
  g9 = c("Bruck", "Grayson", "Kayley"),
  g10 = "Isaac T"
)

set.seed(12022024)
groups[sample(1:length(groups), length(groups), replace = F)]

# course response forms:  https://go.middlebury.edu/crf