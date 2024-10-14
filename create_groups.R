# generate groups
names <- c(
  "An Adhikari",
  "Eujin Chae",
  "Bryce Charron",
  "Laura Clervil",
  "Logan Cosgrove",
  "Anna Krouse",
  "Carly McAdam",
  "Henrik Nelson",
  "Lucas Nerbonne",
  "Liz Rightmire",
  "Isaac Rosario",
  "Bruck Setu",
  "Hedavam Solano",
  "Xander Swann",
  "Isaac Thompson",
  "Grayson Wade",
  "Kayley Watson",
  "Kate Wohl",
  "Charlotte Zhuang"
)

create_groups <- function(seed = NULL, names, n = 4){
  if(!is.null(seed)) set.seed(seed)
  order <- sample(1:length(names), replace = F)
  matrix(names[order], ncol = n)
}

create_groups(10142024, names, 4)
