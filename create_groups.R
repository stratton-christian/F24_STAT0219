# generate groups
names <- c(
  "An Adhikari",
  "Eujin Chae",
  "Bryce Charron",
  "Laura Caroline Clervil",
  "Logan Cosgrove",
  "Anna Krouse",
  "Carly McAdam",
  "Henrik David Nelson",
  "Liz Rightmire",
  "Isaac Luis Rosario",
  "Bruck Setu",
  "Lia Smith",
  "Hedavam Rafael Solano",
  "Xander Swann",
  "Isaac Thompson",
  "Grayson David Wade",
  "Kate Wohl",
  "Yeimy Lizeth Zacarias",
  "Claire Runqi Zhang",
  "Charlotte Zhuang"
)

create_groups <- function(seed = NULL, names, n = 4){
  if(!is.null(seed)) set.seed(seed)
  order <- sample(1:length(names), replace = F)
  matrix(names[order], ncol = n)
}

create_groups(09092024, names, 4)
