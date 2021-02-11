require(reshape)
require(ggplot2)

clear_data <- function(df) {
  df <- df[, (colnames(df) %in% c("Nazwa","Miesiące","Rok","Wartosc"))]
  df <- with(df, df[!(Wartosc == "" | is.na(Wartosc)), ])
  df <- with(df, df[!(Rok < 2013 | is.na(Rok)), ])
  return(df)
}

analyzing_data <- function(df, product) {
  df = as.data.frame(xtabs(Wartosc ~ Nazwa, df))
  names(df) <- c("Wojewodztwa", "Cena")
  df["Produkty"] = product
  df$Cena <- as.numeric(as.character(df$Cena)) / (7*12)
  return(df)
}

products = c("Cebula", "Marchew", "Pomarancze", "Jablka", "Cytryny")
lista = list()

for (i in 1:length(products)){
  path = sprintf("~/Pulpit/Statystyka/%s.csv", products[i])
  df <- read.csv(path, TRUE, ";", dec=",")
  df <- clear_data(df)
  df <- analyzing_data(df, products[i])
  lista[[i]] <- df
}

df_list <- merge_recurse(lista)

plot <- ggplot(df_list, aes(x=Wojewodztwa, y=Cena, fill = Produkty)) + geom_col()
plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

