# Práctica 3.2 Minería de Datos: Árboles de Decisión (Regresión)
# Brenda Itzel Guzmán Bonilla
# Brenda García Briones
# María José Merino Pérez


#   EVALUACIÓN ÁRBOL DE DECISIÓN

source("DecisionTree.R")

data <- read.csv("weather_regresion.csv", header = TRUE)
#data <- read.csv("weather.csv", header = TRUE)
# Entrenamiento del árbol de decisión
colnames(data)[colnames(data) == "Hours_Played"] <- "label"

X <- data[, names(data) != "label", drop = FALSE]
Y <- data$label  

# Construir el árbol de regresión
tree <- build_regression_tree(X, Y, max_depth = 3, min_samples_leaf = 2)

# Imprimir el árbol
print_regression_tree(tree)


