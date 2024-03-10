#Exercício 1 - Filtrar inteiros
x <- sample(seq(1.00, 6.75, 0.25)); x
x[x%%1 == 0]

#Exercício 2 - encontrar elementos repetidos, resposta 1 2 5
z <- c(1,2,1,5,3,4,1,5,2,8,1)
sort(unique(z[duplicated(z)]))

#Exercício 3 - encontrar elementos que não são repetidos

setdiff(z,duplicated(z))

#Exercício 4 - 