# Definindo Funcoes

mysum <- function(a, b) {
  a + b
}

class(mysum)

mysum

makePower <- function(n) {
  function(x) {
    x^n
  }
}

square <- makePower(2)
cube <- makePower(3)

subvector <- function(vector, begin = 1, end = length(vector)) {
  return(vector[begin:end])
}

mydist <- function(x = c(0, 0), y = c(0, 0)) {
  sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2)
}

truncd <- function(n, digits = 0) {
  trunc(n * 10^digits) / 10^digits
}

mylength <- function(...) {
  length(c(...)) 
}

mylength <- function(...) {
  length(list(...)) 
}

# Comandos Condicionais

odd <- function(x) {
  if (x %% 2 == 1) {
    TRUE
  } else {
    FALSE
  }
}

odd <- function(x) {
  ifelse(x %% 2 == 1, TRUE, FALSE)
}

odd <- function(x) {
  x %% 2 == 1
}

mymin <- function(a, b) {
  if (a < b) return(a)
  else return(b)
}

mymin <- function(a, b) {
  if (a < b) return (a)
  else return(b)
}

myabs <- function(a) {
  if (a < 0) {
    -a
  } else {
    a
  }
}

bhaskara <- function(a = 0, b = 0, c = 0) {
  if (a != 0) {
    delta <- as.complex(b^2 - 4*a*c)
    if (delta != 0) {
      return(c((-b + sqrt(delta)) / (2 * a), 
               (-b - sqrt(delta)) / (2 * a)))
    } else {
      return(-b / (2 * a))
    }
  } else {
    return(-c / b)
  }
}

# Comandos de Repeticao

printVector <- function(v) {
  i <- 1
  while (i <= length(v)) {
    print(v[i])
    i <- i + 1
  }
}

printVector <- function(v) {
  for (i in v) {
    print(i)
  }
}

mysum <-function(...) {
  x <- 0
  for (i in c(...)) {
    x <- x + i
  }
  return(x)
}

mylength <- function(vector) {
  k <- 0
  for (i in vector) {
    k <- k + 1
  }
  return(k)
}

mylength <- function(...) {
  k <- 0
  for (i in c(...)) {
    k <- k + 1
  }
  return(k)
}

multlength <- function(...) {
  result <- NULL
  for (i in list(...)) {
    result <- c(result, length(i))
  }
  return(result)
}

multlength(25:30, matrix(1:12, 3, 4), 
           rnorm(5), sample(10))

mymin <- function(...) {
  min <- Inf
  for (i in c(...)) {
    if (i < min) {
      min <- i
    }
  }
  return(min)
}

mymin <- function(...) {
  min <- Inf
  if (missing(...)) {
    warning("missing arguments; returning Inf")
  } else {
    for (i in c(...)) {
      if (i < min) {
        min <- i
      }
    }
  }
  return(min)
}

# lapply

L <- list(a = 25:30, 
          b = matrix(1:12, 3, 4), 
          c = rnorm(100), 
          d = sample(10))
lapply(L, mean)

lapply(L, length)

lapply(2:4, runif)

lapply(2:4, runif, min = 0, max = 10)

lapply(datasets::faithful, max)

lapply(faithful, min)

lapply(faithful, function(x) {max(x) - min(x)})

# sapply

sapply(L, mean)

sapply(L, range)

lapply(faithful, range)

lapply(faithful, quantile)

sapply(faithful, quantile)

sapply(faithful, quantile, c(0, 1/3, 2/3, 1))

sapply(faithful, quantile, seq(0, 1, 0.1))

# apply

m <- matrix(sample(12), nrow = 3, ncol = 4); m

apply(m, 1, min)

apply(m, 2, max)

m <- matrix(sample(8))
dim(m) <- c(2, 2, 2); m

apply(m, 1, mean)

apply(m[ , , 1], 1, mean)

apply(m, 2, mean)

apply(m[ , , 2], 2, mean)

total <- sum(datasets::HairEyeColor); total

apply(HairEyeColor, 1, sum) / total
apply(HairEyeColor, 2, sum) / total
apply(HairEyeColor, 3, sum) / total

# mapply

mapply(rep, 1:3, 5)
mapply(rep, 5, 1:3)

mapply(log, 2:5, 2:3)

tipo1 <- sample(10:99, 10); tipo1
tipo2 <- sample(10:99, 10); tipo2
tipo3 <- sample(10:99, 10); tipo3
tipo4 <- sample(10:99, 10); tipo4

mapply(min, tipo1, tipo2, tipo3, tipo4)
mapply(max, tipo1, tipo2, tipo3, tipo4)
mapply(function (...) {mean(c(...))}, 
       tipo1, tipo2, tipo3, tipo4)

(tipo1 <- tipo1 >= 50)
(tipo2 <- tipo2 >= 50)
(tipo3 <- tipo3 >= 50)
(tipo4 <- tipo4 >= 50)

mapply(all, tipo1, tipo2, tipo3, tipo4)
mapply(any, tipo1, tipo2, tipo3, tipo4)

# tapply

x <- c(rnorm(100), runif(100), sample(100))
f <- gl(n = 3, k = 100, 
        labels = c("norm", "unif", "sample"))

df <- data.frame(x, f)
tapply(df$x, df$f, range)

df <- df[sample(nrow(df)), ]
tapply(df$x, df$f, range)
tapply(df$x, df$f, mean)

tapply(datasets::mtcars$mpg,
       datasets::mtcars$cyl, mean)
tapply(mtcars$qsec, mtcars$cyl, mean)
tapply(mtcars$hp, mtcars$vs, mean)

tapply(row.names(mtcars), mtcars$gear, c)
tapply(mtcars$hp, mtcars$gear, c)
tapply(mtcars$hp, mtcars$gear, length)

tapply(mtcars$hp, list(mtcars$gear, mtcars$cyl), mean)
tapply(mtcars$hp, list(mtcars$gear, mtcars$cyl), length)

qfactor <- function(vector) {
  q <- quantile(vector)
  result <- NULL
  for (i in vector)
    if (i <= q["25%"])
      result <- c(result, "q1")
  else if (i <= q["50%"])
    result <- c(result, "q2")
  else if (i <= q["75%"])
    result <- c(result, "q3")
  else result <- c(result, "q4")
  return(as.factor(result))
}

qfactor(mtcars$hp)

tapply(mtcars$mpg, qfactor(mtcars$hp), mean)
tapply(mtcars$mpg, qfactor(mtcars$qsec), max)
tapply(mtcars$hp, qfactor(mtcars$mpg), mean)

tapply(datasets::Loblolly$height,
       datasets::Loblolly$age, min)
tapply(Loblolly$height, Loblolly$age, mean)
tapply(Loblolly$height, Loblolly$age, max)

tapply(datasets::airquality$Temp,
       datasets::airquality$Month, mean)
tapply(airquality$Solar.R, airquality$Month, 
       mean, na.rm = TRUE)
tapply(airquality$Ozone, airquality$Month, 
       mean, na.rm = TRUE)
tapply(airquality$Temp, airquality$Wind > 10,
       mean)
tapply(airquality$Temp, 
       ifelse(airquality$Wind > 10, 
              "Windy", "Not Windy"), mean)

# Agrupando com dois critérios
tapply(airquality$Temp, 
       list(airquality$Month, 
            ifelse(airquality$Wind > 10, 
                   "Windy", "Not Windy")), mean)
tapply(airquality$Temp, 
       list(airquality$Month, 
            ifelse(airquality$Wind > 10, 
                   "Windy", "Not Windy")), length)

tapply(datasets::iris$Petal.Length,
       datasets::iris$Species, mean)
tapply(iris$Petal.Width, iris$Species, mean)
tapply(iris$Petal.Length / iris$Petal.Width, 
       iris$Species, mean)

tapply(iris$Petal.Length, iris$Species, summary)
simplify2array(tapply(iris$Petal.Length, 
                      iris$Species, summary))

tapply(esoph$ncases, esoph$agegp, sum) # casos com cancer
tapply(esoph$ncontrols, esoph$agegp, sum) # casos sem cancer

# Fracao de pessoas com cancer por faixa de idade
tapply(esoph$ncases, esoph$agegp, sum) / 
  (tapply(esoph$ncases, esoph$agegp, sum) + 
     tapply(esoph$ncontrols, esoph$agegp, sum))

# Fracao das pessoas com cancer na mostra em cada faixa de idade 
tapply(esoph$ncases, esoph$agegp, sum) / 
  sum(tapply(esoph$ncases, esoph$agegp, sum))

# Fracao de pessoas com cancer por faixa de consumo de alcool
tapply(esoph$ncases, esoph$alcgp, sum) / 
  (tapply(esoph$ncases, esoph$alcgp, sum) + 
     tapply(esoph$ncontrols, esoph$alcgp, sum))

# Fracao das pessoas com cancer na mostra em cada faixa de consumo de alcool
tapply(esoph$ncases, esoph$alcgp, sum) / 
  sum(tapply(esoph$ncases, esoph$alcgp, sum))

# Fracao de pessoas com cancer por faixa de consumo de tabaco
tapply(esoph$ncases, esoph$tobgp, sum) / 
  (tapply(esoph$ncases, esoph$tobgp, sum) + 
     tapply(esoph$ncontrols, esoph$tobgp, sum))

# Fracao das pessoas com cancer na mostra em cada faixa de consumo de tabaco
tapply(esoph$ncases, esoph$tobgp, sum) / 
  sum(tapply(esoph$ncases, esoph$tobgp, sum))