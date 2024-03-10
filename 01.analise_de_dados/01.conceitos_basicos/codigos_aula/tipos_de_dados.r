# Informacoes Basicas

x <- 5; x

y <- # comando incompleto

2

ls()

rm(x)

ls()

rm(list = ls())

ls()

# Tipos basicos

class("Ola")

class(1)

class(1L)

class(TRUE)

class(1 + 4i)
is.numeric(12)
is.integer(12)
is.integer(12L)
is.integer("12")

# Tipos Complexos

# Vetores
x <- c(0.5, 3.1, 0.6, 1.5); x

c(1, 1.5, 2, 2.5, 3, 3.5, 4)

x <- c(TRUE, FALSE, TRUE, TRUE, FALSE); x

(y <- c(T, F, F, F, F, F))

c("a", "b", "c")

c("R", "C", "Java", "C++", "Python")

c(12L, -3L, 7L, -2L)

c(1L, 2L, 3L, 4L, 5L)

c(1+0i, 2+4i)

c(1+1i, 2-2i, -3+3i, -4-4i)

1:10

10:20

-3:3

3.2:10

3:3

7:2

seq(from = 1, to = 10, by = 1)

seq(from = 1, to = 10, by = 2)

seq(from = 3, by = 3, length.out = 10)

rep(x = 1, times = 5)

rep(x = c(1,2,3), times = 2)

rep(x = c(1,2,3), times = 3, each = 2)

x <- c(1, 2, 3, 2, 1)
y <- 6:8
z <- rep(x = 7, times = 3)

c(x, y)
c(y, z)
c(x, 4, z, 5, 5, x)

x <- c(x, 7); x
x <- c(x, 5); x
x <- c(0, x); x

rm(x)
x <- c(x, 1)

x <- NULL; x
x <- c(x, 1); x
x <- c(x, 1); x

c(1.3, 2.1, 1.7, T, 1.2)

c(1.3, 2.1, 1.7, "a", 1.2)

c(TRUE, 2, FALSE, TRUE)

c("a", TRUE, 2, "b", FALSE, 1)

c(1+2i, 1.2, TRUE, 1-1i, -0.7, FALSE)

x <- -1.3:3; x

as.integer(x)

as.logical(x)

as.logical(as.integer(x))

x <- -1.3:3; x

as.complex(x)

y <- as.character(x); y

as.complex(y)

vector(mode = "numeric", length = 10)

vector(length = 8, mode = "integer")

vector("logical", length = 5)

vector("complex", 6)

vector("character", 12)

# Matrizes

m <- matrix(nrow = 2, ncol = 3); m

dim(m)

attributes(m)

matrix(data = 2, nrow = 2, ncol = 3)

matrix(data = 1:2, nrow = 2, ncol = 3)

matrix(data = 1:3, nrow = 2, ncol = 3)

matrix(data = 1:6, nrow = 2, ncol = 3)

matrix(data = 1:2, nrow = 2, ncol = 3, byrow = TRUE)

matrix(data = 1:3, nrow = 2, ncol = 3, byrow = TRUE)

matrix(data = 1:6, nrow = 2, ncol = 3, byrow = TRUE)

m <- 1:24; m

dim(m) <- c(4,6)

m

dim(m) <- c(2,4,3); m

x <- 1:3
y <- 7:9

rbind(x, y)

cbind(x, y)

rbind(1:4, 5:8, 3:6)

cbind(1:3, 9:7, 5:7, 6:4)

x <- 1:3; y <- 9:6

rbind(x, y)

x <- 1:3; y <- 9:6; z <- 4:5

cbind(x, y, z)

a <- matrix(data = 1:6, nrow = 2, ncol = 3); a

rbind(a, a)

a <- matrix(data = 1:6, nrow = 2, ncol = 3); a

cbind(a, a)

# Listas

list("a", 2.3, FALSE, 1+2i)

list(c(FALSE, 2), matrix(ncol = 3, nrow = 2), list("b", 3:5))

# Fatores

f <- factor(c("yes", "yes", "no", "yes", "no")); f

attributes(f)

factor(c("male", "female", "female", "male"))

factor(c("M", "G", "G", "P", "G", "M"))

factor(c("M", "G", "G", "P", "G", "M"), 
       levels = c("P", "M", "G"), ordered = TRUE)

# Data Frames
name <- c("Alice", "Bob", "Julia")

age <- c(19, 21, 20)

dados <- data.frame(nome = name, idade = age); dados

dados[[1]]

name <- c("Alice", "Bob", "Julia")

age <- c(19,21,20)

dados <- data.frame(nome = name, idade = age,
                    stringsAsFactors = TRUE); dados

dados[[1]]

cliente <- c(T,F,T)

dados <- cbind(dados, cliente); dados

jack <- data.frame(nome = "Jack", idade = 22, cliente = F); jack

dados <- rbind(dados, jack); dados

data.frame(foo = 6:9, bar = c(T, F, T, F))

data.frame(idade = c(25, 36, 24, 33), 
           cliente = c(F, T, F, F), 
           row.names = c("Aline", "Bianca", "Carlos", "Daniel"))

# Valores Desconhecidos

x <- c(1, 2, NaN, NA, 4)

is.na(x)

is.nan(x)

# Nomes

x <- c(24, 32, 28); x

names(x)

names(x) <- c("ana", "beatriz", "carlos") 

x

names(x)

x <- list(digitos = 0:9, letras = c("a", "b", "c"), alternativas = factor(c("V", "F")))

x

m <- matrix(1:12, nrow = 3, ncol = 4); m

dimnames(m) <- list(c("SP", "RJ", "MG"),c("A", "B", "C", "D"))

m

# Subconjuntos

x <- c("e", "b", "a", "d", "a", "f", "d"); x

x[1]

x[2:4]

x[c(1, 4, 3, 6, 3)]

x <- c(3, 7, 4, 1, 6, 2, 1); x

x[-2]

x[c(-3, -1, -6)]

x[-3:-5]

x

x > 2

x[x > 2]

pares <- (x %% 2 == 0); pares

x[pares]

m <- matrix(nrow = 3, ncol = 4, data = 1:12); 

m

m[2, 2]

m[1, 3]

m

m[2,  ]

m[ , 3]

m

m[ , 3, drop = FALSE]
m[2,  , drop = FALSE]

m

m[2:3, 2:4]

m

m[c(1,3), c(3, 2, 4)]

dimnames(m) <- list(c("SP", "RJ", "MG"), 
                    c("A", "B", "C", "D"))

m["RJ", c("A", "C")]
m["RJ", c("A", "C"), drop = FALSE]

x <- list(foo = 1:4, bar = 0.6, 
          msg = c("hello", "world"), 
          quiz = list(5, 7, 3))

x[1]

x[[1]]

x <- list(foo = 1:4, bar = 0.6, 
          msg = c("hello", "world"), 
          quiz = list(5, 7, 3))

x$bar

x["bar"]

x[["bar"]]

x <- list(foo = 1:4, bar = 0.6, 
          msg = c("hello", "world"), 
          quiz = list(5, 7, 3))

x[c(1, 3)]

x <- list(foo = 1:4, bar = 0.6,
       msg = c("hello", "world"),
       quiz = list(5, 7, 3))

x[[4]][[2]]

x[[c(4, 2)]]

x[[c(3, 2)]]

x <- list(foo = 1:4, bar = 0.6,
       msg = c("hello", "world"),
       quiz = list(5, 7, 3))

name <- "msg"

x[[name]]

x$name

x$msg

x <- list(foo = 1:4, bar = 0.6,
       msg = c("hello", "world"),
       quiz = list(5, 7, 3))

x$m

x[["m"]]

x[["m", exact = FALSE]]

d <- data.frame(foo = 6:8, bar = c(T, F, T)); d

d$fo

d[["ba"]]

d

d[2]

d[["bar"]]

d

names(d)

d$quiz <- c("cat", "dog", "rat"); d

d

names(d)

dd <- d

dd

dd[ , 1:2] <- dd[ , 2:1]; dd

dd[ , c("foo", "quiz")] <- dd[ , c("quiz", "foo")]; dd

names(dd)[1:2] <- names(dd)[2:1]

dd

d

d$bar <- NULL; d

names(d)[1] <- "tmp"

d

# Data e Hora

nascimento <- as.Date("1975-09-19")

nascimento

hoje <- Sys.Date(); hoje

hoje - nascimento

hoje - as.Date("1970/01/01")

unclass(hoje)

agora <- Sys.time(); agora

unclass(agora)

agora + 3600
agora + 7*24*3600

weekdays(agora)

agora

agora <- as.POSIXlt(agora)

names(unclass(agora))

unclass(agora)

s1 <- "15/08/2015-17:10"

t1 <- strptime(s1, "%d/%m/%Y-%H:%M")

t1

s2 <- "09/19/1994 04:13:47 PM"

t2 <- strptime(s2, "%m/%d/%Y %I:%M:%S %p")

t2

?strptime