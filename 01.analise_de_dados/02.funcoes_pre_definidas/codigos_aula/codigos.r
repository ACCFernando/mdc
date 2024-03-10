# Operações Vetorizadas

3 + 4

a <- 1:5; a

a + 10

a

a - 3

3 * a

a / 4

4 / a

a

a ^ 2

2 ** a

a %% 4

a %/% 2

a <- c(1, 2, 3, 4)
b <- c(5, 6, 7, 8)
c <- c(3, 4)

a / b

a / c

a <- c(1, 2, 3, 4)
b <- c(5, 6, 7, 8)
d <- c(2, 3, 4)

a * b

a / d

b <- c(5, 6, 7, 8)

c <- c(3, 4)

d <- c(2, 3, 4)

b * c

(b - c) * d


3 > 4

a

a > 4

a != 3

1 < a

a

a == 2

a >= 0

(a ^ 2) > 10

2 ^ (a < 4)

b <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE); b

c <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE); c

b | c

b & c

b

c

!b

(sqrt(10) > 3) || (teste > 0)  # "or preguiçoso"
(sqrt(10) > 3) | (teste > 0)

(sqrt(10) < 3) && (teste > 0)  # "and preguiçoso"
(sqrt(10) < 3) & (teste > 0)

b

c

x <- c(TRUE, FALSE, NA)

names(x) <- c("TRUE", "FALSE", "NA"); x

outer(x, x, "&")

x

outer(x, x, "|")

!x

x

outer(x, x, "==")

x <- matrix(1:4, nrow =  2, ncol = 2); x

y <- matrix(10:7, nrow =  2, ncol = 2); y

y - x

x

y

x*y

x

y

x %*% y # multiplicação de matrizes

# Constantes

letters    # letras minúsculas

LETTERS    # letras maiúsculas

month.abb  # mêses do ano (abreviados)

month.name # mêses do ano

pi         # constante pi

pi - 3.141593

print(pi, digits = 10)

# Funções matemáticas básicas

sqrt(c(1024, 49, 225))

exp(c(2, 3, 4))

factorial(c(3, 5, 4, 6))

log(c(1, 1000, 1024))   # log na base e

log2(c(1, 1000, 1024))  # log na base 2

log10(c(1, 1000, 1024)) # log na base 10

log(1000, base = 10)

log(1024, 2)

ceiling(c(-2.2, -1.8, 2.1, 2.7))

floor(c(-2.2, -1.8, 2.1, 2.7))

trunc(c(-2.2, -1.8, 2.1, 2.7))

round(c(-2.2, -1.8, 2.1, 2.7))

round(-3.5:4)

round(6.12345:12, digits = 3)

signif(6.12345:12, digits = 4)

abs(-2.2:3)

x <- seq(1, 10, 0.25)
x <- sample(x); x

# Funções Trigonométricas

sin(c(0, pi/4, pi/2, pi))

cos(c(0, pi/4, pi/2, pi))

tan(c(0, pi/4, pi/2, pi))

acos(c(1, 0, -1))

# Funções para Sequências, Vetores, Listas e Fatores

f <- gl(3, 10, length = 25, 
        labels = c("bronze", "prata", "ouro"), 
        ordered = TRUE)

x <- c(7, 2, 1, 6, 3, 4, 1, 5, 0, 8, 2); x

length(x)

length(x) <- 10; x

length(x) <- 12; x

outer(1:10, 1:10, "*")

outer(1:4, 1:4, log)

x <- runif(10); x
  
sort(x)

sort(x, decreasing = TRUE)

x

order(x)

rank(x)

x[order(x)]

rev(x)

z <- sample(5, 10, replace = TRUE); z

unique(z)

duplicated(z)

e <- c(1, 2, 1, 5, 3, 4, 1, 5, 2, 8, 1); e

x <- c(7, 2, 1, 6, 3, 4, 1, 5, 0, 8); x

sum(x)
prod(x)
prod(1:6)

cummin(x)
cummax(x)
cumsum(x)
cumprod(x)

m <- matrix(data = sample(30), nrow = 5, ncol = 6); m

sum(m)
rowSums(m)
colSums(m)

# Funções para Conjuntos

x <- c(1, 2, 3, 4, 5); x

y <- c(3, 4, 5, 6, 7); y

union(x, y)

intersect(x, y)

setequal(x, y)

setdiff(x, y)

setdiff(y, x)

is.element(x, y)

is.element(y, x)

setequal(c(1, 2, 3), c(2, 3, 1))

e <- c(1, 2, 1, 5, 3, 4, 1, 5, 2, 8, 1)

# Funções Probabilíssticas

runif(100, min = 0, max = 10)
rnorm(100, mean = 0, sd = 10)

sample(10)
sample(letters)
sample(1:60, 6)
sample(1:10, 100) # Erro!
sample(1:10, 100, replace = TRUE)

x <- sample(c("a", "b"), 10000, prob = c(1, 2), replace = TRUE); x

length(x[x == "a"])
length(x[x == "b"])

# Funções Estatíssticas

x <- runif(10000)

mean(x)

m <- matrix(data = sample(25), nrow = 5, ncol = 5); m

mean(m)
rowMeans(m)
colMeans(m)

min(x)
max(x)
range(x)
median(x)
quantile(x)
quantile(x, c(0.1, 0.3, 0.5, 0.7, 0.9))
summary(x)
var(x)
sd(x)
sqrt(var(x))

x <- c(3, 2, 5, 7, 9, 1); x

mean(x)

median(x)

sd(x)

x <- runif(1000)
y <- runif(1000)
cor(x, y)
cor(x, x^2)
cor(x, 5*x)
cor(y, -y)
cor(log(x), x^2)

# Funções para Strings

x <- "Mineracao de Dados Complexos"
y <- "Analise de Dados"

nchar(c(x, y))
strsplit(c(x, y), split = " ")

substr(x, start = 11, stop = 18)
substr(x, start = 11, stop = 18) <- y
chartr("zenitpolar", "polarzenit", y)

x <- "MDC"
y <- "Analise de Dados"

paste(x, y, sep = " - ")
paste(letters, sep = "")
paste(letters, collapse = "")

strtrim(c(x, y), 7)
tolower(y)
toupper(y)

x <- "Mineracao de Dados Complexos"

y <- "Analise de Dados"

sub("a", "*", y)

gsub("os", "_-_", x)

gsub("a", "", y)

grep("Dados", c(x, "MDC", y))

# Funções Lógicas

x <- c(F, T, F); x

any(x)

y <- c(F, F, F); y

any(y)

x <- c(T, T, F); x

all(x)

y <- c(T, T, T); y

all(y)

# Função do.call()

param <- as.list(letters)

param$sep <- ""

param

do.call(paste, param)
