#Implemente uma função mymin() que retorne o menor elemento de
#um vetor dado.

mymin <- function(vetor){
  minimo <- vetor[1]
  for(i in vetor){
    if(i<minimo){
      minimo <- i
      }
  }
return (minimo)
}

vetor <- c(3,4,1,5,6)
minimo_vetor <- mymin(vetor); minimo_vetor


#Implemente uma função myprod() que calcule o produto de
#todos os elementos de um vetor dado.

myprod <- function(vetor){
  prod <- 1
  for(i in vetor){
    prod<-prod*i
  }
return (prod)
}

prod_vetor <- myprod(vetor);prod_vetor


#Implemente uma função mymean() que retorne a média dos
#elementos de um vetor dado.

mymean <- function(vetor){
  soma<-0
  for(i in vetor){
    soma<-soma+i
    media<-soma/length(vetor)
  }
return (media)
}

med_vetor <- mymean(vetor);med_vetor


#Implemente uma função subset() que, dados dois conjuntos,
#verifique se o primeiro é um subconjunto do segundo.

vetor_1 <- c(1,2,3,4,5,6)
vetor_2 <- c(1,2,3)
vetor_3 <- c(7,8,9)

#primeira solução
subset <- function(vetor_a, vetor_b){
  all(is.element(vetor_a,vetor_b))
}

teste_subset <- subset(vetor_2, vetor_1); teste_subset

#alternativa - funciona melhor, as duas soluções são O(N*M), 
#mas a ela pode retornar antes e não percorrer o vetor inteiro
subset <- function(vetor_a, vetor_b){
  for(elem in vetor_a){
    if(!is.element(elem,vetor_b)){
      return(F)
    }
  }
  return(T)
}

teste_subset <- subset(vetor_3, vetor_1); teste_subset


#Implemente uma função index() que, dado um vetor e um
#elemento, retorne todas as posições do vetor que sejam iguais
#ao elemento.

#primeira solução
index <- function(vector, element){
  n <- length(vector)
  result <- NULL
  for(i in 1:n){
    if(vector[i] == element){
      result <- c(result, i)
    }
  }
  return(result)
}

indice <- index(vetor_3,8); indice

#alternativa - operacoes vetoriais incorporadas na propria linguagem
#bem mais rapida, independentemente do tamanho do vetor
index <- function(vector, element){
  (1:length(vector))[vector == element]
}

indice <- index(vetor_3,8); indice


#Implemente uma função gcd2() que, dados dois números
#inteiros não negativos, calcule o Máximo Divisor Comum
#(Greatest Common Divisor) dos números dados.
#Exercício que involve recursão - usa o algoritmo de Euclides, o primeiro da história
#Esse algoritmo é recursivo

gcd2 <- function(x,y){
  if (y==0){
    return(x)
  } else {
    return(gcd2(y,x%%y))
  }
}

gcd2(15,21)
