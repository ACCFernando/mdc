########################################
# Teste 1a - INF-0612          
# Nome(s): Fernando Candalaft, Leonardo Cesar Silva dos Santos
########################################

## Os vetores C, L e V representam os produtos distribuidos nas cidades de Campinas, Limeira e Vinhedo, respectivamente.

C <- c("Xampu", "Sabonete", "Arroz", "Chocolate", "Leite", "Refrigerante", "Queijo", "Suco", "Vinho", "Cerveja")
L <- c("Leite", "Cerveja", "Arroz", "Chocolate")
V <- c("Sabonete", "Detergente", "Refrigerante", "Carne", "Vinho", "Chocolate", "Papel", "Leite", "Iogurte")


## Perguntas:
## Quais os produtos que sao vendidos em Campinas, mas nao sao vendidos em Limeira?

diff_C_L <- setdiff(C,L); 
print("Produtos vendidos em Campinas, mas não em Limeira:"); diff_C_L

## Quais os produtos que sao vendidos em Vinhedo, mas nao sao vendidos em Campinas?

diff_V_C <- setdiff(V,C); 
print("Produtos vendidos em Vinhedo mas não em Campinas:"); diff_V_C

## Quais os produtos que sao vendidos em pelo menos uma cidade?

union_C_L <- union(C,L)
union_C_L_V <- union(union_C_L,V);
print("Produtos vendidos em pelo menos uma cidade:"); union_C_L_V

## Quais os produtos que sao vendidos em todas as cidades?

names(C) <- C
names(L) <- L
names(V) <- V

c_l = C[L]
c_l_v = V[c_l] 

print("Produtos vendidos em todas as cidades:"); c_l_v[!is.na(c_l_v)]

## Se a filial de Limeira parar a distribuicao de produtos, a filial de Campinas 
## possui todos os itens necessarios para atender a demanda de Limeira? 

diff_L_C = setdiff(L,C)
print("Filial de campinas possui todos os items"); diff_L_C

