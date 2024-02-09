########################################
# Teste 1a - INF-0612          
# Nome(s): Leonardo Cesar Silva dos Santos e Fernando Augusto Candalaft
########################################

## Os vetores C, L e V representam os produtos distribuidos nas cidades de Campinas, Limeira e Vinhedo, respectivamente.

C <- c("Xampu", "Sabonete", "Arroz", "Chocolate", 
       "Leite", "Refrigerante", "Queijo", "Suco", "Vinho", "Cerveja")
L <- c("Leite", "Cerveja", "Arroz", "Chocolate")
V <- c("Sabonete", "Detergente", "Refrigerante", "Carne", "Vinho", 
       "Chocolate", "Papel", "Leite", "Iogurte")


## Perguntas:
## Quais os produtos que sao vendidos em Campinas, mas nao sao vendidos em Limeira?
campinas <- tolower(C); limeira <- tolower(L); vinhedo <- tolower(V)

prod_campinas_nao_limeira <- setdiff(campinas, limeira)
# Lista de produtos que estão em Campinas mas não em Limeira
print(prod_campinas_nao_limeira)

## Quais os produtos que sao vendidos em Vinhedo, mas nao sao vendidos em Campinas?
prod_vinhedo_nao_campinas <- setdiff(vinhedo, campinas)
# Lista de produtos que estão em Vinhedo mas não em Campinas
print(prod_vinhedo_nao_campinas)

## Quais os produtos que sao vendidos em pelo menos uma cidade?
lista_todos_prods <- union(campinas, limeira)
lista_todos_prods <- union(lista_todos_prods, vinhedo)
# Lista de produtos que estão em pelo menos uma das cidades
print(lista_todos_prods) # A operação "union" elimina qualquer duplicidade

## Quais os produtos que sao vendidos em todas as cidades?
lista_prods_presentes_cidades <- intersect(campinas, limeira)
lista_prods_presentes_cidades <- intersect(lista_prods_presentes_cidades, vinhedo)

# Lista de produtos presentes em todas as cidades
print(lista_prods_presentes_cidades)

## Se a filial de Limeira parar a distribuicao de produtos, a filial de Campinas 
## possui todos os itens necessarios para atender a demanda de Limeira? 

# (Outra forma de formular a pergunta: a lista de produtos vendidos em Limeira está contida na 
# lista de produtos vendidos em Campinas?)
limeira_em_campinas_bool <- is.element(limeira, campinas)
campinas_atende_limeira <- all(limeira_em_campinas_bool)
# Resposta para a pergunta se a filial Campinas atende a cidade de Limeira
print(campinas_atende_limeira)
