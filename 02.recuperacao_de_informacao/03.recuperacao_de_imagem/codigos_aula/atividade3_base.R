
#------------------------------------------------#
# Funcao para exibir uma imagem                  #
#------------------------------------------------#
show_image <- function(img, classe){
  displayImg(img)
  title(classe)
}

#------------------------------------------------#
# Carrega e retorna todas as imagens             #
#------------------------------------------------#
read_images <- function(path){
  name_imgs <- list.files(path, full.names = TRUE)
  all_im <- lapply(name_imgs, load.image)
  
  return(all_im)
}

#------------------------------------------------#
# Retorna ground_truth escolhida classe relevante#
#------------------------------------------------#
get_ground_truth<- function(classes, classe_relevante){
  ground_truth <- integer(length(classes))
  ground_truth[which(classes %in% classe_relevante)] <-1
  return(ground_truth)
}