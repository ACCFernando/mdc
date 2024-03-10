#########
# Color Histograms
#########
# Extraindo o vetor de carecterísticas, juntando os
# histogramas de cada banda (255 dimensoes por cada banda)
extract_color_hist <- function(path_img){
  img <- load.image(path_img)
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}


#########
# Color Moments
#########
extract_color_moments <- function(path_img){
  # carregando a imagem
  img <- load.image(path_img)
  # atualizando os valores dos canais 
  img[,,1:3] <- img[,,1:3]*255
  # computando o número de pixels
  n <- dim(img)[1] * dim(img)[2]
  # computando o momento 1 para os 3 canais
  m1 <- c(sum(img[,,1]), sum(img[,,2]), sum(img[,,3]))/n
  
  # cálculo comum ao demais momentos de cor
  I_E <- list(img[,,1] - m1[1], img[,,2] - m1[2], img[,,3] - m1[3])
  # computando o momento 2 para os 3 canais
  m2 <- c(sum(I_E[[1]]^2), sum(I_E[[2]]^2), sum(I_E[[3]]^2))/n
  m2 <- m2^(1/2)
  
  # computando o momento 3 para os 3 canais
  cuberoot <- function(x)sign(x)*abs(x)^(1/3)
  m3 <- c(sum(I_E[[1]]^3), sum(I_E[[2]]^3), sum(I_E[[3]]^3))/n
  m3 <- cuberoot(m3)
  
  # computando o momento 4 para os 3 canais
  m4 <- c(sum(I_E[[1]]^4), sum(I_E[[2]]^4), sum(I_E[[3]]^4))/n
  m4 <- m4^(1/4)  
  
  return(c(m1, m2, m3, m4))
}

get_ranking_by_moment <- function(M, query, w){
  moment_distance <- function(img_a, img_b, w) {
    return (sum(abs(img_a-img_b) * w))
  }
  d <- lapply(rownames(M), function(x) moment_distance(M[query,], M[x,], w))
  d <- unlist(d)
  names(d) <- rownames(M)
  return(names(sort(d)))
}

#########
# Color Coherence Vector
#########
get_connected_componnents <- function(img) {
  n <- dim(img)[1]
  m <- dim(img)[2]
  C <- matrix(0, nrow = n, ncol = m)
  C_colors <- list()
  K <- 1
  for (a in 1:n) {
    for (b in 1:m) {
      if (C[a,b] == 0){
        C[a,b] <- K
        C_colors[length(C_colors) + 1] <- list(img[a,b,])
        Q <- list(c(a,b))
        q <- 1
        while(q <= length(Q)){
          i <- Q[[q]][1]
          j <- Q[[q]][2]
          for (x in -1:1) {
            for (y in -1:1) {
              i_ <- i + x
              j_ <- j + y
              if (i_ > 0 && i_ <= n && j_ > 0 && j_ <= m) {
                if(identical(img[i,j,], img[i_,j_,]) && C[i_, j_] == 0){
                  Q[[length(Q) + 1]] <- c(i_, j_)
                  C[i_,j_] <- K
                }
              }
            }
          }
          q <- q + 1
        }
        K <- K + 1
      }
    }
  }
  return(list(C, C_colors))
};


extractCCV <- function(path_img, tau = -1) {
  img <- load.image(path_img)
  n <- dim(img)[1][1]
  m <- dim(img)[2][1]
  if (tau == -1) {
    tau <- round(n*m*0.01)
  }
  
  # discretizando as cores
  img[,,1] <- (img[,,1]*255) %/%64  
  img[,,2] <- (img[,,2]*255) %/%64  
  img[,,3] <- (img[,,3]*255) %/%64  
  
  # matriz com as components conexas 
  result <- get_connected_componnents(img[,,])
  C <- result[[1]]
  C_colors <- result[[2]]
  
  # conta o tamanho das componentes
  size_comps <- tapply(unlist(C),factor(unlist(C)),length)
  
  # criação dos descritores
  alpha <- rep(0, 64)
  beta <- rep(0, 64)
  dim(alpha) <- c(4,4,4)
  dim(beta) <- c(4,4,4)
  
  # atualização dos descritores
  for (k in 1:length(size_comps)){
    pos_i <- C_colors[[k]][1] + 1
    pos_j <- C_colors[[k]][2] + 1
    pos_k <- C_colors[[k]][3] + 1
    if (size_comps[k] >= tau){
      alpha[pos_i, pos_j, pos_k] <- alpha[pos_i, pos_j, pos_k] + size_comps[[k]]
    }else{
      beta[pos_i, pos_j, pos_k] <- beta[pos_i, pos_j, pos_k] + size_comps[[k]]
    }
  }
  return(list(alpha, beta))
}

distanceCCV <- function(descA, descB){
  
  alphaA <- descA[[1]]
  alphaB <- descB[[1]]
  betaA <- descA[[2]]
  betaB <- descB[[2]]
  
  # Comparação feita usando distância L1 
  return (sum(abs(alphaA - alphaB) + abs(betaA - betaB)))
}


get_ranking_CCV <- function(path_img, name_imgs, query){
  d <- c()
  descQuery <- extractCCV(path_img[query])
  for(i in 1:length(path_img)){
    cat("processing", path_img[i], "...\n")
    descImg <- extractCCV(path_img[i])
    d[i] <- distanceCCV(descImg, descQuery)
  }
  names(d) <- name_imgs
  d <- sort(d)
  return(names(d))
}


#########
# Border/Interior Classification
#########
border_intern <- function(img){
  n <- dim(img)[1]
  m <- dim(img)[2]
  
  viz4 <- list(c(-1,0), c(0,-1), c(0,1), c(1,0))  
  C <- matrix(F, nrow = n, ncol = m)
  
  for (i in 1:n){
    for (j in 1:m){
      interno <- T
      for (viz in viz4){
        i_ <- i + viz[1]
        j_ <- j + viz[2]
        if (i_ > 0 && i_ <= n && j_ > 0 && j_ <= m) {
          if(!identical(img[i,j], img[i_,j_])){
            interno = F
            break
          }
        }
      } 
      C[i,j] <- interno
    }
  }
  
  return(C)
}

BIC <- function(path_img){
  img <- load.image(path_img)
  n <- dim(img)[1][1]
  m <- dim(img)[2][1]
  
  # discretizando as cores
  img[,,1] <- (img[,,1]*255) %/% 64  
  img[,,2] <- (img[,,2]*255) %/% 64  
  img[,,3] <- (img[,,3]*255) %/% 64  
  img <- img[,,,]
  int_img <- matrix(0, nrow=n, ncol=m)
  for (i in 1:n){
    for (j in 1:m){
      int_img[i,j] <- img[i,j,3] + img[i,j,2]*4 + img[i,j,1]*16 + 1
    }
  }
  
  C <- border_intern(int_img)  
  
  intern_hist <- hist(int_img[C], plot=FALSE, breaks=0:64)$counts
  border_hist <- hist(int_img[!C], plot=FALSE, breaks=0:64)$counts
  
  return (c(intern_hist, border_hist))
}

get_ranking_BIC <- function(path_img, name_imgs, query){
  descQuery <- BIC(path_img[query])
  get_distance <- function(img) {
    cat("processing ", img, "...\n")
    # extraindo o vetor de caracteristica com o método BIC
    descImg <- BIC(img)
    return(dist(t(data.frame(descImg, descQuery)), 
                method = "manhattan"))
  }
  # computa as distancias da imagem de consulta para todas as imagens
  distances <- unlist(lapply(path_img, get_distance))
  # recuperando as referencias das imagens
  names(distances) <- name_imgs
  # retornando o ranking (referencias das imagens ordenadas)
  return(names(sort(distances)))
}

#########
# Co-ocurrence Matrix
#########
get_ranking_cooccurrence <- function(path_imgs, name_imgs, query){
  
  # carregando a imagem de consulta
  query <- readTIFF(path_imgs[query], as.is = TRUE, info = TRUE)
  # obtendo o descritor para a imagem de consulta
  descQuery <- c(glcm(query)$glcm$ave)
  
  # carrega uma imagem, extrai o descritor e computa a distancia 
  # para a imagem de consulta
  get_distance <- function(img) {
    cat("processing ", img, "...\n")
    img <- readTIFF(img, as.is = TRUE, info = TRUE)
    descImg <- c(glcm(img)$glcm$ave)
    return(dist(t(data.frame(descImg, descQuery)), 
                method = "manhattan"))
  }
  # computa as distancias da imagem de consulta para todas as imagens
  distances <- unlist(lapply(path_imgs, get_distance))
  # recuperando as referencias das imagens
  names(distances) <- name_imgs
  # retornando o ranking (referencias das imagens ordenadas)
  return(names(sort(distances)))
}


#########
# Co-ocurrence Matrix - Haralick
#########
get_ranking_haralick <- function(path_imgs, name_imgs, query){
  d <- c()
  query <- glcm(readTIFF(path_imgs[query], as.is = T, info = T))
  descQuery <- haralick(query)[,"ave"]
  for(i in 1:length(path_imgs)){
    cat("processing", path_imgs[i], "...\n")
    img_glcm <- glcm(readTIFF(path_imgs[i], as.is = T, info = T))
    descImg <- haralick(img_glcm)[, "ave"]
    d[i] <- dist(t(data.frame(descImg, descQuery)), method = "manhattan")
  }
  names(d) <- name_imgs
  d <- sort(d)
  return(names(d))
}

#########
# LBP UNIFORME
#########

extract_lbp_image <- function(path_img){
  img <- load.image(path_img)
  cat("processing ", path_img, "...\n")
  r1 <- lbp(img[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
}

#########
# Momentos: Forma
#########

# calculando centroide
centroide <- function(M) {
  c(momento(M, 1, 0) / momento(M, 0, 0),
    momento(M, 0, 1) / momento(M, 0, 0))
}

# calculando momentos centrais
momento <- function(M, p, q, central = FALSE) {
  r <- 0
  if (central) {
    c <- centroide(M)
    x <- c[1]
    y <- c[2]
  } else {
    x <- 0
    y <- 0
  }
  for (i in 1:nrow(M))
    for (j in 1:ncol(M))
      r <- r + (i - x)^p * (j - y)^q * M[i,j]  
  return(r)
}

Momentos <-function(path_img){
  img <- load.image(path_img)
  img <- grayscale(img)[,,1,1]
  features <-NULL
  for(i in 0:2){
    for(j in 0:2){
      #adiciona um novo momento como caracteristica no vetor de caracteristicas da imagem
      features <- cbind(features,momento(img, i,j, central=TRUE))
    }
  }
  return(features)
}

get_ranking_by_shape_moment <- function(path_img, name_imgs, query){
  distL1_vect <- function(x, y){
    return (sum(abs(x-y)))
  }
  
  d <- c()
  descQuery <- Momentos(query)
  for(i in 1:length(path_img)){
    descImg <- Momentos(path_img[i])
    print(i)
    d[i] <- distL1_vect(descImg, descQuery)
  }
  names(d) <- name_imgs
  d <- sort(d)
  return(names(d))
}








###########
# AUX Functions IM package
###########
histeq <- function(I) {
  I = (I-min(I))/(max(I)-min(I))
  I = round(I*255);
  
  G =256;	  
  H =array(0:255,256);		
  T =array(0,256);
  
  H = apply(H,1, function(z){ sum(I==z) });
  
  for (i in 2:length(H)){
    H[i]= H[i-1]+H[i]	
  }
  
  T = H*(G-1)/length(I);
  
  for (i in 1:length(I)){
    I[i]=T[I[i]+1]
  }
  
  return(I)
}

rotate270 <- function(img) {
  im <- as.data.frame(t(img));
  im <- rev(im);
  im <- as.matrix(im);
  return(im)
}

displayImg <- function(img) {
  #if image is not grayscale, convert to grayscale
  if(length(dim(img))>2) {
    img = rowSums(img, dims=2)/3
  }
  if(length(dim(img))==2) {
    levels = seq(0,1,.0000001);
    g = gray(levels);
    #rotate image so that it appears aligned
    img = rotate270(img);
    #perform histogram equalization on displayed image
    img <- histeq(img);
    par(mfrow = c(1,1))
    image(img,col=g,axes=FALSE);
  } else {
    return("problem with image format")
  }
}


###########
# AUX Functions wvtool package
###########
glcm<- function(x, t.level=4, d=1) { # GLCM 
  if (length(dim(x))>2){warning("data must be grayscale image")
  } else{
    if(max(x)<=1){
      x <- x*(2^attr(x,"bits.per.sample")-1)
    }
    i.img <- x
    img.info <- attributes(x)
    s.level <- img.info$bits.per.sample
    tgl <- 2^t.level  # gray level of target image
    sgl <- 2^s.level # gray level of source image
    ini.img <- matrix(round(i.img*(tgl-1)/(sgl-1)), nrow(i.img),ncol(i.img)) # linear re-scaling
    #  translation in 4 directions, (0, 45, 90, 135 direction: vector length = d)
    trans <- c(d, 2*d,2*d, 0, 0, d, 0, 0)   
    trans <- matrix(trans,2,4)
    img.bank <- list()
    ref.img <- ini.img[(d+1):(nrow(ini.img)-d),(d+1):(ncol(ini.img)-d)]  # initial ROI that overlaps with translated image banks.
    o.glcm <- list()
    for (i in 1:ncol(trans)){ # image bank
      img.bank[[i]] <- ini.img[ (trans[1,i]+1):(trans[1,i]+nrow(ref.img)),(trans[2,i]+1):(trans[2,i]+ncol(ref.img))]
    }
    for (k in 1:ncol(trans)){
      ref1 <- factor(ref.img,levels=0:(tgl-1))  # i, j pair
      ref2 <- factor(img.bank[[k]],levels=0:(tgl-1))  # j, i pair
      t.glcm <- table(ref1,ref2) + t(table(ref1,ref2))
      m.glcm <- matrix(t.glcm,tgl,tgl)
      o.glcm[[k]] <- m.glcm/sum(m.glcm, na.rm=TRUE)
    }
    o.glcm[[5]]<- (o.glcm[[1]]+o.glcm[[2]]+o.glcm[[3]]+o.glcm[[4]])/4
    names(o.glcm) <- c("th_0", "th_45", "th_90", "th_135", "ave")
    return(list(glcm=o.glcm, level=t.level, d=d))
  }}

haralick <- function(x) {
  o.hara <- matrix(0, 15,5) #Haralick parameters output matrix
  for (th in 1:5) {
    pglcm<-x$glcm[[th]]
    nx <- ncol(pglcm)
    ny <- nrow(pglcm)
    px <- colSums(pglcm)
    py <- rowSums(pglcm)
    pxpy <-matrix(px,nx,ny)*t(matrix(py,nx,ny))
    px_y <- matrix (0, nx+ny)
    pxmy <- matrix (0, (nx+ny)/2)
    
    # means & standard deviation
    vx <- 1:nx
    vy <- 1:ny
    mx <- sum(px*vx) 
    my <- sum(py*vx)
    stdevx <- sum(px*(vx-mx)^2)
    stdevy <- sum(py*(vy-my)^2)
    
    # HX,HY,HXY for f12 and f13
    hxy1_0 <- matrix (0, nx,ny)
    hxy2_0 <- matrix (0, nx,ny)
    hxy1_0 <- pglcm*log10(pxpy)
    hxy2_0 <- (pxpy)*log10(pxpy)
    
    hx <- -sum(px*log10(px),na.rm=TRUE)
    hy <- -sum(py*log10(py),na.rm=TRUE)
    hxy1 <- -sum(hxy1_0,na.rm=TRUE)
    hxy2 <- -sum(hxy2_0,na.rm=TRUE)
    op <- matrix(1:nx,nx,ny)
    oq <- t(op)
    spq <- matrix(1:nx,nx,ny)+t(matrix(1:ny,nx,ny))
    dpq <- abs(matrix(1:nx,nx,ny)-t(matrix(1:ny,nx,ny)))
    
    #1 Angular Second Moment / Homogeniety "asm"
    o.hara[1,th] <- sum(pglcm^2)				
    #2 Contrast "con"
    o.hara[2,th] <- sum(dpq^2*pglcm)
    #3 inverse Difference Moment "idm"
    o.hara[3,th] <- sum(pglcm/(1+dpq^2))
    #4 Entropy "ent"
    o.hara[4,th] <- -sum(pglcm*log10(pglcm),na.rm=TRUE) 
    #5 Correlation 	"cor"
    o.hara[5,th] <- sum ((op-mx)*(oq-my)*pglcm/(sqrt(stdevx*stdevy)))
    #6 Variance in Haralick 1973	"var"
    o.hara[6,th] <- sum((op-((mx+my)/2))^2*pglcm)
    #7 Sum Average "sav"
    o.hara[7,th] <- sum(spq*pglcm) 
    #8 Sum Entropy "sen"
    #9 Difference Entropy "den"
    sen<- array(0,(2*nx))  # sen
    den.1 <- array(0,nx)  # den
    den.2 <- array(0,nx)  # den
    pglcm2 <- cbind(pglcm[,nx:1])  # a matrix with its column reverse order
    for (i in 2:nx) {
      sen[i]<-sum(diag(pglcm2[1:i,(nx-i+1):nx]))  # sen upper diagonal (include diagonal)
      den.1[i]<-sum(diag(pglcm[1:i,(nx-i+1):nx]))  # den lower diagonal (include diagonal)
      sen[1]<-pglcm2[1,nx]
      den.1[1]<-pglcm[1,nx]
    }
    for (i in 1:(nx-2)) {
      sen[i+nx]<-sum(diag(pglcm2[(i+1):nx,1:(nx-i)]))  # sen upper diagonal (include diagonal)
      den.2[nx-i]<-sum(diag(pglcm[(i+1):nx,1:(nx-i)]))  # den lower diagonal (include diagonal)
    }
    sen[nx+nx-1]<-pglcm2[nx,1]
    den.2[1]<-pglcm[nx,1]
    o.hara[8,th] <- -sum(sen*log10(sen),na.rm=TRUE)  # sen
    den <- den.1+den.2
    o.hara[9,th] <- -sum(den*log10(den),na.rm=TRUE)  # den
    #10 Difference Variance "dva"
    o.hara[10,th]<- sum(((dpq-o.hara[9])^2)*pglcm )
    #11 Sum Variance "sva"
    o.hara[11,th] <- sum(((spq-o.hara[8])^2)*pglcm)
    #12 Information Measures of Correlation "f12" (- sign was intentionally added as the value give minus value)
    o.hara[12,th] <- -(o.hara[4]-hxy1)/max(hx,hy)
    #13 Information Measures of Correlation "f13"
    o.hara[13,th] <- sqrt(1-exp(-2*abs(hxy2-o.hara[4] ))) 
    #14 Cluster Shade "sha"
    o.hara[14,th] <- sum((spq-mx-my)^3*pglcm)
    #15 Cluster prominence "pro"
    o.hara[15,th] <- sum((spq-mx-my)^4*pglcm)	#15 Cluster prominence
  }
  colnames(o.hara) <- c("th_0", "th_45", "th_90", "th_135", "ave")
  rownames(o.hara) <- c("asm","con","idm", "ent","cor","var","sav","sen","den","dva","sva","f12","f13", "sha","pro")
  rng <- apply(o.hara,1,max)-apply(o.hara,1,min)
  o.hara <- cbind(o.hara,rng)
  return(o.hara)
} #  (15 parameters calculated)

lbp <- function(x, r=1) {            # main for  LBP calculation
  img <- x
  if (r<=1){
    trans <- c(-1, 0, -1,1, 0,1,1,1,1,0,1,-1,0,-1,-1,-1) ; r <- 1
  } else {
    trans <- c(-2, 1, -1,2, 1,2,2,1,2,-1,1,-2,-1,-2,-2,-1) ; r <- 2
  }
  trans <-matrix(trans,2,8)
  x <- nrow(img)
  y <- ncol(img)
  o.img <- list()
  t.img <- list()
  o.img.1 <- img[(r+1):(x-r), (r+1):(y-r)]
  for ( i in 1:ncol(trans)) {
    dx <- trans[1,i]
    dy <- trans[2,i]
    o.img[[i]] <- img[(r+1+dx):(x-r+dx), (r+1+dy):(y-r+dy)] - o.img.1	
  }
  ulst.d <- unlist(o.img)
  ulst.d <- ifelse(ulst.d>=0, 1, 0)
  bin.data <- matrix(ulst.d,length(ulst.d)/8,8)
  o.img.f <- o.img.f2 <-matrix( apply(bin.data,1,bin2dec),x-2*r,y-2*r)
  
  bn <- matrix(dec2bin(0:255),ncol=8)
  tr <- apply(bn,1 ,lbnum)
  tr1 <- which(tr<=2)-1
  tr2 <- which(tr>2)-1
  
  for(i in 1:58){
    o.img.f2[o.img.f2==tr1[i]] <- i-1
  }
  o.img.fd <- data.frame(no=1:length(o.img.f),lbp=array(o.img.f))
  sub2 <- o.img.fd[o.img.fd$lbp %in% tr2, ]
  o.img.f2[sub2$no] <- 58
  dim(o.img.f2) <- c(nrow(o.img.f),ncol(o.img.f))
  
  return(list(lbp.u2 = o.img.f2, lbp.ori =o.img.f))
}

bin2dec <- function(x) {
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
}

dec2bin <- function(x, digit=8){
  ifelse(x <= 0 & digit <= 0, return(NULL), return(append(Recall(x%/%2,digit-1), x%%2)))
}

lbnum <- function(seq) {   # routine for LBP calculation
  seq.trans<- seq[2:length(seq)]
  p <- seq [1:length(seq)]
  q <- seq.trans[1:length(seq.trans)]
  q <- c(q,seq[1])
  r <- p - q
  sum( r != 0)
}

rot90c <- function(x){
  img.rot <- t(apply(x,2,rev))
  attr(img.rot, "bits.per.sample") <- attr(x,"bits.per.sample")
  attr(img.rot, "samples.per.pixel") <- attr(x, "samples.per.pixel")
  return(img.rot)
  
}