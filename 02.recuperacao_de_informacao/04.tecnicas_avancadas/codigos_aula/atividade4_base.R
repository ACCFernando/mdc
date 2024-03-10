
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

#------------------------------------------------#
#                   COMBMIN                      #
#------------------------------------------------#

combmin <- function(...) {
  order(mapply(min, ...))
}

#------------------------------------------------#
#                   COMBMAX                      #
#------------------------------------------------#
combmax <- function(...) {
  order(mapply(max, ...))
}


#------------------------------------------------#
#                   COMBSUM                      #
#------------------------------------------------#

combsum <- function(...) {
  order(mapply(sum, ...))
}

#------------------------------------------------#
#                     BORDA                      #
#------------------------------------------------#

bordacount <- function(...) {
  # obtem os rankings
  rankings <- mapply(rank, list(...), SIMPLIFY = FALSE)
  # calcula a ordem baseada na soma das posições dos rankings
  return(do.call(combsum, rankings))
}

#------------------------------------------------#
#              MOMENTOS DE FORMA                 #
#------------------------------------------------#

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
  sub2 <- subset(o.img.fd,o.img.fd$lbp %in% tr2)
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
