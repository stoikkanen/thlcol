#' Create THL colorpalette
#'
#' Create a color palette using the rules in https://yhteistyotilat.fi/wiki08/x/DgBJAQ
#' 
#' @param palette type of the palette
#' @param shadegroup base shade group, from "A" to "I"
#' @param altshadegroup base shade group, from "A" to "I" (used by palette "grey2")
#' @param darkness base darkness
#' @param n number of colors for palette "large"
#' @param m maximum number of colors. If m>n, pad with NA
#' @param alpha opacity (recycled)
#' @return a vector of RGB colors
#' @export
thlpal<-function(palette=c("grey1","grey2","shadegroup","large"),shadegroup="A",altshadegroup=NULL,darkness=c("K","V","T","L"),n=NULL,m=NULL,alpha=1) {
  palette<-match.arg(palette)
  darkness<-match.arg(darkness)
  shadegroup   <-match.arg(   shadegroup,LETTERS[1:9])
  if(palette=="grey1") {
    if(is.null(n)) n<-6
    alpha<-rep(alpha,length=n)
    res<-c(thlcol(shadegroup=shadegroup,darkness="K",alpha=alpha[1]),thlgrey(alpha=alpha[2:n]))
  }
  if(palette=="grey2") {
    if(is.null(n)) n<-7
    alpha<-rep(alpha,length=n)
    if(is.null(altshadegroup)) altshadegroup<-c("A"="E","B"="C","C"="B","D"="E","E"="A","F"="G","G"="F","H"="I","I"="H")[shadegroup]
    altshadegroup<-match.arg(altshadegroup,LETTERS[1:9])
    res<-c(thlcol(shadegroup=   shadegroup,darkness="K",alpha=alpha[1]),
           thlcol(shadegroup=altshadegroup,darkness="K",alpha=alpha[2]),
           thlgrey(alpha=alpha[3:n]))
  }
  if(palette=="shadegroup") {
    if(is.null(n)) n<-5
    alpha<-rep(alpha,length=5)
    res<-c(thlcol(shadegroup=shadegroup,darkness=c("V","K","T","L"),alpha=alpha[1:4]),thlgrey(2,alpha=alpha[5]))[1:n]
  }
  if(palette=="large") {
    if(is.null(n)) {
      warning("You should always give number of colors for palette 'large'")
      n<-27
    }
    alpha<-rep(alpha,length=n)
    sg<-LETTERS[1:9]
    sc<-which(sg==shadegroup) # where to start
    if(sc>1)
      sg<-c(sg[c(sc:9,1:(sc-1))])
    if(n%in%1:9) {
      dk<-rep(darkness,n)
    }
    if(n%in%10:18) {
      nn<-ceiling(n/2)
      if(darkness=="K") dk<-c(rep(c("K","V"),length=nn),rep(c("V","K"),length=nn))
      if(darkness=="V") dk<-c(rep(c("V","K"),length=nn),rep(c("K","V"),length=nn))
      if(darkness=="T") dk<-c(rep(c("T","K"),length=nn),rep(c("K","T"),length=nn))
      sg<-rep(sg[1:nn],2)
    }
    if(n>18) {
      nn<-ceiling(n/3)
      dk<-c(rep(c("K","T","V"),length=nn),rep(c("T","V","K"),length=nn),rep(c("V","K","T"),length=nn))
      sg<-rep(sg[1:nn],3)
    }
    res<-thlcol(shadegroup=sg,darkness=dk,alpha=alpha)[1:pmin(n,27)]
  }
  if(n>27) {
    warning("Only 27 colors available, recycling")
  } else {
    if(n>length(res)) {
      warning("Requested number of colors larger than palette default, recycling")
    }
  }
  res<-rep(res,length=n)
  if(!is.null(m)) res<-c(res,rep(NA,m-n))
  res
}
#' Vector of THL colors
#'
#' Create a vector of THL colors as defined in https://yhteistyotilat.fi/wiki08/x/DgBJAQ
#' The parameters are recycled
#'
#' @param shadegroup group of groups of shades to use, a letter from "A" to "I"
#' @param darkness darkness or darknesses to use, "V" light, "K" medium, "T" dark, "L" bright (use "L" sparingly) for thlcol, 1 to 5 for thlgrey
#' @param i number of the color (1-41)
#' @param alpha opacity of the color (recycled)
#' @return a vector of RGB colors
#' @export
thlcol<-function(shadegroup=NULL,darkness=NULL,alpha=1) {
  if(is.null(shadegroup)) {
    if(is.null(darkness)) {
      shadegroup<-rep(LETTERS[1:9],4)
      darkness  <-rep(c("V","K","T","L"),each=9)
    } else {
      shadegroup<-rep(LETTERS[1:9],length(darkness))
      darkness  <-rep(darkness,each=9)
    }
  } else {
    if(is.null(darkness)) {
      darkness<-rep("K",length(shadegroup))
    }
  }
  if(is.null(darkness  )) darkness<-c("V","K","T","L")
  colors<-thlcolbynum(1:27)
  addcol<-thlcolbynum(33:41)
  colmat<-cbind(matrix(colors,ncol=3,byrow=TRUE),addcol)
  dimnames(colmat)<-list(LETTERS[1:9],c("V","K","T","L"))
  n<-pmax(length(shadegroup),length(darkness),length(alpha))
  shadegroup<-rep(shadegroup,length=n)
  darkness<-rep(darkness,length=n)
  alpha<-rep(alpha,length=n)
  res<-rep(NA,n)
  for(i in 1:n) {
    tmp<-col2rgb(colmat[shadegroup[i],darkness[i]],alpha=TRUE)
    tmp["alpha",]<-round(alpha[i]*255)
    res[i]<-rgb(tmp[1],tmp[2],tmp[3],tmp[4],maxColorValue=255)
  }
  ##print(cbind(shadegroup,darkness,alpha,res))
  res
}
#' @describeIn thlcol Vector of THL greys
#' @export
thlgrey<-function(darkness=NULL,alpha=1) {
  greys <-thlcolbynum(28:32)
  if(is.null(darkness))
    darkness<-1:5
  darkness<-darkness[darkness%in%1:5]
  res<-greys[darkness]
  alpha<-rep(alpha,length=length(res))
  tmp<-col2rgb(res,alpha=TRUE)
  tmp["alpha",]<-round(alpha*255)
  rgb(tmp[1,],tmp[2,],tmp[3,],tmp[4,],maxColorValue=255)
}
#' @describeIn thlcol Vector of THL colors by number
#' @export
thlcolbynum<-function(i=1,alpha=1) {
  colors<-c("#7cd0d8","#25a5a2","#11414c","#84b266","#5faf2c","#244911","#d888a9","#bf4073","#7a2242",
            "#7699d6","#2f62ad","#0e1e47","#cc9966","#cc7219","#663d0c","#cc7acc","#993499","#571259",
            "#cccc72","#a59c2b","#595616","#9f7fcc","#6938af","#3b007f","#71cc96","#16994a","#06602b")
  greys <-c("#b2b2b2","#8c8c8c","#666666","#3f3f3f","#191919")
  addcol<-c("#00e5cf","#5ae500","#e50068","#0073e5","#e57700","#d900e5","#e5d400","#5e00e5","#00e55c")
  allcolors<-c(colors,greys,addcol)
  i<-i[i%in%(1:length(allcolors))]
  res<-col2rgb(allcolors[i],alpha=TRUE)
  res["alpha",]<-rep(alpha,length=length(i))*255
  rgb(res[1,],res[2,],res[3,],res[4,],maxColorValue=255)
}
#' Demo function
#'
#' Demonstrate the use of THL colors
#'
#' @param ... parameters passed to thlpal
#' @export
thlcolsample<-function() {
  par(mfcol=c(11,1),mar=c(0,0,2,0)+.1,oma=c(0,0,0,0)+.5)
  thlcolsamplebar("grey1")
  thlcolsamplebar("grey2",shadegroup="A")
  thlcolsamplebar("grey2",shadegroup="B")
  thlcolsamplebar("grey2",shadegroup="D")
  thlcolsamplebar("grey2",shadegroup="F")
  thlcolsamplebar("grey2",shadegroup="H")
  thlcolsamplebar("shade",shadegroup="A")
  thlcolsamplebar("large",n=9)
  thlcolsamplebar("large",n=18,shadegroup="D")
  thlcolsamplebar("large",n=21,shadegroup="D")
  thlcolsamplebar("large",n=27,shadegroup="D")
  invisible(NULL)
}
#' @describeIn thlcolsample Draw single bar with a palette
#' @export
thlcolsamplebar<-function(...) {
  ## string version of the call, adjusted
  call<-gsub("thlcolsamplebar","thlpal",deparse(sys.call()))
  ## baseplot
  plot(NA,NA,bty="n",xaxt="n",yaxt="n",ylab="",xlab="",ylim=c(0,1),xlim=c(0,27),main=call,adj=0,xaxs="i",yaxs="i")
  ## call palette. Use m=27 for comparability. Fails, if ... has m=.
  pal<-thlpal(...,m=27)
  ## all colors for reference
  tot<-thlcolbynum(1:41)
  ## match color labels. Omit alphas as they mean nothin' to me!
  lab<-match(substring(as.character(pal),1,7),substring(toupper(as.character(tot)),1,7))
  for(i in 1:27) {
    polygon(c(0,1,1,0,0)+i-1,c(0,0,1,1,0),col=pal[i],border=NA)
    text(i-.5,.5,adj=c(.5,.5),lab[i],col="white")
  }
  invisible(lab)
}

