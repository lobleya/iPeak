# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#------------------------------------------------------------
library("ggplot2")
library("htmlwidgets")
library("rbokeh")
library("modes")
library("MASS")
library("rtracklayer")
#------------------------------------------------------------
#globals
#------------------------------------------------------------
PEAKCOLOR<-c("gold2","royalblue","red2","forestgreen")
#------------------------------------------------------------
#' plotSinglePeak
#' @description plots a single peak as a smooth density
#' @export
#' @param x a number
#' @param xlim
#' @param ylim
#' @param col
#' @param outdir
#' @param single
#' @param dna
#' @param aa
#' @param mint
#' @param title
#' @example plotSinglePeak(peak,xlim,ylim,col,outdir,single,dna,aa,mint,title)
#' @return ggplot2::peakImage
#' plotSinglePeak(peak,
#'                xlim=c(50,100),
#'                ylim=c(0,50000),
#'                col="blue",
#'                outdir=".",
#'                single=T,
#'                mint=50,
#'                title=T)
plotSinglePeak <-function(peak,
                          xlim,
                          ylim,
                          col,
                          outdir,
                          single,
                          dna  =F,
                          aa   =F,
                          mint,
                          title=T)
{

  win <- c(min(xlim)-10,max(xlim)+10) #window size to plot peak over
  xx  <- peak$xx[which(peak$xx>min(win) & peak$xx<max(win))]
  yy  <- peak$yy[which(peak$xx>min(win) & peak$xx<max(win))]

  if(is.null(ylim)){ylim<-c(min(yy),max(yy))}

  ylim[2]<-ifelse(ylim[2]<1000,1000,ylim[2])

  origx <- peak$origx
  plist <- peak$peaks

  filter<- unlist(lapply(plist,
                        function(x){
                          return(x$xfrom>=min(xlim) & x$xto<=max(xlim))
                          }))

  plist <-plist[filter==T]
  peakplotlist<-list(NA)


  if(!single)
  {

    peakplot<-ggplot(data.frame(xx=xx,yy=yy),
                     aes(xx,yy))+
      geom_path( col  =col,
                 alpha=0.5,
                 size =1)+
      xlab("Position")+
      ylab("Intensity")+
      xlim(xlim)+ylim(ylim)+
      geom_hline(yintercept=mint,
                 col="red",alpha=0.6,
                 size=1, linetype=2)
  }
  # Check indexes here - xx and yy should be in window
  #    - may need xfrom and xto to be local
  #    - peaks should be single peak with posx (x index of apex)
  #             xfrom start x index xto - end x index
  #             apex and area

  pnames<-names(plist)
  if(is.null(pnames)){pnames=paste("peak",seq(1,length(plist),by=1),sep="")}

  for(i in 1:length(plist))
  {


    if(single)
    {
      xlim    <-c(plist[[i]]$xfrom-2,plist[[i]]$xto+2)
      peakplot<-ggplot(data.frame(xx=xx,yy=yy),
                       aes(xx,yy))+
        geom_path( col  =col,
                   alpha=0.5,
                   size =1)+
        xlab("Position")+
        ylab("Intensity")+
        xlim(xlim)+ylim(ylim)+
        geom_hline(yintercept=mint,
                   col="red",alpha=0.6,
                   size=1, linetype=2)
    }

    maxp    <- plist[[i]]$apex
    data    <- data.frame(x =c(0, plist[[i]]$xapex),
                          y =c(mint,plist[[i]]$apex))

    peakplot<- peakplot+geom_line( data    =data,
                                   aes(x,y),
                                   col     ="red",
                                   linetype=2,
                                   size=1)

    data    <- data.frame(x =c(plist[[i]]$xfrom,
                                plist[[i]]$xto,
                                plist[[i]]$xto,
                                plist[[i]]$xfrom),
                          y =c(0,0,maxp*1.2,maxp*1.2))

    peakplot<- peakplot+geom_polygon(data = data,
                            aes(x,y),
                            col =NA,
                            fill="gray",
                            alpha=0.2)


    if(single)
    {
      #finish plot and title here
      peakplot<-peakplot+theme(panel.background=element_rect(fill = "white"),
                               panel.border    =element_rect(fill = alpha("gray80",0.1)),
                               panel.grid.major=element_line(colour   =alpha("gray",0.3),
                                                             size     =0.3,
                                                             linetype =3),
                               panel.grid.minor=element_line(colour   =alpha("gray",0.5),
                                                             size     =0.3,
                                                             linetype =3))

      if(title)
      {
        peakplot<-peakplot+
          ggtitle(label<-paste("Peak window: ",
                               sprintf(plist[[i]]$xfrom,fmt="%.2f"),  " - ",
                               sprintf(plist[[i]]$xto,fmt="%.2f"),  " area ",
                               sprintf(plist[[i]]$area,fmt="%.2f"), sep=""))
      }

      fname<-paste(outdir,
                   paste(pnames[i],'.pdf',sep=""),
                   sep='/')

      ggsave(filename=fname,plot =peakplot, width=5, height=3.5)
      peakplotlist[[i]]<-peakplot
    }#if single


  }# end foreach peak in window

  if(!single)
  {
    # addin plot and title here
    peakplot<-peakplot+theme(panel.background=element_rect(fill <-"white"),
                             panel.border    =element_rect(fill <-alpha("gray80",0.1)),
                             panel.grid.major=element_line(color<-alpha("gray",0.3),
                                                            size <-0.3,
                                                            linetype<-3),
                             panel.grid.minor=element_line(color   <-alpha("gray",0.5),
                                                            size    <-0.3,
                                                            linetype<-3))
    # peakplots - add y axislabel


    if(title)
    {
      peakplot<-peakplot+
        ggtitle(label<-paste("Peak window: ",
                             xlim[1],  " - ",
                             xlim[2], " n = ",length(plist), sep=""))
    }
    #one plot per peak list
    fname<-paste(outdir,
                 paste('peaks',min(xlim),'-',max(xlim),'.pdf',sep=''),
                 sep='/')

    ggsave(filename=fname,plot=peakplot,width=5,height=3.5)

    peakplotlist[[1]]<-peakplot
    plot(peakplot)
    peakplot<-NA
  }#if multi single

  if(dna)
  {
    #write nt seq at bottom +-7

  }

  if(aa)
  {
    #write aa string at bottom +-7
  }

  return(peakplotlist)
}
#' readFiles
#' @export
#' @param files
#' @param fmt
#' @return list::list (list of peaks per file)
#' readFiles(files=my.files,
#'           fmt  ="fsa|bed|txt")
readFiles <- function(files,fmt="fsa")
{
  files=files[grep("bed|fsa|txt",files)]
  llength=3000
  ulength=80000
  data=list(NA)
  nfiles=length(files)
  for(i in 1:nfiles)
  {
    f=files[i]
    # format is bigWig or bed or fsa
    if(fmt=="bw"){
      f=as.data.frame(import.bw(f))
    }
    else if (fmt=="fsa"){
      f    <- seqinr::read.abif(f)
      lens <- lapply(f$Data, length)
      aaa  <- table(unlist(lens))
      nch  <- as.vector(aaa[which(as.numeric(names(aaa)) >
                                    3000 & as.numeric(names(aaa)) < 80000)])
      real.len <- as.numeric(names(aaa)[which(aaa == nch &
                                                as.numeric(names(aaa)) > llength & as.numeric(names(aaa)) < ulength)])
      channels <- as.vector(aaa[which(as.numeric(names(aaa)) > 1000)])

      prov <- NULL
      #names(all.inds.mats)[i] <- as.character(listp2[i])
      v    <- as.vector(which(unlist(lens) %in% real.len))
      reads <- list(NA)
      for (j in 1:channels) {
        reads[[j]] <- f$Data[[v[j]]]
      }
      f <- matrix(unlist(reads), ncol = channels)
      prov=NULL
      f<-apply(f,2,fourierT)
      f<-apply(f,2,saturate)
      f<-pullup(f, nch)
      rownames(f)=paste("index",seq(1,nrow(f)),sep="_")
      colnames(f)=paste("ch",seq(1,nch),sep="_")
      print(dim(f))

    }else{
      # assume text or bed file input
      f<-read.table(f,header=F,row.names=NULL,sep="\t",skip=1)
      colnames(f)<-c("chr","pos","score")

    }
    data[[i]]=f

  }
  names(data) <-basename(files)
  class(data) <- c("fsa_stored")
  return(data)

}
#' saturate
#' @param x
#' @return corrected x intensitites for saturation effect
#' @example saturate(x),
saturate<-function(x)
{

  y2  <- x
  st1 <- which(x > 8000)
  if (length(st1) > 1) {
    peak.sta <- c(1, which(diff(st1) > 12) + 1)
    peak.end <- c(which(diff(st1) > 12), length(st1))
    ini <- st1[peak.sta]
    end <- st1[peak.end]
    "%!in%" <- function(x, y) !(x %in% y)
    for (i in 1:length(ini)) {
      v1 <- ini[i]
      v2 <- end[i]
      v3 <- v1:v2
      v4 <- v3[bigPeaks(x[v3], tre = 7000)$pos]
      heis <- x[v4]
      if (length(v4) >= 2) {
        sort.heis <- sort(heis, decreasing = TRUE)[1:2]
        v4.1 <- sort(v4[which(heis %in% sort.heis)],
                     decreasing = FALSE)
        v5b <- (v4.1[1]:(v4.1[length(v4.1)]))
        v5 <- v5b[which(v5b %!in% v4.1)]
        for (j in 1:length(v5)) {
          v6 <- v5[j]
          left <- v4.1[1]
          right <- v4.1[2]
          a <- x[left] - x[v6]
          b <- x[right] - x[v6]
          y2[v6] <- x[v6] + (2 * ((abs(a) + abs(b))/2))
        }
      }
      else {
        y2 <- y2
      }
    }
  }
  else {
    y2 <- y2
  }
  return(y2)

}

#' assignPvals
#' @export
#' @param pvals sampled pvalues for different distributions
#' @param peaks peak list to assign pvalue to
#' @return peaks list with calculated pvalues
#' assignPvals(pvals,
#'             peaks)
assignPvals<-function(pvals,peaks)
{
  for(p in 1:length(peaks))
  {
    peaks[[p]]$pval<-calcP(pvals[[peaks[[p]]$uidx]]$sample,
                           peaks[[p]]$area)
  }
  return(peaks)
}

#' refinePeaks
#' @export
#' @param scores a set of peaks
#' @param minpc min overlap between peaks
#' @param thresh background threshold
#' @param xlim window of x for scoring peaks
#' @param ch channel
#' @return refined list of peaks with trimmed start-stop
#' refinePeaks(scores,
#'             minpc,
#'            thresh,
#'             xlim,
#'             ch)
refinePeaks<-function(scores, minpc,thresh,xlim,ch)
{
  npeaks<-list(NA)
  pvals <-list(NA)
  #scores is 1 set of peaks on a
  #--- write this another time ...
  #compare yy profile and max/min to determine starts and ends
  uidx=0
  for(p in 1:nrow(scores$peaks))
  {
    apex<-scores$peaks[p,1]
    peak<-findStarts(peak=list(NA),
			  origx=scores$origx,
                     xx=scores$xx,
                     yy=scores$yy,
                     apex=apex,
                     minpc=minpc,
                     thresh=thresh)


    peak<-findEnds( peak=peak,
                    origx=scores$origx,
                    xx=scores$xx,
                    yy=scores$yy,
                    apex=apex,
                    minpc=minpc,
                    thresh=thresh)
    tmp<-NULL
    tmp[[1]]<-list(len=(peak$tidx-peak$fidx)+1)
    uidx<-ifelse(is.na(match(tmp,
                             pvals)),
                 uidx+1,match(tmp,pvals))

    pvals[[uidx]]<-tmp[[1]]
    peak$uidx  <-uidx #lookup for pvalues
    peak$xlim  <-paste(min(xlim),"-",max(xlim),sep=" ")
    peak$name  <-paste(paste(xlim[1],xlim[2],sep="-"),
                       PEAKCOLOR[ch],p,sep=".")
    npeaks[[p]]<-peak

  }
  # use minima/antimodes to determine multimodal peaks and shift
  names(npeaks)<-rownames(scores$peaks)
  #--- shift is the value to separate peaks
  #--- min-d is the pcent height diff to consider a new peak
  npeaks<-filterOverlaps(npeaks)
  pvals<-samplePvals(pvals,scores$yy)
  npeaks<-assignPvals(pvals=pvals,peaks=npeaks)
  return(npeaks)
}

#' findPeaks
#' @description function for finding peaks along a vector of scores
#' @param scores input data from readFiles
#' @param thresh background threshold
#' @param xlim   window of x for peaks
#' @param minpc  min percent overlap
#' @param ch     channel index of peaks
#' @return list of peaks
#' findPeaks(scores,
#'           thresh,
#'           xlim=c(0,50000),
#'           minpc,
#'           ch)
findPeaks<-function(scores,thresh,xlim,minpc,ch)
{

  if(is.null(xlim)|| is.nan(xlim[1]) || is.nan(xlim[2])){
    xlim=c(0,max(scores$xx))
  }


  idx<-which(scores$xx > min(xlim) & scores$xx < max(xlim))
  x  <-cbind(scores$xx[idx],
             scores$yy[idx],
             scores$origx[idx])

  maxima.ind  <- intersect(which(diff(sign(diff(x[,2]))) == -2)+ 1 ,which( x[,2] > thresh))
  minima.ind  <- intersect(which(diff(sign(diff(x[,2]))) == 2 )+ 1, which( x[,2] > thresh))
  #clarify starts and ends of peaks


  peaks       <- as.matrix(x[maxima.ind, ])
  antimode    <- as.matrix(x[minima.ind, ])
  #print(x)


  scores$peaks    <-peaks
  scores$antimodes<-antimode
  print(scores$peaks[1:5,])
  scores$peaks    <-refinePeaks(scores,minpc,thresh,xlim,ch)
#  scores$peaks    <-clusterPeaks(scores$peaks) #add cluster number

  return(scores)
}
#' findStarts
#' @param peak a peak object
#' @param xx  a vector of positions
#' @param yy  a vector of scores
#' @param idx   index of peak
#' @param apex  peak inflection point
#' @param minpc minimum percent overlap of peak
#' @param thresh background threshold of peak
#' @return peak object with start index assigned
#' findStarts(peak,
#'            origx,
#'            xx,
#'            yy,
#'            apex,
#'            minpc,
#'            thresh)
findStarts<-function(peak,origx,
				xx,yy,apex,
					minpc,thresh)
{
  if(is.null(apex)   || is.nan(apex))  { print("Error: apex nothere !")}
  if(is.null(minpc)  || is.nan(minpc)) { minpc=0.5}
  if(is.null(thresh) || is.nan(thresh)){ thresh=20 }

  #loop for peaks on yy
  p   <-apex
  xf  <-0
  flag<-1
  cy  <-yy[p-1]
  apex<-yy[apex]
  ly  <-yy[p]

  while(flag)
  {
    cy<-yy[p]

    if(cy < thresh || p==1 || (cy - ly > 0 &  cy/apex < minpc ) )
    {
      flag<-0
    }

    ly<-yy[p]
    p <-p-1
  }


  peak$xfrom<-origx[p]
  peak$fidx <-p
  peak$yfrom<-yy[p]


  # print(peak)
  return(peak)
}
#' samplePvals
#' @description calculates sampling distribution of peaks
#' @export
#' @param pvals list of pvalues
#' @param yy    scores to sample
#' @return list of sampling distributions
#' samplePvals(pvals,
#'             yy)
samplePvals<-function(pvals,yy)
{

  for(p in 1:length(pvals))
  {

    re   <- ifelse(length(yy)-pvals[[p]]$len>10000,F,T)
    s    <- sample.int(length(yy)-pvals[[p]]$len,10000,replace=re)
    sl   <- unlist(sapply(s,function(x){return(sum(yy[x:(x+pvals[[p]]$len)]))}))
    sl   <- log1p(sl)

    pvals[[p]]$sample<-sl
  }

  #print("Finished")
  return(pvals)
}

#' findEnds
#' @description finds the end index for a peak
#' @param peak a peak object (list with midpoint, from and to)
#' @param xx original scale values where peak was called
#' @param yy original intensity values where peak was called
#' @param idx tracking number for peak
#' @param apex highest point of peak
#' @param minpc minimum percent overlap between peaks
#' @param thresh background threshold for peak
#' @return peak object with from to area and height
#' findEnds(peak,
#'          origx=origx,
#'          xx=my.x,
#'          yy=my.scores,
#'          apex=230,
#'          minpc=0.5,
#'          thresh=50)
findEnds<-function(peak,origx,xx,yy,
                   apex,minpc,
                   thresh)
{

  minpc<-ifelse(minpc > 1,0.8,minpc )
  area <-0
  width<-0
  xapex<-apex
  p    <-apex+1  #index on xx
  apex <-yy[apex-1]
  cy   <-yy[p]
  ly   <-yy[p-1]
  flag <-1

  while(flag)
  {
    cy<-yy[p]
    #     print(c(cy,ly,length(xx),minpc,apex))
    if(yy[p]<thresh || p>=length(xx) || ( cy-ly > 0 & cy/apex < minpc ))
    {
      flag<-0
    }

    ly<-yy[p]
    p <-p+1
  }

  #print(peak)
  subpeak    <-yy[peak$fidx:p]
  peak$xto   <-origx[p]
  peak$tidx  <-p
  peak$yto   <-cy
  peak$area  <-sum(yy[peak$fidx:peak$tidx])
  peak$width <-(peak$tidx-peak$fidx)+1
  peak$apex  <-max(yy[peak$fidx:peak$tidx])
  peak$modes <-nrow(amps(cbind(xx[peak$fidx:p],subpeak))$Peaks)
  peak$xapex <-origx[xapex]
  peak$midp  <-xapex
  peak$cluster <-1


  # print(peak)
  return(peak)
}
#' calcP
#' @description calculates significance of peak using area
#' @param ydist score sampling distribution
#' @param yarea observed score
#' @return empirically derived pvalue for peak area and length
#' calcP(ydist=rnorm(1000,50,25),
#'       yarea=5010.2)
calcP<-function(ydist,yarea)
{
  zp=1.00
  #-- empirical pvalue calculated
  #--  using function length of peak plus area

  zp   = 1-pnorm((abs(log1p(yarea)-mean(ydist))/sd(ydist)))

  return(sprintf("%.3e",zp))
}
#' filterOverlaps
#' @description merges overlapping peaks keeping biggest by area
#' @export
#' @param peaks original peak list
#' @return filtered peak list
#' filterOverlaps(peaks)
filterOverlaps<-function(peaks)
{
  ps<-unlist(lapply(peaks,function(p){return(p$fidx)}))
  pe<-unlist(lapply(peaks,function(p){return(p$tidx)}))
  w <-unlist(lapply(peaks,function(p){return(p$apex)}))
  p <-IRanges(start=ps,end=pe)
  #should be mutually exclusive
  hits<-findOverlaps(query     =p,
                     subject   =p,
                     minoverlap=5)

  hits<-as.data.frame(hits)

  uhits<-unique(hits[,1])
  pidx=NULL
  for(i in 1:length(uhits))
  {
    shits<-hits[hits[,1]==uhits[i],2]
    #take biggest
    pidx<-append(pidx,
                 shits[order(w[shits],decreasing=T)][1])
  }

  return(peaks[unique(pidx)])
}
#' fourierT
#' @description discrete fourier transform of scores/intensities
#' @param x vector of intensitites to be smoothed
#' @param top parameter controlling outliers
#' @return transform of data
#' fourierT(x=my.scores,
#'          top=0.3)
fourierT<-function(x,top=0.3)
{
  sn.fft <- fft(x)
  qq     <- length(x) * top
  sn.fft[qq:(length(x) - qq)] = 0 + (0+0i)
  sn.ifft <- fft(sn.fft, inverse = TRUE)/length(sn.fft)
  return(Re(sn.ifft))
}
#' bigPeaks
#' @description calls peaks on a vector using min threshold
#' @param x a vector of intensitities
#' @param tre a threshold for background
#' @return list of peak locations along x, pos and height
#' bigPeaks(x,
#'          tre=50)
bigPeaks<-function(x,tre)
{
  r <- rle(x)
  v <- which(rep(x = diff(sign(diff(c(-Inf, r$values, -Inf)))) ==
                   -2, times = r$lengths))
  pos <- v[which(x[v] > tre)]
  hei <- x[pos]
  out <- list(pos = pos, hei = hei)

  return(out)
}
#' scorePeaks
#' @description scores peaks along a data set
#' @export
#' @param data    an input set of raw data from readFiles
#' @param xlim    a window along x scale for calling peaks
#' @param channel channel ladder for microsatellites
#' @param chnum   index of score column to be used
#' @param dna     is the x scale bp T/F
#' @param aa      is the x scale aa T/F
#' @param maxhei  the max height of the scores
#' @param thresh  background threshold for peaks
#' @param minpc   minimum percent ovedrlap between peaks
#' @return list::list a list of cleaned peaks one per input file
#' scorePeaks(data,
#'            channel=my.ROX500,
#'            chnum=4, #red channel
#'            dna  =F,
#'            aa   =F,
#'            maxhei=5000,
#'            thresh=50,
#'            minpc =0.5,
#'            xlim  =c(50,500))
scorePeaks<-function(data,
                  channel=NULL,
                  ch=2,
                  dna=F,
                  aa=F,
                  maxhei,
                  thresh,
                  minpc=0.5,
                  xlim)
{
  # scoring function for chr from to dna / rna tracks
  library(modes)

  pnames <-gsub("\\..+$","",names(data))
  miny   <-thresh

  if(!is.null(channel))
  {
    #  These are fsa files and
    #  we need to fit the channel first
    list.data <-lapply(data,function(x){return(list(x[,ch]))})

    list.data <- lapply(list.data,
                        findLadder,
                        ladder = channel,
                        draw   = F,
                        dev    = dev,
                        warn   = warn,
                        method = "iter2",
                        init.thresh=50)

       print(summary(list.data[[1]][[1]]))
    #    print(summary(list.data[[1]][[3]]))
    list.models <- lapply(list.data,
                          function(da) {
                            y <- da[[3]]
                            x <- da[[1]]
                            mod <- lm(y ~ I(x) + I(x^2) + I(x^3) + I(x^4) + I(x^5),
                                      data = da)
                            return(mod)
                          })


    list.models.inv <- lapply(list.data, function(da) {
      x <- da[[3]]
      y <- da[[1]]
      mod <- lm(y ~ x, data = da)
      return(mod)
    })

    cols<-ch
    nx <- lapply(data, function(x, cols) {
      1:length(x[, cols])
    }, cols = cols)

    newxx <- numeric()
    newyy <- numeric()


    peaks <- list(NA)

    for (idx in 1:length(data))
    {
      newxx <- predict(list.models[[idx]],
                       newdata <- data.frame(x = nx[[idx]]))

      newyy <- ifelse(data[[idx]][, cols]<0,0,data[[idx]][,cols])

      peaks[[idx]] <- list(xx = newxx, yy = newyy, origx = nx[[idx]])

    }

  }else{
    #reformat to same as fsa
    peaks<-lapply(data,function(x){list(xx    = 1:nrow(x),
                                        yy    = x$score,
                                        origx = x$pos,
                                        chr   = x$chr)})
    names(peaks)<-names(data)
  }

  peaks <-lapply(peaks,
                 findPeaks,
                 thresh=thresh,
                 xlim  =xlim,
                 minpc =minpc,
                 ch)

  #merge close peaks and check for overlaps
  #peaks<-mergePeaks(peaks,shift)
  names(peaks)<-pnames
  #return result
  return(peaks)
}
#' amps
#' @export
#' @description calls peaks for the results set
#' @param x a matrix of x (positions) and y (scores)
#' @return a list of peaks
#' amps(x)
amps<-function(x)
{
  maxima.ind <- which(diff(sign(diff(x[,2]))) == -2) + 1
  minima.ind <- which(diff(sign(diff(x[,2]))) == 2) + 1
  colnames(x) <- c("Location (x)", "Amplitude (y)")
  peaks <- as.matrix(x[maxima.ind, ])
  antimode <- as.matrix(x[minima.ind, ])
  ls <- list(peaks, antimode)
  names(ls) <- c("Peaks", "Antimode")
  return(ls)

}
#' peakSummary
#' @description produces summary statistics for run
#' @export
#' @param peaks a set of peak results from scorePeaks
#' @return list of stats from a set of peaks
#' peakSummary(peaks)
peakSummary<-function(peaks)
{
  # number of peaks, where ave size/height/area/width
  # bias towards position by density
}
#' summaryImage
#' @description produces a hexbin image of peaks
#' @export
#' @param peaks
#' @param width
#' @param height
#' @param outdir
#' @return ggplot2::image
summaryImage<-function(results,width,height,outdir)
{
  library(ggplot2)
  library(rbokeh)

  title<-unique(results$Name)

  ff<-figure(xlab="position",
             ylab="intensity",
             width=600,
            height=500,
            title=paste("Peak Summary: ",title)) %>% ly_hexbin(results$Xapex,results$Apex)

  rbokeh2html(ff,paste(outdir,"PeakSummary",title,".html",sep=""))

  #hexbinplots
  return(ff)
}
#' writePeakTable
#' @description outputs a dataframe of peaks
#' @export
#' @param peaks a list of peaks from scorePeaks
#' @param fmt   choose from xls|txt file
#' @param outfile an output file for peaks
#' @return data.frame of peak info
writePeakTable<-function(peaks, fmt, outfile)
{
  pnames<-names(peaks)
  if(is.null(pnames))
  {
    pnames<-paste("peakfile_",seq(1,length(peaks)),sep="")
  }

  idx<-1
  library("xlsx")

  header=c("Index",
           "Name",
           "PeakName",
           "PeakIdx",
           "Apex",
           "Max.Hei",
           "Width",
           "Start",
           "End",
           "Area",
           "Window",
           "Modality",
           "Cluster",
           "Pval")

  dataf<-data.frame(row.names = header,
                    stringsAsFactors = F)

  # write peak table as xlsx or as tab sep
  for( i in 1:length(peaks) )
  {
    pidx<-length(peaks[[i]]$peaks)

    for( pi in 1:pidx )
    {
      p<-peaks[[i]]$peaks[[pi]]

      tabRow<-c(idx,
                pnames[i],
                p$name,
                pi,
                sprintf("%.3f",p$xapex),
                sprintf("%.3f",p$apex),
                p$width,
                sprintf("%.3f",p$xfrom),
                sprintf("%.3f",p$xto),
                sprintf("%.3f",p$area),
                p$xlim,
                p$modes,
                p$cluster,
                p$pval)

      dataf<-cbind(dataf,tabRow,
                   deparse.level=0)
      pidx<-pidx+1
      idx<-idx+1
    }

  }

  dataf          <-t(dataf)

  print(dim(dataf))
  if(fmt=="xls")
  {
       write.xlsx(dataf,
                    file     =outfile,
                    sheetName="peakList",
                    col.names=T,
                    row.names=F)
  }else{
         write.table(dataf,
                     file=outfile,
                     sep="\t",
                     col.names=T,
                     row.names=F,
                     qmethod="double")
  }
  #------endif ------------------------
  return(dataf)
}
#'peakSummaryMultiImg
#' @decription multiImg
#' @export
#' @param peaks  - a results list of peaks from peakSummary
#' @param width  - img width
#' @param height - img heiht
#' @param outdir - output directory
#' @return htmlWidget can be used with shiny/ggplot2
peakSummaryMultiImg<-function(results,width,height,outdir)
{

    library(ggplot2)
    library(rbokeh)

    multiImg<-NULL
    multiResults<-split(results,results$chr)

    for(i in 1:length(multiResults))
    {
     title<-names(results)

     ff<-figure(xlab="position",
                ylab="intensity",
                width=600,
                height=500,
                title=paste("Peak Summary: ",title)) %>% ly_hexbin(results$xapex,results$apex)
     multiImg[[i]]<-ff

    }

    multiImg<-marrangeGrob(multiImg,nrow=,ncol=)

    rbokeh2html(multiImg,paste(outdir,"PeakSummary",title,".html",sep=""))

    #hexbinplotsPanel
    return(multiImg)

}
#'
#' peakHTMLTable
#' @decription HTML table of peaks
#' @export
#' @param peaks a results list of peaks from scorePeaks
#' @param results a data.frame of results from writePeakTable or peakResults
#' @return htmlWidget can be used with shiny/ggplot2
peakHTMLTable<-function(results)
{
  library(DT)

  peakImg<-apply(rownames(results),function(n){paste(n,".pdf",sep="")})

  for(i in 1:nrow(results))
  {
    results$img[i]<-paste('<img src="',
                           peakImg[i],
                          '",width=10,height=5></img>',sep="")
  }

  oname<-names(results)

  htmlwidgets::saveWidget(DT::datatable(results),file = paste(outdir,"/",oname,"table.html",sep=""))

  return(DT::datatable(results))
  #html functions
}
#' writeHTMLReport
#' @decription writes out HTML report from peaks
#' @export
#' @param peaks a list of peaks
#' @param peakResults a data.frame of peak results
#' @param peakImgList a summary hexbin plot from summaryImage
#' @param htmlTable a peak results table
#' @param fmt - shiny|html choose output shiny dashboard or html page
#' @param outdir  output directory for report
#' @return a report object
writeHTMLReport<-function(peaks,
                          peakResults,
                          peakImgList,
                          fmt="shiny",
                          outdir=".")
{
  #put together shiny dashboard/report
  htmlOut<-NULL

  if(fmt=="shiny")
  {
    library(shinydashboard)
    # one tab per input file
    dh<-dashboardHeader(title = "iPeaks Report",
                        messages,
                        notifications,
                        tasks)

    for(i in 1:length(peaks))
    {


     db<-dashboardBody(
                      # three parts; summary text, HexImage and Table
                      tabSetPanel(
                      sidebarUserPanel(
                        name="summary information",
                        textOutput("summary")
                      ),

                      box(
                          title = "peak density plot",
                          status="primary",
                          rbokehOutput("hexplot"),
                          width =800,
                          height=600
                      ),

                      box(title ="data table",
                          status="primary",
                          dataTableOutput("data")
                          )
                        #-- make interactive with peak click highlight in hex image

                      )

     )

    dp<-dashboardPage(header   = dh,
                        body   = db,
                        skin   = "blue")
    }

    srv<-function(input,output){

    #i comes from selection menu on mid panel

    output$text   <-renderText({
      #panel has selection box for info/chromosome
         peakText[[i]]
    })

    output$hexplot<-renderRbokeh({
      #peak image needs to be interactive with table
      #peakplotlist[[i]]
    })

    output$data   <-renderDataTable(
        #peakTable[[i]],
        options=list(pageLength=3)
    )

    }

    app<-shinyApp(dp,srv)
    app

  }else{

      #summaryTxt  <-peakSummary(peaks)
      #summaryImg  <-summaryImage(peaks)
      #summaryTable<-DataTable(peaks)

    #saveWidget(file = paste(outdir,".html",sep=""))
  }

  return(htmlOut)
}
#' pullup
#' @description denoising function for scores/itensities
#' @param mat a data.frame of peak results
#' @param channel the index of intensitites
#' @return a de-noised intensity vector
pullup<-function (mat, channel = 4)
{
  all.cols      <- apply(mat[, -channel], 1, sum)
  rows          <- dim(all.cols)[1]
  peak.all.cols <- bigPeaks(all.cols, tre = 300)
  vv1           <- which(diff(peak.all.cols$pos) > 100)
  vv1           <- c(vv1, length(peak.all.cols$pos))
  vv2           <- vv1 + 1
  vv2           <- vv2[-length(vv2)]
  vv2           <- c(1, vv2)
  regions       <- apply(data.frame(cbind(vv2, vv1)), 1, function(x,
                                                                  pos) {
    y <- pos[x[1]:x[2]]

    return(y)
  }, peak.all.cols$pos)

  for (i in 1:length(regions)) {
    maxis <- apply(mat[, -channel], 2, function(x, regi) {
      max(x[regi])
    }, regi = regions[[i]])
    ch <- c(1:length(maxis))
    wiii <- which(maxis == max(maxis))
    rest <- ch[-wiii]
    decrease <- regions[[i]][1]:(regions[[i]][length(regions[[i]])])
    mat[decrease, rest] <- mat[decrease, rest] - (mat[decrease,
                                                      wiii] * 0.3)
  }
  all.inds.mats2 <- apply(mat, 2, function(x) {
    x[which(x < 0)] <- x[which(x < 0)]
    return(x)
  })
  return(all.inds.mats2)
}

#' filterPeaks
#' @description function to filter peaks based on overlap
#' @export
#' @param peaks a data.frame of peak results
#' @param xvals original x positions
#' @param yvals original scores
#' @param minpc minimum overlap between peaks
#' @return list::list peak list per input file object
filterPeaks<-function(peaks,xvals,yvals,minpc)
{

  for(i in 1:length(peaks))
  {
    yfrom <- yvals[peaks[i]$pos-1]
    yto   <- yvals[peaks[i]$pos+1]
    idx   <- peaks[i]$pos

    ydiff <- -10
    while(yvals[idx] > 0 & ( ydiff < 0 | yvals[idx]/apex < minpc  ))
    {
      ydiff <- yvals[idx+1]-yvals[idx]
      idx   <- idx -1
    }
    peaks[i]$xfrom   <-idx
    peaks[i]$yfrom   <-yvals[idx]

    idx     <- peaks[i]$pos
    ydiff   <- -10

    while(yvals[idx]>0 & ( ydiff >0 |  yvals[idx]/apex < minpc))
    {
      ydiff <- yvals[idx]-yvals[idx+1]
      idx   <- idx + 1
    }

    peaks[i]$area <- sum(yvals[peaks[i]$xfrom:idx])
    peaks[i]$modes<- length(findPeaks(xvals[peaks[i]$xfrom:idx]))  #number peaks in window
    peaks[i]$xto  <- idx
    peaks[i]$yto  <- yvals[idx]
  } #for

  return(peaks)
}
#' mergePeaks
#' @description merges peaks based on spacing and overlap
#' @export
#' @param peaks a data.frame of peak results
#' @param shift minimum shift on x between peaks
#' @param minpc minimum percent overlap between peaks
#' @return a peak list object
mergePeaks<-function(peaks,shift,minpc)
{

  newpeaks<-list()

  for( i in 1:length(peaks) )
  {
    # check shift width
    if(abs(peaks[i+1]$xapex-peaks[i]$xapex) < shift)
    {
      peaks[i]$start <- min(peaks[i]$start,peaks[i+1]$start)
      peaks[i]$end   <- max(peaks[i]$end,  peaks[i+1]$end)
      peaks[i]$apex  <- max(peaks[i]$apex, peaks[i+1]$apex)
      idx            <- length(newpeaks)
      newpeaks[idx]  <- peaks[i]

      i <- i + 1 # skip next one
    }


  }

  peaks <- newpeaks
  delete(newpeaks)

  return(peaks)
}

#' clusterPeaks
#' @description clusters peaks based on minimising within to between sums squares
#' @export
#' @param peaks a list of peaks
#' @return a peak list with cluster numbers assigned
clusterPeaks<-function(peaks)
{
  #compare peaks by euclidean distance over height and width
  pstats<-t(matrix(
    unlist(lapply(peaks,
           function(p){
             return(as.numeric(c(p$width,p$xapex)))
           })),
    ncol<-length(pstats),
    nrow<-2))


  pdist  <- dist(pstats)
  pclust <- hclust(pdist,method<-"ward.D2")
  pdist  <- as.matrix(pdist)
  #find maximum wss statistic and cutree
  library("NbClust")
  min.nc<-min(15,nrow(pstats)-2)
  res <- NbClust(pstats,
                 distance = "euclidean",
                 min.nc   = 2,
                 max.nc   = min.nc,
                 method = "ward.D2",
                 index = "ratkowsky")

  pnum<-res$Best.nc[1]

  print("Splitting into N clusters: ",pnum)

  pgroups<-cutree(pclust,k=pnum)

  print("Group sizes are:  ",table(pgroups))

  #annotate peak list with cluster number
  for(p in 1:length(peaks)){p[[i]]$cluster<-pgroups[i]}

  return(peaks)
}
#' separate
#' @description separates peaks according to shift space
#' @param g a list of peaks
#' @param shift minimum shift on x between peaks
#' @param type [if x is dna use base-pair scale for shift]
#' @return a peak list object where multi-modal peaks are separated
separate<-function (g, shift = 1, type = "bp")
{
  if (type == "bp") {
    vv <- which(diff(g$wei) < shift)
  }
  else {
    vv <- which(diff(g$pos) < shift)
  }
  vv2 <- vv + 1
  while (length(vv) > 0) {
    keep <- numeric()
    for (h in 1:length(vv)) {
      a1 <- vv[h]
      a2 <- vv2[h]
      a3 <- c(g$hei[a1], g$hei[a2])
      a4 <- c(a1, a2)
      keep[h] <- (a4[which(a3 == max(a3))])[1]
    }
    keep <- unique(keep)
    "%!in%" <- function(x, y) !(x %in% y)
    keep2 <- unique(c(vv, vv2)[which(c(vv, vv2) %!in% keep)])
    if (type == "bp") {
      g <- list(pos = g$pos[-keep2], hei = g$hei[-keep2],
                wei = g$wei[-keep2])
    }
    else {
      g <- list(pos = g$pos[-keep2], hei = g$hei[-keep2])
    }
    if (type == "bp") {
      vv <- which(diff(g$wei) < shift)
    }
    else {
      vv <- which(diff(g$pos) < shift)
    }
    vv2 <- vv + 1
  }
  return(g)
}
#' findLadder
#' @description findLadder assigns dna bp scale to an arbitrary x scale
#' @param x
#' @param ladder
#' @param draw
#' @param close=50
#' @param dev=50
#' @param warn=T
#' @param init.thresh=50
#' @param sep.index 8
#' @param method iter or iter2
#' @param reducing=NULL
#' @param who="sample"
#' @param attempt=10
#' @return ladder fit to x scale
findLadder<-function (x, ladder, draw = TRUE, close=50,
                      dev = 50, warn = TRUE, init.thresh = NULL,
                      sep.index = 8, method = NULL,
                      reducing = NULL, who = "sample",
                      attempt = 10)
{
  x<-unlist(x)
  hohoho <- bigPeaks(x[1:length(x)], 100)
  if (is.null(method)) {
    tototo <- bigPeaks(x[1:length(x)], median(hohoho$hei) * 2)
    close <- (length(x) - tototo$pos[length(tototo$hei)])
    if (close < 100) {
      method = "iter"
    }
    else {
      method = "iter2"
    }
  }
  if (is.null(init.thresh)) {
    if (mean(x) > 1000) {
      init.thresh <- quantile(hohoho$hei, 0.95)
    }
    else {
      init.thresh <- median(hohoho$hei)/2.2
    }
  }
  MSE <- function(x, y) {
    X <- cbind(1, x)
    qr.X <- qr(X)
    b <- t(qr.Q(qr.X)) %*% y
    R <- qr.R(qr.X)
    beta <- as.vector(backsolve(R, b))
    fit <- X %*% beta
    res <- list(mse = sum((y - fit)^2), beta = beta[2])
    return(res)
  }
  MSE2 <- function(x, y) {
    X <- cbind(1, x)
    qr.X <- qr(X)
    b <- t(qr.Q(qr.X)) %*% y
    R <- qr.R(qr.X)
    beta <- as.vector(backsolve(R, b))
    fit <- X %*% beta
    mse <- sum((y - fit)^2)
    sst <- sum((y - mean(y))^2)
    r2 <- 1 - (mse/sst)
    res <- list(mse = mse, beta = beta, r2 = r2)
    return(res)
  }
  MSE3 <- function(mix, miy) {
    X <- cbind(1, mix)
    qr.X <- qr(X)
    b <- t(qr.Q(qr.X)) %*% miy
    R <- qr.R(qr.X)
    beta <- as.vector(backsolve(R, b))
    fit <- X %*% beta
    mse <- sum((miy - fit)^2)
    sst <- sum((miy - mean(miy))^2)
    r2 <- 1 - (mse/sst)
    res <- list(mse = mse, beta = beta, r2 = r2)
    return(res)
  }
  thresh = init.thresh
  roxy <- bigPeaks(x[1:length(x)], thresh)
  if (!is.null(reducing)) {
    nono <- which(roxy$pos %in% reducing)
    roxy <- lapply(roxy, function(x) {
      x[nono]
    })
  }
  nnn <- length(roxy$pos)
  fdsa = 1
  while (nnn < length(ladder)) {
    if (fdsa == 1) {
      cat("\nReducing threshold 2x to find ladder \n")
    }
    thresh = thresh/2
    roxy <- big.peaks.col(x[1:length(x)], thresh)
    nnn <- length(roxy$pos)
    fdsa <- fdsa + 1
  }
  whot <- length(roxy$pos) * 0.2
  what <- which(roxy$hei == max(roxy$hei))
  roxy <- separate(roxy, shift = sep.index, type = "pos")
  if (method == "iter") {
    ii <- which(roxy$hei == max(roxy$hei)) + 1
    iii <- length(roxy$hei)
    roxy <- list(pos = roxy$pos[ii:iii], hei = roxy$hei[ii:iii])
    step1 <- combn(roxy$pos[1:attempt], 3)
    step2 <- apply(step1/10, 2, MSE, y = ladder[1:3])
    mse <- unlist(lapply(step2, function(x) {
      x$mse
    }))
    covs <- apply(step1, 2, function(x, y) {
      cov(x, y)
    }, y = ladder[1:3])
    step2 <- mse * covs
    step3 <- step1[, which(step2 < sort(step2, decreasing = FALSE)[20])]
    step4 <- apply(step3, 2, function(x, y) {
      which(y %in% x)
    }, y = roxy$pos)
    caller <- function(roxy, www, ladder.call, x) {
      threshold <- length(x)
      posi <- numeric()
      fact2 <- length(ladder.call)
      expect <- roxy$pos[www]
      xxx <- ladder.call[c(1:3)]
      modx <- lm(expect ~ poly(xxx, degree = 1))
      expecto <- predict(modx, data.frame(xxx = ladder.call))
      ladder.call <- ladder.call[which(expecto < threshold *
                                         0.85)]
      available <- length(roxy$pos) - length(ladder.call)
      ava2 <- length(ladder.call) - abs(available)
      if (available > 0) {
        if ((length(ladder.call) - 1) < 3) {
          tope <- length(ladder.call)
        }
        else {
          tope <- length(ladder.call) - 1
        }
      }
      else {
        tope <- ava2 - 2
      }
      expect <- rep(NA, tope + 1)
      for (i in 3:tope) {
        if (i == 3 & i != tope) {
          expect[1:3] <- roxy$pos[www]
          xxx <- ladder.call[c(1:3)]
          mod <- MSE2(xxx, expect[1:3])
          beta <- (mod)$beta
          expecto <- as.vector(beta[1] + matrix(ladder.call) %*%
                                 beta[-1])
          act <- roxy$pos[-which(roxy$pos %in% expect)]
          yoyo <- abs(expecto[i + 1] - act)
          good <- which(yoyo == min(yoyo, na.rm = TRUE))
          expect[i + 1] <- act[good]
          if (mod$r2 < 0.9) {
            i = tope
          }
        }
        if (i > 3 & i <= 5) {
          xx <- ladder.call[c(1:i)]
          mod <- MSE2(xx, expect[1:i])
          beta <- (mod)$beta
          expecto <- as.vector(beta[1] + matrix(ladder.call) %*%
                                 beta[-1])
          act <- roxy$pos[-which(roxy$pos %in% expect)]
          yoyo <- abs(expecto[i + 1] - act)
          good <- which(yoyo == min(yoyo, na.rm = TRUE))
          expect[i + 1] <- act[good]
          if (mod$r2 < 0.9) {
            i = tope
          }
        }
        if (i > 5) {
          xx <- cbind(ladder.call[c(1:i)], ladder.call[c(1:i)]^2,
                      ladder.call[c(1:i)]^3, ladder.call[c(1:i)]^4)
          mod <- MSE2(xx, expect[1:i])
          beta <- (mod)$beta
          if (length(which(is.na(beta))) > 0) {
            beta[which(is.na(beta))] <- 0
          }
          toto <- cbind(matrix(ladder.call), matrix(ladder.call)^2,
                        matrix(ladder.call)^3, matrix(ladder.call)^4)
          expecto <- cbind(rep(1, dim(toto)[1]), toto) %*%
            beta
          act <- roxy$pos[-which(roxy$pos %in% expect)]
          yoyo <- abs(expecto[i + 1] - act)
          good <- which(yoyo == min(yoyo, na.rm = TRUE))
          expect[i + 1] <- act[good]
          if (is.na(mod$r2)) {
            mod$r2 <- 0.1
          }
          if (mod$r2 < 0.9) {
            i = tope
          }
        }
        if (i == tope & i != 3) {
          if (i < 5) {
            expect[1:3] <- roxy$pos[www]
            xx <- ladder.call[c(1:i)]
          }
          else {
            xx <- cbind(ladder.call[c(1:i)], ladder.call[c(1:i)]^2,
                        ladder.call[c(1:i)]^3, ladder.call[c(1:i)]^4)
          }
          mod <- MSE2(xx, expect[1:i])
          beta <- (mod)$beta
          if (length(which(is.na(beta))) > 0) {
            beta[which(is.na(beta))] <- 0
          }
          if (i < 5) {
            toto <- cbind(matrix(ladder.call))
          }
          else {
            toto <- cbind(matrix(ladder.call), matrix(ladder.call)^2,
                          matrix(ladder.call)^3, matrix(ladder.call)^4)
          }
          expecto <- cbind(rep(1, dim(toto)[1]), toto) %*%
            beta
          act <- roxy$pos[-which(roxy$pos %in% expect)]
          yoyo <- abs(expecto[i + 1] - act)
          good <- which(yoyo == min(yoyo, na.rm = TRUE))
          expect[i + 1] <- act[good]
        }
        if (i == tope & i == 3) {
          expect[1:3] <- roxy$pos[www]
        }
      }
      posi <- expect
      tutu <- abs(length(x) - posi)
      posi <- posi[1:which(tutu == min(tutu, na.rm = TRUE))]
      heii <- roxy$hei[which(roxy$pos %in% posi)]
      fact3 <- length(posi)/fact2
      if (length((posi)) < 6) {
        fact <- summary(lm(ladder.call[1:length(posi)] ~ poly(posi, degree = length((posi)) - 1)))$r.squared * fact3
      }
      else {
        fact <- summary(lm(ladder.call[1:length(posi)] ~ poly(posi, degree = 5)))$r.squared * fact3
      }

      roxy2 <- list(pos = posi,
			  hei = heii,
			  wei = ladder.call[1:length(posi)],
                    corr = abs(cor(ladder.call[1:length(posi)], posi)),
                    error = fact)
      return(roxy2)
    }
    rt <- apply(data.frame(step4), 2, FUN = caller, roxy = roxy,
                ladder.call = ladder, x = x)
    corrs3 <- unlist(lapply(rt, function(x) {x$error}))
    roxy3 <- rt[[which(corrs3 == max(corrs3))]]
    if (draw == TRUE) {
      limi <- sort(roxy3$hei, decreasing = TRUE)
      plot(x, type = "l", xaxt = "n", ylim = c(0, (limi[3] +
                                                     1000)), cex.axis = 0.6, las = 2, xlim = c((min(roxy3$pos) -
                                                                                                  100), (max(roxy3$pos) + 100)), col = transp("grey35",
                                                                                                                                              0.7), ylab = "RFU", xlab = "", lwd = 2, main = attributes(x)$mycomm,
           cex.main = cex.title)
      axis(1, at = roxy3$pos, labels = roxy3$wei, cex.axis = 0.6)
      points(x = roxy3$pos, y = roxy3$hei, cex = 1.1, col = transp("black",
                                                                   0.85))
      points(x = roxy3$pos, y = roxy3$hei, pch = 20, col = transp("red",
                                                                  0.7))
      legend("topleft", legend = paste("Correlation:",
                                       round(roxy3$corr, digits = 4), sep = ""), bty = "n")
      legend("topright", legend = c("Peaks selected"),
             col = c("red"), bty = "n", pch = c(20), cex = 0.85)
    }
    roxy <- roxy3
  }
  if (method == "iter2") {
    last <- length(roxy$pos)
    lld <- length(ladder)
    if ((last - attempt) < 0) {
      roxy$wei <- ladder[1:length(roxy$pos)]
      roxy$corr <- 0
      roxy$error <- 0
      if (draw == TRUE) {
        limi <- sort(roxy$hei, decreasing = TRUE)
        plot(x, type = "l", xaxt = "n", ylim = c(0, (limi[3] +
                                                       1000)), cex.axis = 0.6, las = 2, col = transp("grey35",
                                                                                                     0.7), ylab = "RFU", xlab = "", lwd = 2, main = attributes(x)$mycomm,
             cex.main = cex.title)
        axis(1, at = roxy$pos, labels = roxy$wei, cex.axis = 0.6)
        points(x = roxy$pos, y = roxy$hei, cex = 1.1,
               col = transp("black", 0.85))
        points(x = roxy$pos, y = roxy$hei, pch = 20,
               col = transp("red", 0.7))
        legend("topleft", legend = paste("Correlation:",
                                         round(roxy$corr, digits = 4), sep = ""), bty = "n")
        legend("topright", legend = c("Peaks selected"),
               col = c("red"), bty = "n", pch = c(20), cex = 0.85)
        legend("center", legend = c("Intensity too low to be detected"),
               col = c("red"), bty = "n", cex = 0.85)
      }
    }
    else {
      step1 <- combn(roxy$pos[last:(last - attempt)], 3)
      step2 <- apply(step1/10, 2, MSE, y = ladder[lld:(lld -
                                                         2)])
      mse <- unlist(lapply(step2, function(x) {
        x$mse
      }))
      covs <- apply(step1, 2, function(x, y) {
        cov(x, y)
      }, y = ladder[lld:(lld - 2)])
      step2 <- mse
      step3 <- step1[, which(step2 < sort(step2, decreasing = FALSE)[20])]
      step4 <- apply(step3, 2, function(x, y) {
        sort(which(y %in% x), decreasing = TRUE)
      }, y = roxy$pos)
      caller <- function(roxy, www, ladder.call, x) {
        threshold <- length(x)
        last3 <- length(ladder.call):(length(ladder.call) -
                                        2)
        posi <- numeric()
        fact2 <- length(ladder.call)
        expect <- roxy$pos[www]
        xxx <- ladder.call[last3]
        modx <- lm(expect ~ poly(xxx, degree = 1))
        expecto <- predict(modx, data.frame(xxx = ladder.call))
        available <- length(roxy$pos) - length(ladder.call)
        ava2 <- length(ladder.call) - abs(available)
        if (available < 0) {
          tope <- 3
        }
        else {
          tope <- length(ladder.call)
        }
        expect <- rep(NA, tope)
        lenlad <- length(ladder.call)
        for (i in 3:(tope - 1)) {
          if (i == 3 & i != tope) {
            expect[tope:(tope - 2)] <- roxy$pos[www]
            xxx <- ladder.call[last3]
            mod <- MSE3(mix = xxx, miy = expect[tope:(tope -
                                                        2)])
            beta <- (mod)$beta
            expecto <- as.vector(beta[1] + matrix(sort(ladder.call,
                                                       decreasing = TRUE)) %*% beta[-1])
            condo <- sort(expect[which(!is.na(expect))],
                          decreasing = TRUE)
            act <- sort(roxy$pos[-which(roxy$pos %in%
                                          condo)], decreasing = TRUE)
            yoyo <- abs(expecto[i + 1] - act)
            good <- which(yoyo == min(yoyo, na.rm = TRUE))
            qwer <- i
            qwer2 <- length(expect) - qwer
            expect[qwer2] <- act[good]
            if (mod$r2 < 0.9) {
              i = tope
            }
          }
          else if (i > 3 & i <= 5) {
            xx <- ladder.call[c(lenlad:(lenlad - (i -
                                                    1)))]
            mod <- MSE3(mix = xx, miy = expect[tope:qwer2])
            beta <- (mod)$beta
            expecto <- as.vector(beta[1] + matrix(sort(ladder.call,
                                                       decreasing = TRUE)) %*% beta[-1])
            condo <- sort(expect[which(!is.na(expect))],
                          decreasing = TRUE)
            act <- sort(roxy$pos[-which(roxy$pos %in%
                                          condo)], decreasing = TRUE)
            yoyo <- abs(expecto[i + 1] - act)
            good <- which(yoyo == min(yoyo, na.rm = TRUE))
            qwer <- i
            qwer2 <- length(expect) - qwer
            expect[qwer2] <- act[good]
            if (mod$r2 < 0.9) {
              i = tope
            }
          }
          else if (i > 5) {
            ladder.call2 <- sort(ladder.call, decreasing = TRUE)
            expect2 <- sort(expect, decreasing = TRUE)
            xx <- cbind(ladder.call2[c(1:i)])
            mod <- MSE3(mix = xx, miy = expect2)
            beta <- (mod)$beta
            if (length(which(is.na(beta))) > 0) {
              beta[which(is.na(beta))] <- 0
            }
            toto <- cbind(matrix(ladder.call2))
            expecto <- cbind(rep(1, dim(toto)[1]), toto) %*%
              beta
            condo <- sort(expect[which(!is.na(expect))],
                          decreasing = TRUE)
            act <- sort(roxy$pos[-which(roxy$pos %in%
                                          condo)], decreasing = TRUE)
            yoyo <- abs(expecto[i + 1] - act)
            good <- which(yoyo == min(yoyo, na.rm = TRUE))
            qwer <- i
            qwer2 <- length(expect) - qwer
            expect[qwer2] <- act[good]
            if (is.na(mod$r2)) {
              mod$r2 <- 0.1
            }
            if (mod$r2 < 0.9) {
              i = tope
            }
          }
          else if (i == tope & i == 3) {
            expect[1:3] <- roxy$pos[www]
          }
        }
        posi <- expect
        heii <- roxy$hei[which(roxy$pos %in% posi)]
        fact3 <- length(posi)/fact2
        if (length((posi)) < 6) {
          fact <- summary(lm(ladder.call[1:length(posi)],
                               poly(posi, degree = length((posi)) - 1)))$r.squared *
            fact3
        }
        else {
          fact <- summary(lm(ladder.call[1:length(posi)] ~
                               poly(posi, degree = 5)))$r.squared * fact3
        }
        roxy2 <- list(pos = posi, hei = heii, wei = ladder.call[1:length(posi)],
                      corr = abs(cor(ladder.call[1:length(posi)],
                                     posi)), error = fact)
        return(roxy2)
      }
      rt <- apply(data.frame(step4), 2, FUN = caller, roxy = roxy,
                  ladder.call = ladder, x = x)
      corrs3 <- unlist(lapply(rt, function(x) {
        x$error
      }))
      roxy3 <- rt[[which(corrs3 == max(corrs3))[1]]]
      if (draw == TRUE) {
        limi <- sort(roxy3$hei, decreasing = TRUE)
        plot(x, type = "l", xaxt = "n", ylim = c(0, (limi[3] +
                                                       1000)), cex.axis = 0.6, las = 2, col = transp("grey35",
                                                                                                     0.7), ylab = "RFU", xlab = "", lwd = 2, main = attributes(x)$mycomm,
             cex.main = cex.title)
        axis(1, at = roxy3$pos, labels = roxy3$wei, cex.axis = 0.6)
        points(x = roxy3$pos, y = roxy3$hei, cex = 1.1,
               col = transp("black", 0.85))
        points(x = roxy3$pos, y = roxy3$hei, pch = 20,
               col = transp("red", 0.7))
        legend("topleft", legend = paste("Correlation:",
                                         round(roxy3$corr, digits = 4), sep = ""), bty = "n")
        legend("topright", legend = c("Peaks selected"),
               col = c("red"), bty = "n", pch = c(20), cex = 0.85)
      }
      roxy <- roxy3
    }
  }
  return(roxy)
}
#' plotAllPeaks
#' @description plots all peaks (1 per file to outdir)
#' @export
#' @param peaks  list of peak results
#' @param outdir output directory
#' @param single plot one peak per image or multiple
#' @param xlim  window along x scale
#' @param ylim  window along y scale (can be local or global max)
#' @return list of peakPlotImages

plotAllPeaks<-function(peaks,outdir,single,xlim,ylim,thresh)
{
  #check output dir exists if not make it
  if(!dir.exists(outdir))
  {
    if(!dir.create(outdir,mode="0777"))
    {
      stop(paste("Could not create output directory : ",outdir))
    }
  }

  pnames<-names(peaks)

  for(idx in 1:length(peaks))
  {
    if(!dir.create(paste(outdir,pnames[idx],sep="/"),mode="0777"))
    {
      stop(paste("Could not create output directory : ",pnames[idx]))
    }

    out  <-paste(outdir,pnames[idx],sep="/")

    #---plot all peaks in xlim on single plot axes
    pplot<-plotSinglePeak(peaks[[idx]],
                          ylim  =ylim,
                          xlim  =xlim,
                          col   ="royalblue",
                          mint  =thresh,
                          outdir=out,
                          single=single,
                          title =T)


    if(!pplot){stop(paste("Error: plot(s) not made ",pnames[idx]))}
  }
  #check peaks - one subdir for each fsa file with set of peaks
  #peaks numbered by xlim


}
#' demoFSA
#' @export
#' @description demoFSA is a an example usage script for microsatellite marker data
#'         the input format is assumed to be ABI solid
#' @return peaks returns list of peaks to indicate success

demoFSA<-function()
{
  fsadir   <- "C://Users/alo33/Documents/Work/iPeak/data/FSA"
  fsafiles <- paste(fsadir,dir(fsadir),sep="/")
  files    <- readFiles(fsafiles,fmt="fsa")

  #define channel ladder
  channel  <- c(35,50,75,100,139,
                150,160,200,250,300,
                340,350,400,450,490,500)

  #--- for fsa files only use channel calibration ladder
  peaks    <- scorePeaks(files,
                         channel=channel,
                         chnum  =2,
                         dna    =F,
                         aa     =F,
                         thresh =50,
                         minpc  =0.5,
                         xlim   =c(50,500))

  outdir<-fsadir

  writePeakTable(peaks  =peaks,
                 fmt    ="xls",
                 outfile=paste(outdir,"allPeaks.xlsx",sep="/"))



  plotAllPeaks(peaks,
               outdir =paste(outdir,"img",sep="/"),
               single =T,
               xlim  =c(50,500),
               ylim  =c(0,5000),
               thresh=50)

  #writeHTMLReport(peaks,outfile="")

  return(peaks)

}
#' demoWIG
#' @export
#' @description demoWIG is a an example usage script for microsatellite marker data
#'         the input format is assumed to be ABI solid
#' @return peaks returns list of peaks to indicate success

demoWIG<-function()
{
  wigdir   <- "data/WIG"
  wigfiles <- paste(wigdir,dir(wigdir),sep="/")
  files    <- readFiles(wigfiles,fmt="wig")

  #--- for fsa files only use channel calibration ladder
  peaks    <- scorePeaks(files,
                         channel=NULL,
                         chnum  =2,
                         dna    =F,
                         aa     =F,
                         thresh =50,
                         minpc  =0.5,
                         xlim   =c(50,500))

  outdir<-wigdir

  return(peaks)

}
#' checkFargs
#' @description a function checker script for safety
checkFargs<-function(fargs,fun)
{
 #--- function argument checker
 sapply(fargs,
    function(a){
     try (if(grep("^[a-zA-Z0-9\.\_]+$",a)) stop(paste("Illegal characters in function arguments:",a,fun,sep=" ")))
 })

}
#' demoBED
#' @export
#' @description a demoBED script for example using chipseq peaks
#' @return peaks returns list of peaks
demoBED<-function()
{
  #-- this is a chipseq peak example - valid for all peaky types
  #pick up path locally from pkg vars
  #beddir   <- "data/bed"
  paste(system.file(package="iPeak"),"/data/bed",sep="")
  bedfiles <- paste(beddir,dir(beddir),sep="/")
  files    <- readFiles(bedfiles,fmt="bed")
  peaks    <- scorePeaks(files,channel=NULL,ch=3,dna=T,plot=T)
  outdir   <- beddir

  results  <- writePeakTable(peaks,
                             fmt    ="txt",
                             outfile=paste(outdir,"allPeaks.txt",sep="/"))

  if(!dir.exists(paste(outdir,'img',sep='/')))
  {
    dir.create(paste(outdir,'img',sep='/'),mask='0777')
  }

  resultsByChr<-split(results,results$Name)
  imagesByChr <-lapply(resultsByChr,summaryImage,6,7,outdir)

  writePeakSummary(peaks,outfile)
  writeHTMLReport(peaks,outfile)

}
