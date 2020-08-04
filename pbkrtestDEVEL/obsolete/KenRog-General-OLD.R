KRmodcomp <- function(largeModel, smallModel,beta0=0){
    UseMethod("KRmodcomp")
}

KRmodcomp.mer<-function(largeModel,smallModel,beta0=0) {
    ##smallModel can either be a lmer model or a restriction matrix L

    w <- modcomp_init(largeModel,smallModel,matrixOK=TRUE)
    if (w==-1) {
        print ('Error in KRmodcomp')
        print( 'both models have either equal fixed mean stucture')
        print( 'or are not nested')
        stop()
    }
    if (w==0){
        print ('Error in KRmodcomp')
        print( 'first given model is submodel of second')
        print(' exchange the models')
        stop()
    }



    ## refitting large model with REML if necessary
    ##!!! NEED to check that a gaussian mixed model is fitted
    largeModel<-
        if (largeModel@dims['REML'] == 1)
        {
            largeModel
        }
        else
        {
            update(largeModel,.~.,REML=TRUE)
        }


    L<- if   ( 'mer' %in% class(smallModel) ) {
      .fatAB(smallModel@X,largeModel@X)
    } else {
      smallModel
    }



    ## check wethter the the rows of L are linear depende
    ## if this is the case, a row.reduced L with
    ## linear independen rows is constructed
    ## this is neceaasre to avoid singlur L %*% Phi t(L)

    q<-rankMatrix(L)

    if (q < nrow(L) ){
      L<-t(qr.Q(qr(t(L)))[,1:qr(L)$rank])
    }
    L<-.makeSparse(L)

    ## All computations are based on 'largeModel' and the restriction matrix 'L'
    ## -------------------------------------------------------------------------

    stats<-.KRmodcompPrimitive(largeModel,L, beta0)
    formSmall<-
        if ('mer' %in% class(smallModel)){
            .zzz <- formula(smallModel)
            attributes(.zzz) <- NULL
            .zzz
          } else {
            list(L=L,beta0=beta0)
          }
    formLarge <- formula(largeModel)
    attributes(formLarge) <- NULL

    res<-list(stats=stats,f.large=formLarge,f.small=formSmall)
    class(res)<-c("KRmodcomp")
    res
}


.KRmodcompPrimitive<-function(largeModel,L, beta0) {

    X<-largeModel@X
    
    Phi    <- vcov(largeModel)
    GGamma <- VarCorr(largeModel)
                                        # s -> n.varcomp
    n.groupFac<- largeModel@dims['nt'] #= number of random effects terms (..|..)
                                        # (..|F1) + (..|F1) are group factors!
                                        # without the residual variance
    
    ## size of the symmetric variance Gamma_i for reach groupFac
    nn.GGamma <- integer(n.groupFac)
    ggamma <- NULL
    for (ii in 1: (n.groupFac)) {
      Lii<-GGamma[[ii]]
      nu<-ncol(Lii)
      nn.GGamma[ii]<- nu
      ## The lower.tri construxtion esnures, that (because Lii is symmetric!)
      ## Lii[lower.tri(Lii,diag=TRUE)= Lii[1,1],Lii[1,2],Lii[1,3]..Lii[1,nu],
      ##                               Lii[2,2], Lii[2,3] ...
      ggamma<-c(ggamma,Lii[lower.tri(Lii,diag=TRUE)])
    }
    
    ## number of variance parameters of each GGamma_i
    mm.GGamma<- nn.GGamma * (nn.GGamma+1)/2
    ##adding the residuals variance to ggamma
    ##so in ggamma nd n.ggamma the residual variance is included!
    ggamma<-c(ggamma,attr(GGamma,'sc')^2)
    n.ggamma<-length(ggamma)

    ##
    group.index<-largeModel@Gp
    nn.groupFac<-diff(group.index)

    ## number of random effects in each groupFac
    ## residual error here excluded!
    nn.groupFacLevels<-nn.groupFac/nn.GGamma

    Zt<-largeModel@Zt
    ## Sigma:
    G<-Omega<-NULL
    for (ss in 1:n.groupFac)
      {
        zIndex.sub<-group.index[ss]+
          1+c(0:(nn.GGamma[ss]-1))*nn.groupFacLevels[ss] +
            rep(0:(nn.groupFacLevels[ss]-1),each=nn.GGamma[ss])
        ##ZZ<-Zt[ (index.nn.group[ss]+1):index.nn.group[ss+1], ]
        ZZ<-Zt[zIndex.sub, ]
        #cat("dim(ZZ)"); print(dim(ZZ))
        Ig<-sparseMatrix(1:nn.groupFacLevels[ss],
                         1:nn.groupFacLevels[ss],x=1)
            
        for (rr in 1:mm.GGamma[ss] )
          {
            ii.jj <- .indexVec2Symmat(rr,nn.GGamma[ss])
            ii.jj <- unique(ii.jj)
            EE    <-
              if (length(ii.jj)==1){
                sparseMatrix(ii.jj,ii.jj,x=1,dims=rep(nn.GGamma[ss],2))
              } else {
                sparseMatrix(ii.jj,ii.jj[2:1],dims=rep(nn.GGamma[ss],2))
              }
            EE<-Ig %x% EE  ## Kronecker product
            G<-c(G,list(t(ZZ)%*% EE %*% ZZ))
          }
      }
    
    G<-c(G,list(sparseMatrix(1:nrow(X),1:nrow(X),x=1))) ## The last one is for the residual!
    
    ##Error ?? before (28.04.2010): Sigma<-G[[1]]<- ggamma[1]*G[[1]]
    t0 <- proc.time()
    Sigma<-ggamma[1]*G[[1]]
    for (ii in 2:n.ggamma) {
      Sigma<- Sigma + ggamma[ii] * G[[ii]]
    }
    cat(sprintf("Finding Sigma:    %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()
                
    SigmaInv <- chol2inv(chol(forceSymmetric(Sigma)))
    cat(sprintf("Finding SigmaInv: %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()

    ##cat(sprintf("n.ggamma=%i\n", n.ggamma))
    Ktrace <- matrix(NA,n.ggamma,n.ggamma)
    for (ii in 1:n.ggamma) {
      SigInv.ii.G <- SigmaInv %*% G[[ii]]
      Omega       <- c(Omega,list(-1 * SigInv.ii.G %*% SigmaInv))

      Ktrace[ii,ii] <- sum(SigInv.ii.G * t(SigInv.ii.G))
      if (ii < n.ggamma){
        for (jj in (ii+1):n.ggamma) {
          ##Ktrace[ii,jj]<- Ktrace[jj,ii]<- sum((SigmaInv%*% G[[ii]]) * (t(G[[jj]]) %*% SigmaInv))
          ##Ktrace[ii,jj] <- Ktrace[jj,ii]<- sum(SigInv.ii.G * (t(G[[jj]]) %*% SigmaInv))
          Ktrace[ii,jj] <- Ktrace[jj,ii]<- sum(SigInv.ii.G *  tcrossprod(G[[jj]], SigmaInv))
        }
      }     
    }
    
    cat(sprintf("Finding Ktrace:   %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()
    
    P <- Q <-NULL
    for (ii in 1:n.ggamma) {
      Om.ii.Sigma <- Omega[[ii]] %*% Sigma
      P <- c(P, list(forceSymmetric( t(X) %*% Omega[[ii]] %*% X)))
      #Om.ii.Sigma <- crossprod(Omega[[ii]], Sigma)
      for (jj in c(1:n.ggamma)) {
        #k<- Omega[[ii]] %*% Sigma %*% Omega[[jj]]
        #Om.ii.Sigma <- Omega[[ii]] %*% Sigma
        k <- Om.ii.Sigma %*% Omega[[jj]]
        #k<- tcrossprod(Om.ii.Sigma, Omega[[jj]])
        Q<- c(Q,list(t(X) %*% k %*% X ))
        #Q<- c(Q,list(crossprod(X,k) %*% X ))
      }}
    cat(sprintf("Finding P,Q:      %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()
   
    IE2<-matrix(NA,n.ggamma,n.ggamma)
    for (ii in 1:n.ggamma) {
      Phi.P.ii <- Phi %*% P[[ii]]
      for (jj in c(1:n.ggamma)) {
        IE2[ii,jj]<- Ktrace[ii,jj] - 2 * sum(Phi*Q[[.ij2r(ii,jj,n.ggamma)]]) +
          sum( Phi.P.ii * (  P[[jj]] %*% Phi))
      }}
    cat(sprintf("Finding IE2:      %10.5f\n", (proc.time()-t0)[1] )); t0 <- proc.time()

    eigenIE2 <- eigen(IE2,only.values=TRUE)$values
    condi    <- min(abs(eigenIE2))
    
    W<- if(condi>1e-10) forceSymmetric(2* solve(IE2)) else forceSymmetric(2* ginv(IE2))
    ##print('kenRog ginv W')

    U<-matrix(0,ncol(X),ncol(X))
                                        # U is symmetric because Q[i,j,,]+Q[j,i,,] is symmetric
    for (ii in 1:n.ggamma) {
      for (jj in c(1:n.ggamma)) {
        U<- U+  W[ii,jj] * (Q[[.ij2r(ii,jj,n.ggamma)]]- P[[ii]] %*% Phi %*% P[[jj]])
      }}
    
    
    GGAMMA <-  Phi %*% U %*% Phi
    PhiA   <-  Phi + 2* GGAMMA
    Theta  <-  t(L) %*% solve( L %*% Phi %*% t(L), L)

    A1<-A2<-0
    ThetaPhi<-Theta%*%Phi
    for (ii in 1:n.ggamma) {
        for (jj in c(ii:n.ggamma)) {
            e<-ifelse(ii==jj, 1, 2)
            ui<-ThetaPhi %*% P[[ii]] %*% Phi
            uj<-ThetaPhi %*% P[[jj]] %*% Phi
            A1<- A1+  e* W[ii,jj] * (.spur(ui) * .spur(uj))
            A2<- A2+  e* W[ii,jj] *  sum(ui * t(uj))
        }}


    q<-rankMatrix(L)
    B<-1/(2*q) * (A1+6*A2)
    g<- ( (q+1)*A1 - (q+4)*A2 )  / ((q+2)*A2)
    c1<-g/(3*q+ 2*(1-g))
    c2<- (q-g) / (3*q + 2* (1-g))
    c3<- (q+2-g) / ( 3*q+2*(1-g))

###orgDef: E<-1/(1-A2/q)
###orgDef: V<- 2/q * (1+c1*B) /  ( (1-c2*B)^2 * (1-c3*B) )

    V0<-1+c1*B
    V1<-1-c2*B
    V2<-1-c3*B
    V0<-ifelse(abs(V0)<1e-10,0,V0)


###orgDef: V<- 2/q* V0 /(V1^2*V2)
###orgDef: rho <-  V/(2*E^2)

    rho <- 1/q * (.divZero(1-A2/q,V1))^2 * V0/V2

    df2 <- 4 + (q+2)/ (q*rho-1)

###orgDef: F.scaling <-  df2 /(E*(df2-2))
###altCalc F.scaling<- df2 * .divZero(1-A2/q,df2-2,tol=1e-12)
    ## this does not work because df2-2 can be about 0.1
    F.scaling<-ifelse( abs(df2-2)<1e-2, 1 , df2*(1-A2/q)/(df2-2))

                                        #The F-statistic
    betaDiff<-cbind(fixef(largeModel)-beta0)

    Fstat<- F.scaling/q * t(betaDiff) %*% t(L) %*% solve(L%*%PhiA%*%t(L),L%*%betaDiff)

    Fstat<-as.numeric(Fstat)
    pval<-pf(Fstat,df1=q,df2=df2,lower=FALSE)

                                        #fstatistioc not multilplied by F.scaling
    FstatU<- as.numeric(1/q * t(betaDiff) %*% t(L) %*% solve(L%*%PhiA%*%t(L),L%*%betaDiff))
    pvalU<-pf(FstatU,df1=q,df2=df2,lower=FALSE)


    stats<-c(df1=q,df2=df2,Fstat=Fstat,
             p.value=pval,F.scaling=F.scaling,FstatU=FstatU,p.value.U=pvalU,condi=condi,
             A1=A1,A2=A2,V0=V0,V1=V1,V2=V2,rho=rho)
    attr(stats,"eigenIE")<-eigenIE2
    stats
}



print.KRmodcomp <- function(x,...){
    cat("F-test with Kenward-Roger approximation \n")
##     formLarge <- x$f.large
##     attributes(formLarge) <- NULL
    cat("large : ")
    print(x$f.large)
    if (inherits(x$f.small,"call"))
    {
      cat("small : ")
      print(x$f.small)
    }
    else {
      formSmall <- x$f.small
      cat("small : Lbeta=beta0")
      cat('L=')
      print(formSmall$L)
      cat('beta0=')
      print(formSmall$beta0)
    }

    stats<-x$stats
##     cat(sprintf("df1=%3i, df2=%8.2f, Fstat=%8.2f, pval=%7.5f, Fscal= %4.3f \n",
##                 stats['df1'], stats['df2'], stats['Fstat'], stats['pval'],
##                 stats['F.scaling']) )

    sss<-stats[c("Fstat","df1","df2","p.value","F.scaling")]
    sss<-as.data.frame(as.list(sss))
    
    sss$p.value <-round(sss$p.value, options("digits")$digits)
    sss$Fstat   <-round(sss$Fstat,   options("digits")$digits)

    print(sss, row.names=FALSE)
    
##    print(as.data.frame(stats))
    
    if (stats['F.scaling']<0.2) {
        cat('The scaling factor for the F-statistic is smaller than 0.2 \n')
        cat('The unscaled statistic might be more reliable \n ')
        cat('Results fromm the unscaled F-statistic \n')
        cat(sprintf("df1=%3i, df2=%8.2f, FstatU=%8.2f, pvalU=%7.5f  \n",
                    stats['df1'], stats['df2'], stats['FstatU'], stats['pvalU']))
    }
    return(invisible(x))
}
