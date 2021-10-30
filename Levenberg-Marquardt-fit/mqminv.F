      subroutine mqminv(ndata,a,ma,lista,mfit,
     >                  covar,alpha,nca,chisq,funcs,alamda,ier)
        implicit none
      external funcs
***
* Levenberg-Marquardt nonlinear chisq fit
* W.H.Press et al - NUMERICAL RECIPES, Cambridge UP 86, page 526
* IERR added - set to 1 if LISTA contains improper permutation
*                     2 if covariance matrix is cannot be converted
***
#       include "v0find.inc"
#       include "veefit.inc"
        type(fvtpar_t) fvtpar
        pointer(ifvtpar_p,fvtpar)
        integer ier,itnumb,j,k,ma,mfit,nca,ndata,MMAX
        integer lista(ma)
        parameter ( MMAX = 100 )
        double precision a(nca), covar(nca,nca), alpha(nca,nca)
        double precision atry(MMAX), beta(MMAX), da(MMAX)
        double precision chisq,ochisq
        real alamda
        common/FRZITV/ itnumb
* Number of iterations
        common /FVTPARS/ ifvtpar_p
***
* True is Chisq INCREASED at least once for this fit
***
*   Initialization
      ier = 0
      if ( alamda .lt. -0.1 ) then
        alamda = fvtpar%xlamda
        call mqchiv(ndata,a,ma,mfit,chisq,funcs)
        call mqcofv(ndata,a,ma,lista,mfit,alpha,beta,nca,funcs)
        ochisq = chisq
        do  13  j = 1, ma
           atry(j) = a(j)
13      continue
      endif
*
      do  15  j = 1, mfit
        do  14  k = 1, mfit
          covar(k,j) = alpha(k,j)
 14     continue
        covar(j,j) = alpha(j,j)*(1.0+alamda)
        da(j) = beta(j)
15    continue
*
*   Matrix solution
      call gaussj_v0fit(covar,mfit,nca,da,1,1,ier)
      if( ier .ne. 0 ) then
        ier = 2
        if( alamda .ne. 0.0 ) return
      endif
*
*  If converged, evaluate covariance matrix
      if ( alamda .eq. 0.0 ) then
        call covsrt_v0fit(covar,nca,ma,lista,mfit)
        return
      end if
*
      do  16  j = 1, mfit
        atry(lista(j)) = a(lista(j))+da(j)
 16    continue
      call mqchiv(ndata,atry,ma,mfit,chisq,funcs)
      if( chisq .le. ochisq ) then
        if( alamda .lt. 1.0E-15 ) then
          ier = 3
          call covsrt_v0fit(covar,nca,ma,lista,mfit)
          return
        endif
* accept new solution
        alamda = 0.1*alamda
        call mqcofv(ndata,atry,ma,lista,mfit,covar,da,nca,funcs)
        ochisq = chisq
        do  18  j = 1, mfit
          do  17  k = 1, mfit
            alpha(k,j) = covar(k,j)
 17        continue
          beta(j) = da(j)
          a(lista(j)) = atry(lista(j))
 18      continue
      else
* failure - increase ALAMDA  and return
        if( alamda .gt. 1.0E+15 ) then
          ier = 3
          call covsrt_v0fit(covar,nca,ma,lista,mfit)
          return
        endif
        alamda = 10.0*alamda
      endif
* DEBUGGING Print - delete when ready:  (OK - I know - NEVER)
c 900   continue
       continue
      if( fvtpar%print .ne. 0.0 ) then
        write(6,'('' Iteration'',I5,''  chisq'',F15.5,
     >          ''       Lambda'',1PE15.1)') itnumb,
     >                chisq/real((2*ndata)-mfit),alamda
      endif
      end

*****************************************************************************

      subroutine mqchiv(ndata,a,ma,mfit,chisq,funcs)
        implicit none
      external funcs
#       include "veefit.inc"
***
* Used by MQMINV  to calculate chisq
* W.H.Press et al - NUMERICAL RECIPES, Cambridge UP 86, page 527
****
* Maximum number of hits per VEE - TPC+WC+DC = MXHITV
        integer i,ndata,ma,mfit,MMAX
      parameter ( MMAX = 100 )
      double precision dfitf1,dfitf2
      common/FVTFUN/ dfitf1(MXHITV,9),dfitf2(MXHITV,9)
      double precision dyda(MMAX),dzda(MMAX),a(ma)
      double precision chisq,ymod,xmod
*
      call fvcal2(a,fitf1)
*
      chisq = 0.0
      do 15 i=1,ndata
        call funcs(i,ymod,xmod,dyda,dzda)
        chisq = chisq + ymod*ymod + xmod*xmod
15    continue
      end

*****************************************************************************

      subroutine  mqcofv(ndata,a,ma,lista,mfit,alpha,beta,nlp,funcs)
        implicit none
      external funcs
* Used by MQMINV  to calculate curvature matrix
* W.H.Press et al - NUMERICAL RECIPES, Cambridge UP 86, page 527
*
        integer i,j,k,ndata,ma,mfit,nlp,MMAX
        integer lista(mfit)
        parameter ( MMAX = 100 )
         double precision alpha(nlp,nlp), beta(ma),a(ma)
         double precision dyda(MMAX),dxda(MMAX)
        double precision ymod,xmod,wt,vt
*
      call fvtder(a)
      do  12  j = 1, mfit
        do  11  k = 1, j
          alpha(j,k) = 0.0
11      continue
        beta(j) = 0.0
12    continue
*
      do  15  i = 1, ndata
        CALL FUNCS(i,ymod,xmod,dyda,dxda)
        do 14  j = 1, mfit
          wt = dyda(lista(j))
          vt = dxda(lista(j))
          do  13  k = 1, j
            alpha(j,k) = alpha(j,k) + wt*dyda(lista(k)) +
     >                                vt*dxda(lista(k))
13        continue
          beta(j) = beta(j) + ymod*wt + xmod*vt
14      continue
15    continue
*
      do  17  j = 2, mfit
        do  16  k = 1, j-1
          alpha(k,j) = alpha(j,k)
16      continue
17    continue
      end
******************************************************************************

        subroutine fvtder(param)
        implicit none
*
*   ********************************************************************
*   *                                                                  *
*$$ *   SUBROUTINE FVTDER  calculates derivatives for LM fit           *
*   *   step taken from tp.p.fp (FVRINI)                               *
*   *                                                                  *
*   ********************************************************************
*
#     include "veefit.inc"
        integer ifvtpar_p,iv0_pointa_p,iv0_pointb_p
        integer i,k,numhit,nh1,nh2,pmult
      double precision fvtdev
      type(fvtpar_t) fvtpar
      pointer(ifvtpar_p,fvtpar)
* Maximum number of hits per VEE - TPC+WC+DC
      double precision dfitf1,dfitf2
      common /FVTFUN/ dfitf1(MXHITV,9),dfitf2(MXHITV,9)
      common /FVTRCD/ numhit,nh1,nh2,iv0_pointa_p,
     >                    iv0_pointb_p,pmult
      common /FVTPARS/ ifvtpar_p
      common /FVTDEV/ fvtdev
* Multiplier of derivative step (1 originally,0.1 after failure)
*
      double precision param(*),dd
      double precision partmp(9),fitmp(2,MXHITV)
      real ft(9)
*
*
      do 10 i=1,9
      partmp(i) = param(i)
 10   continue
        ft(1) = fvtpar%stepx
        ft(2) = fvtpar%stepy
        ft(3) = fvtpar%stepz
        ft(4) = fvtpar%steppx
        ft(5) = fvtpar%steppy
        ft(6) = fvtpar%steppz
        ft(7) = fvtpar%steppx
        ft(8) = fvtpar%steppy
        ft(9) = fvtpar%steppz

*
      do  200  i = 1, 9
        dd = fvtdev*ft(i)
        partmp(i) = param(i) + dd
        call fvcal2(partmp,fitmp)
        do 101 k = 1,numhit
           dfitf1(k,i) = (fitmp(1,k)-fitf1(1,k))/dd
           dfitf2(k,i) = (fitmp(2,k)-fitf1(2,k))/dd
 101     continue
        partmp(i) = param(i)
 200   continue
       end
******************************************************************************
        subroutine fvcal2 (param,ztout)
        implicit none
***
* Calculate function for VEE fitter - the function is returned in ZTOUT
***
        integer iv0_pointb_p,iv0_pointa_p
        integer numhit,nh1,nh2,pmult,i,ierr
#     include "veefit.inc"
      type(v0_point_t) v0_pointa(255),v0_pointb(255)
      pointer(iv0_pointa_p,v0_pointa)
      pointer(iv0_pointb_p,v0_pointb)
* Maximum number of hits per VEE - TPC+WC+DC = MXHITV
      common /FVTRCD/ numhit,nh1,nh2,iv0_pointa_p,
     >                     iv0_pointb_p,pmult
        double precision dfitf1,dfitf2
      double precision param(9),ztout(2,MXHITV)
      real xin(3),xout(3),pin(3),pout(3)
        real pmz
      common /FVTFUN/ dfitf1(MXHITV,9),dfitf2(MXHITV,9)
*
*
      xin(1) = param(1)
      xin(2) = param(2)
      xin(3) = param(3)
*
      pin(1) = param(4)
      pin(2) = param(5)
      pin(3) = param(6)

      do 100  i = 1,nh1
        xout(3) = v0_pointa(i)%z
        call trkstep(1.0,xin,pin,xout,pout,.true.,2.0,
     >             .false.,ierr)
        if( ierr .ne. 0 ) then
*         write(6,*) 'Ierr=',ierr
          return
        endif

        if ((v0_pointa(i)%var_x.eq.0).or.(v0_pointa(i)%var_y.eq.0)) then
          return
        endif

        pmz = pmult*(param(3)-v0_pointa(i)%z)
        pmz = MAX(pmz,1.0)
        ztout(1,i) = pmz*(v0_pointa(i)%y-xout(2))/v0_pointa(i)%var_y
        ztout(2,i) = pmz*(v0_pointa(i)%x-xout(1))/v0_pointa(i)%var_x
        pin(1) = pout(1)
        pin(2) = pout(2)
        pin(3) = pout(3)
        xin(1) = xout(1)
        xin(2) = xout(2)
        xin(3) = xout(3)
 100   continue
*
      xin(1) = param(1)
      xin(2) = param(2)
      xin(3) = param(3)
*
      pin(1) = param(7)
      pin(2) = param(8)
      pin(3) = param(9)
*
      do 200  i = 1,nh2
        xout(3) = v0_pointb(i)%z
        call trkstep(-1.0,xin,pin,xout,pout,.true.,2.0,
     >                .false.,ierr)
        if( ierr .ne. 0 ) return

        if ((v0_pointb(i)%var_x.eq.0).or.(v0_pointb(i)%var_y.eq.0)) then
          return
        endif

        pmz = pmult*(param(3)-v0_pointb(i)%z)
        pmz = max(pmz,1.0)
        ztout(1,i+nh1) = pmz*(v0_pointb(i)%y-xout(2))/v0_pointb(i)%var_y
        ztout(2,i+nh1) = pmz*(v0_pointb(i)%x-xout(1))/v0_pointb(i)%var_x
        pin(1) = pout(1)
        pin(2) = pout(2)
        pin(3) = pout(3)
        xin(1) = xout(1)
        xin(2) = xout(2)
        xin(3) = xout(3)
200   continue
*
*      IF(TGTFLG) THEN
*        PX = param(4) + param(7)
*        PY = param(5) + param(8)
*        PZ = param(6) + param(9)
*        YT = param(2) + (XHITSF(NUMHIT)-param(1))*PY/PX
*        ZT = param(3) + (XHITSF(NUMHIT)-param(1))*PZ/PX
*        XTOUT(NUMHIT,1) = (YHITSF(NUMHIT)-YT)/DYHITF(NUMHIT)
*        XTOUT(NUMHIT,2) = (ZHITSF(NUMHIT)-ZT)/DZHITF(NUMHIT)
*      ENDIF
      END

******************************************************************************



