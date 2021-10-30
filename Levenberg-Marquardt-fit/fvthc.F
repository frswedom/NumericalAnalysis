*Modification historyOB
*960314 Original routine from Helen L Caines
*       On error just set iflags and return                        rz
*000210 svertex%iflag += ier now, since v0find already uses it    rab

      subroutine fvthc(ndata,isvertex_p,covar,ier)
      implicit none
      external fvtget
***
*  Driving routine for track fitter
***
#include "veefit.inc"
#include "na49_event_str.gfortran.inc"

      type(fvtpar_t) fvtpar
      type(vertex_t) svertex
      type(track_t) tracka,trackb
      type(rtrack_t) rtracka,rtrackb

      pointer(ifvtpar_p,fvtpar)
      pointer(isvertex_p,svertex)
      pointer(tracka_p,tracka)
      pointer(trackb_p,trackb)
      pointer(rtracka_p,rtracka)
      pointer(rtrackb_p,rtrackb)

      real xin(3),xout(3),pin(3),pout(3)
      integer ierr
      integer ndata,ier,ma,mfit,i,itnumb,NCA
      parameter ( NCA=9)
      integer lista(NCA),kflag,llflag,iflag
      double precision a(NCA),olda(NCA)
      double precision covar(NCA,NCA),alpha(NCA,NCA)
      double precision fvtdev,chisq,ochisq
      real alamda,p,pxz,lambda
      logical fvrcon
      common /FVTPARS/ ifvtpar_p
      common /FRZITV/ itnumb
      common /FVTDEV/ fvtdev
      data kflag /1/

      ma = NCA
      mfit = NCA

      tracka_p = svertex%daughter_p
      trackb_p = tracka%next_daughter_p
      rtracka_p = tracka%rtrack_p
      rtrackb_p = trackb%rtrack_p
***   
*  lista numbers the parameters with those to be adjusted first
*  ma could be larger than mfit
***
      fvtdev = 1.0
      llflag = 0

 1    continue
      ier = 0
      do i=1,ma
        lista(i) = i
      enddo

      a(1) = svertex%x
      a(2) = svertex%y
      a(3) = svertex%z
      a(4) = tracka%px
      a(5) = tracka%py
      a(6) = tracka%pz
      a(7) = trackb%px
      a(8) = trackb%py
      a(9) = trackb%pz
***
*  Initialise MQMINV
***
 25   continue
      itnumb = 0 
      alamda = -1.0
      call mqminv(ndata,a,ma,lista,mfit,covar,alpha,nca,chisq,
     >     fvtget,alamda,ier)
      if (ier .ne. 0) go to 800
***
*  set IFLAG to 0 to require two subsequent decreases in chisq
*  1 to exit if chisq goes down but by less than .001
***
      iflag = kflag
      ochisq = chisq
      do i = 1, nca
        if( abs(a(i)) .lt. 1.0D-25 ) then
          olda(i) = sign(1.0D-25,a(i))
        else
          olda(i) = a(i)
        endif
      enddo
***
*  search loop
***
      do itnumb =1,fvtpar%itemax
        call mqminv(ndata,a,ma,lista,mfit,covar,alpha,nca,chisq,
     >       fvtget,alamda,ier)
        if (llflag.eq.0.and.(ier.eq.2.or.ier.eq.3)) then
          llflag = 1
          fvtdev = 0.1
          if( fvtpar%print .ne. 0.0 ) write(6,'('' Trying again'')')
          if( ier .eq. 2 ) go to 1
          ier = 0
          go to  25
        end if
        if( ier .ne. 0 ) go to  800
***  
*    exit condition - chisq decreased by less then 0.001 relatively
***  
        if (chisq .le. ochisq) then
          if( fvrcon(chisq,ochisq,a,olda) ) then
            if( iflag .eq. 0 ) then
              iflag = 1
            else
*          clean-up
              alamda = 0.0
              call mqminv(ndata,a,ma,lista,mfit,covar,alpha,nca
     >             ,chisq,fvtget,alamda,ier)
              go to 800
            endif
          endif
          ochisq= chisq
          do i=1,nca
            if( abs(a(i)) .lt. 1.0D-25 ) then
              olda(i) = sign(1.0D-25,a(i))
            else
              olda(i) = a(i)
            endif
          enddo
        endif
      enddo
      ier = 4
***
*  Get the parameters back into single precision
***
 800  continue
      tracka%iflag = ier
      trackb%iflag = ier
*  RAB Changed to accommodate v0find using vertex.iflag
      svertex%iflag = svertex%iflag + ier
      if( ier.ne.0 ) return

      svertex%pchi2 = real(chisq) 
      svertex%x = a(1)
      svertex%y = a(2)
      svertex%z = a(3)
      svertex%sig_x = sqrt(covar(1,1))
      svertex%sig_y = sqrt(covar(2,2))
      svertex%sig_z = sqrt(covar(3,3))
      tracka%px = a(4)
      tracka%py = a(5)
      tracka%pz = a(6)
      tracka%tanl = tracka%py/tracka%pz
      p = sqrt(tracka%px**2 + tracka%py**2 + tracka%pz**2)
      lambda = atan(tracka%tanl)
      pxz = p*cos(lambda)

      tracka%qpxz = 1.0/pxz
      tracka%phi = acos(tracka%px/(pxz))
      tracka%sig_px = sqrt(covar(4,4))
      tracka%sig_py = sqrt(covar(5,5))
      tracka%sig_pz = sqrt(covar(6,6))

      tracka%bx = -999.
      tracka%by = -999.
      if (rtracka_p.ne.0) then
         xin(1) = rtracka%x
         xin(2) = rtracka%y
         xin(3) = rtracka%z
         pin(1) = rtracka%px
         pin(2) = rtracka%py
         pin(3) = rtracka%pz      
         xout(3) = svertex%z
         call trkstep(1.0, xin, pin, xout, pout, .true., 2.0,
     >        .false., ierr)
      
         if(ierr.eq.0) then
            tracka%bx = xout(1) - svertex%x
            tracka%by = xout(2) - svertex%y          
         endif
      endif
      
      trackb%px = a(7)
      trackb%py = a(8)
      trackb%pz = a(9)
      trackb%tanl = trackb%py/trackb%pz
      p = sqrt(trackb%px**2 + trackb%py**2 + trackb%pz**2)
      lambda = atan(trackb%tanl)
      pxz = p*cos(lambda)
      trackb%qpxz = (-1.0)/pxz
      trackb%phi = acos(trackb%px/pxz)
      trackb%sig_px = sqrt(covar(7,7))
      trackb%sig_py = sqrt(covar(8,8))
      trackb%sig_pz = sqrt(covar(9,9))

      trackb%bx = -999.
      trackb%by = -999. 
      if (rtrackb_p.ne.0) then
         xin(1) = rtrackb%x
         xin(2) = rtrackb%y
         xin(3) = rtrackb%z
         pin(1) = rtrackb%px
         pin(2) = rtrackb%py
         pin(3) = rtrackb%pz      
         xout(3) = svertex%z
         call trkstep(-1.0, xin, pin, xout, pout, .true., 2.0,
     >        .false.,ierr)
         
         if(ierr.eq.0) then
            trackb%bx = xout(1) - svertex%x
            trackb%by = xout(2) - svertex%y                    
         endif
      endif
      
      end

*******************************************************************************
#ifdef OLD_KAPF
@process fixed(132)
#endif

      logical function fvrcon(chisq,ochisq,a,olda)
      implicit none
***
*  MQ fit exit condition (if TRUE - exit)
***
#      include "veefit.inc"
      type(fvtpar_t) fvtpar
      pointer(fvtpar_p,fvtpar)
      double precision a(*),olda(*)
      double precision chisq,ochisq
      integer ifvtpar_p
      common /FVTPARS/ ifvtpar_p
      fvtpar_p = ifvtpar_p
      
*  True is Chisq INCREASED at least once for this fit
*  
      fvrcon = (
     >    ( (ochisq-chisq)/ochisq.lt.dble(fvtpar%dchisq) ).and.
     >     ( abs((olda(1)-a(1))/olda(1)).lt.dble(fvtpar%deltax) ).and.
     >     ( abs((olda(2)-a(2))/olda(2)).lt.dble(fvtpar%deltay) ).and.
     >     ( abs((olda(3)-a(3))/olda(3)).lt.dble(fvtpar%deltaz) ).and.
     >     ( abs((olda(4)-a(4))/olda(4)).lt.dble(fvtpar%deltapx) ).and.
     >     ( abs((olda(5)-a(5))/olda(5)).lt.dble(fvtpar%deltapy) ).and.
     >     ( abs((olda(6)-a(6))/olda(6)).lt.dble(fvtpar%deltapz) ).and.
     >     ( abs((olda(7)-a(7))/olda(7)).lt.dble(fvtpar%deltapx) ).and.
     >     ( abs((olda(8)-a(8))/olda(8)).lt.dble(fvtpar%deltapy) ).and.
     >     ( abs((olda(9)-a(9))/olda(9)).lt.dble(fvtpar%deltapz) ) )
     
      end
******************************************************************************
