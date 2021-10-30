*Modification history:
*960314 Original routine from Helen L Caines
*       On error don't touch data              rz
*       ds_exit(101) if NULL 'point' pointers  rab
#ifdef OLD_KAPF
@process fixed(132)
#endif
      subroutine vfit(n1,n2,isvertex_p,ierrmat_p)
      implicit none
***
*  Subroutine to call v0 fitter for tracks n1 & n2
*  (n1 +ve track pointer). 
*  ivsvertex - entry vertex_t for starting params
***
#include "veefit.inc"
#include "na49_event_str.gfortran.inc"
#include "point_macros.inc"

      integer isvertex_p,tracka_p,trackb_p,pointa_p,pointb_p
      integer rtracka_p,rtrackb_p
      integer ierrmat_p,iv0_pointa_p,iv0_pointb_p,n1,n2
      integer i,j,k,nh1,nh2,numhit,ier,IFITPAR
      real pmult,prob
      parameter (IFITPAR = 9)
      double precision cov(IFITPAR,IFITPAR)
      type(vertex_t) svertex
      type(track_t) tracka,trackb
      type(rtrack_t) rtracka,rtrackb
      type(point_t) pointa,pointb
      type(v0_point_t) v0_pointa(255),v0_pointb(255)
      type(covar9x9_t) errmat
      pointer(isvertex_p,svertex)
      pointer(tracka_p,tracka)
      pointer(trackb_p,trackb)
      pointer(rtracka_p,rtracka)
      pointer(rtrackb_p,rtrackb)
      pointer(pointa_p,pointa)
      pointer(pointb_p,pointb)
      pointer(ierrmat_p,errmat)
      common /FVTRCD/ numhit,nh1,nh2,iv0_pointa_p,
     >     iv0_pointb_p,pmult

      iv0_pointa_p = loc(v0_pointa(1))
      iv0_pointb_p = loc(v0_pointb(1))
      tracka_p = n1

      rtracka_p = tracka%rtrack_p
      pointa_p = rtracka%point_p

      do i=1,iand(rtracka%n_point,255)
        if (pointa_p .eq. 0) then 
          write (6,*) ' V0FIT: Error, wrong #pts on rtracka'
          call ds_exit(101)  
        endif
        v0_pointa(i)%x = pointa%x
        v0_pointa(i)%y = pointa%y
        v0_pointa(i)%z = pointa%z
        v0_pointa(i)%var_x =PNT_SIG_X(pointa%sigma)
        v0_pointa(i)%var_y =PNT_SIG_Y(pointa%sigma)
        pointa_p = pointa%next_tpoint_p
      enddo

      trackb_p = n2
      rtrackb_p = trackb%rtrack_p
      pointb_p = rtrackb%point_p

      do i=1,iand(rtrackb%n_point,255)
        if (pointb_p .eq. 0) then 
          write (6,*) ' V0FIT: Error, wrong #pts on rtrackb'
          call ds_exit(101)  
        endif
        v0_pointb(i)%x = pointb%x
        v0_pointb(i)%y = pointb%y
        v0_pointb(i)%z = pointb%z
        v0_pointb(i)%var_x =PNT_SIG_X(pointb%sigma)
        v0_pointb(i)%var_y =PNT_SIG_Y(pointb%sigma)
        pointb_p = pointb%next_tpoint_p
      enddo

      nh1 = iand(rtracka%n_point,255)
      nh2 = iand(rtrackb%n_point,255)
      numhit = nh1 + nh2

      call fvthc(numhit,isvertex_p,cov,ier)

      svertex%covar_p = loc(errmat)

      if( ier.eq.0 ) then
        svertex%pchi2 = prob(svertex%pchi2,((2*numhit)-IFITPAR))
        tracka%pchi2 = svertex%pchi2
        trackb%pchi2 = svertex%pchi2
        i = 0
        do k=1,9
          do j=k,9
            i = i + 1
            errmat%c(i) = cov(k,j)
          enddo
        enddo
      endif

c 999  end
      end
