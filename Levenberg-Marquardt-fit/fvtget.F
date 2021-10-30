      subroutine  fvtget(i,y,x,dy,dx)
        implicit none
#       include "veefit.inc"
        integer i
      double precision y,x,dy(*),dx(*)
* Maximum number of hits per VEE - TPC+WC+DC
       double precision dfitf1,dfitf2
      common /FVTFUN/ dfitf1(MXHITV,9),dfitf2(MXHITV,9)
*
      y = fitf1(1,i)
      x = fitf1(2,i)
      dy(1) = -dfitf1(i,1)
      dy(2) = -dfitf1(i,2)
      dy(3) = -dfitf1(i,3)
      dy(4) = -dfitf1(i,4)
      dy(5) = -dfitf1(i,5)
      dy(6) = -dfitf1(i,6)
      dy(7) = -dfitf1(i,7)
      dy(8) = -dfitf1(i,8)
      dy(9) = -dfitf1(i,9)
      dx(1) = -dfitf2(i,1)
      dx(2) = -dfitf2(i,2)
      dx(3) = -dfitf2(i,3)
      dx(4) = -dfitf2(i,4)
      dx(5) = -dfitf2(i,5)
      dx(6) = -dfitf2(i,6)
      dx(7) = -dfitf2(i,7)
      dx(8) = -dfitf2(i,8)
      dx(9) = -dfitf2(i,9)
      end
