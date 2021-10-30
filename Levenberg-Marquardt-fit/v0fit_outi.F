      subroutine v0fit_outi
* 
*   ********************************************************************
*   *                                                                  *
*$$ *   subroutine patrec_out  initialises patrec output               *
*   *                                                                  *
*   *   Ryszard Zybert    Feb 16 13:19:12 1994                         *
*   *   altered for v0fit DEC     1995 HLC                             *
*   ********************************************************************
* 
#     include "veefit.inc"
      character*256 dsexnal
      integer ierr, nn, ntot

c open the output file 

      if( dsexnal('$DSPACK_OUTPUT').eq.' ' ) return
      call fsopou(1,'$DSPACK_OUTPUT',' ',ierr)
c      call fscopy(1,1,nn,ierr)
      call fscopy(1,1,nn)


c flag TRACKS and HITS for output and also some constants

      call fsmark(1,'fvtpar_t',ntot,ierr)

c write out the header with definitions and parameters

      call fsoutv(1,2,nn,ierr)

      end
