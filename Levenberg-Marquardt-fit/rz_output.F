      subroutine rz_output
* 
*   ********************************************************************
*   *                                                                  *
*$$ *   subroutine patrec_output  writes event output                   *
*   *                                                                  *
*   *   Ryszard Zybert    Feb 16 13:18:38 1994                         *
*   ********************************************************************
* 
      character*80 dsexna
      integer nn, ierr
 
      if( dsexna('$DSPACK_OUTPUT').eq.' ' ) return
      call fsoutv(1,1,nn,ierr)
 
      end
