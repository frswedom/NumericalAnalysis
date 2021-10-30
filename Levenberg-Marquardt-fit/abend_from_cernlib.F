*
* $Id: abend_from_cernlib.F 5819 2012-02-06 11:05:33Z dveberic $
*
* $Log: abend.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:36  mclareni
* Kernlib
*
*
          SUBROUTINE ABEND
          IF(LGFILE .EQ. 0)  WRITE(*,1000)
          IF(LGFILE .NE. 0)  WRITE(LGFILE,1000)
          RETURN
1000      FORMAT(31H ABEND ROUTINE HAS BEEN CALLED.)
          END
