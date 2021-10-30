*
* $Id: datime_from_cernlib.F 5819 2012-02-06 11:05:33Z dveberic $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:37  mclareni
* Kernlib
*
*
      SUBROUTINE DATIME (ID,IT)
C
C CERN PROGLIB# Z007    DATIME  DUMMY   .VERSION KERNFOR  4.22  890913
C
C-    DUMMY FOR NON-ESSENTIAL ROUTINE STILL MISSING ON YOUR MACHINE

      COMMON /SLATE/ ISLATE(40)

      DO 24 J=1,6
   24 ISLATE(J) = 0

      ID = 790929
      IT = 1200
      RETURN
      END