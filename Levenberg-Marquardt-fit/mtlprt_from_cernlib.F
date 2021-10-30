*
* $Id: mtlprt_from_cernlib.F 5819 2012-02-06 11:05:33Z dveberic $
*
* $Log: mtlprt.F,v $
* Revision 1.1.1.1  1996/04/01 15:02:52  mclareni
* Mathlib gen
*
*
      SUBROUTINE MTLPRT(NAME,ERC,TEXT)
      CHARACTER*(*) NAME,ERC,TEXT
      LOGICAL LMF,LRF

      IF(ERC(5:6).NE.'.0') THEN
        CALL MTLMTR(ERC,MLG,LMF,LRF)
      ELSE
        LMF=.TRUE.
        LRF=.FALSE.
      ENDIF
      IF(LMF) THEN
        LT=LENOCC(TEXT)
        IF(MLG .LT. 1) WRITE(  *,100) ERC(1:4),NAME,ERC,TEXT(1:LT)
        IF(MLG .GE. 1) WRITE(MLG,100) ERC(1:4),NAME,ERC,TEXT(1:LT)
      ENDIF
      IF(.NOT.LRF) CALL ABEND
      RETURN
100   FORMAT(7X,'***** CERN ',A,1X,A,' ERROR ',A,': ',A)
      END
