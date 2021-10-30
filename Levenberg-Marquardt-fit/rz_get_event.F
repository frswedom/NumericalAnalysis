      subroutine rz_get_event(ievent,ierr)
* 
*   ********************************************************************
*   *                                                                  *
*$$ *   subroutine patrec_get_event  reads one event using DSPACK I/O  *
*   *                                                                  *
*   *   Ryszard Zybert    Feb  7 15:35:57 1994                         *
*   ********************************************************************
* 

      integer ktype, ierr, ievent

c read one record (hopefully an event, but check it)

100   continue
      call dsnclr(1)
      call fdsin(1,ktype,ierr)
      if( ierr.eq.2 ) then
        write(*,'(''eof'')')
        return
      else if( ierr.ne.0 ) then
        write(*,*) 'Read error: ', ierr
        go to 100
      else
        if( ktype.eq.1) then
          ievent = ievent + 1
          write(*,*)'Read event ', ievent
        else
          write(*,*)'Read record type: ', ktype
          go to 100
        endif
      endif

      end
