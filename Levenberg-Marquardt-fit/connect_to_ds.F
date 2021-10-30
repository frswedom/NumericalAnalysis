      subroutine connect_to_ds_v0fit(name)
* 
*   ********************************************************************
*   *                                                                  *
*$$ *   subroutine connect  connect to dspack, open input file         *
*   *                                                                  *
*   *   Ryszard Zybert    Feb 18 13:35:09 1994                         *
*   *   HLC 7.11.95 altered to decide on which tpc to use              *
*   *   HLC 10.12.95 altered idet determination so can run on both     *
*   *         vtpcs  together                                          *
*   *   TAY 30.10.96 Altered to accept global tracks.
*   ********************************************************************
* 
      character*(*) name
      character*256 dsexnal
      character*256 arg
c      integer narg, ierr, i , ktype, iv, idet, selected
      integer narg, ierr, i , ktype, idet, selected
      logical global, select

      common /DET/ idet, selected, global, select

      narg = iargc()

c connect to dspack
#ifdef SHINE_CLIENT_IS_FUNCTION
      call dsinit(0, 'v0fit', 6, 0, 0, ierr)
#else
      call dsinit(1, 'v0fit', 6, 0, 0, ierr)
#endif
      if (ierr.ne.0) then
        write(*,*) "Can't start DSHACK"
      endif

      global = .false.
      select = .false.
      idet = 10

      i = 1
 1    continue
      call getarg(i,arg)
      i = i+1
      if( arg.eq.'-i') then
         call getarg(i,arg)
         i = i + 1
         call ds_setenv('DSPACK_INPUT',arg)
      else if( arg.eq.'-o') then
         call getarg(i,arg)
         i = i + 1
         call ds_setenv('DSPACK_OUTPUT',arg)
      else if( arg.eq.'-n') then
         call getarg(i,arg)
         i = i + 1
         call ds_setenv('EVENTS',arg)
      else if( arg.eq.'-d') then
         call getarg(i,arg)

         if ( index(arg,'1') .gt. 0) then
            idet = 1
         endif

         if ( index(arg,'2') .gt. 0) then
            if (idet.eq.1) then
               idet = 0
            else
               idet = 2
            endif
         endif

         if ((index(arg,'g').gt.0).or.
     >       (index(arg,'a').gt.0)) then
            idet = 0
            global = .true.
         endif
         i = i + 1
 
      else if( arg.eq.'-v') then
         call getarg(i,arg)
         call ds_setenv('SEL',arg)
         select = .true.
         i = i + 1

      endif

      if (i.le.narg ) go to 1


      if (idet.gt.2) then
         write(6,*) ' I dont know which TPCs to use. Exiting'
         call ds_exit(100)
      endif
 

c open input file

      arg = dsexnal('$DSPACK_INPUT')
      if( arg.eq.' ' .or. arg.eq.'NONE' ) return
      call fsopin(1,'$DSPACK_INPUT',' ',ierr)

      if( ierr.ne.0 ) then
        write(*,*) 'Cannot open input file'
        call ds_exit(100)
      endif

c read the header

      call fdsin(1,ktype,ierr)
      if( ktype.ne.0 ) then
        write(*,*)'No definition block, ierr:', ierr
        call ds_exit(100)
      endif

c open .d file

      call ds_ascii('$V0FIND_HOME/inc/v0find.h',.false.,ierr)
      if( ierr.ne.0 ) then
       write(6,*) 'Error opening v0find.h file.'
       call ds_exit(100)
      endif

      call ds_ascii('$V0FIT_HOME/inc/veefit.h',.false.,ierr)
      if( ierr.ne.0 ) then
         write(6,*) 'Error opening .h file is $V0FIT_HOME set?'
         call ds_exit(100)
      endif
c 999  end
      end

