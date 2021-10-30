***
*   Routine to fit vee's. Starting programme
***
c------------------------------------------------
#ifdef SHINE_CLIENT_IS_FUNCTION
      subroutine main_v0fit()
#else
      program f_v0fit
#endif
c------------------------------------------------
        implicit none

#include "veefit.inc"
#include "v0find_vfit.inc"
#include "na49_event_str.gfortran.inc"

        character*256 dsexnal
        integer idsget_ds,ids_add_entry,ids_size
        integer ivsver,iverrmat,iv_fvtpar,iv_cuts,ivfirst,iv_all
        integer ifvtpar_p,isvertex_p,first_vee_p,ierrmat_p,n1,n2
        integer iv0_pointa_p,iv0_pointb_p,ivee_p,first_p,ievent
        integer all_p,nall, selected
        integer ierr,j,nent_first,idet
        integer no_vt1_vees, no_vt2_vees, no_vt0_vees
        integer i,ii,nh1,nh2,numhit,nent1,nev,n_events,isize,no_vees
        real pmult,dsexnu

        logical stand_alone, global, select
        type(vertex_t) svertex
        type(v0_first_vee) first_v0
        type(covar9x9_t) errmat(*)
        type(track_t) daughter
        type(fvtpar_t) fvtpar
*       record /avertex_v0_t/ veecomp
        type(v0_cuts_vt2_t) cuts(0:2)
        type(tpc_t) all_veefit
        pointer(svertex_p,svertex)
        pointer(errmat_p,errmat)
*       pointer(ivee_p,veecomp)
        pointer(daughter_p,daughter)
        pointer(first_vee_p,first_v0)
        pointer(all_p,all_veefit)

        common /FVTPARS/ ifvtpar_p
        common /FVTRCD/ numhit,nh1,nh2,iv0_pointa_p,
     >                  iv0_pointb_p,pmult
        common /DET/ idet, selected, global, select
        data ivsver,iverrmat,iv_fvtpar,ivfirst /4*0/
        data iv_cuts/0/
        data nev /1*1/

***
*  Connect to dspack via hepdb
***
        call connect_to_ds_v0fit('v0fit')

***
* Open input file,mark output and find no. of events if in stand
*   alone mode
***
        stand_alone = dsexnal('$DSPACK_INPUT').ne.' '
        if (stand_alone) then
           nev = int(dsexnu('$EVENTS',ierr))
           call v0fit_outi
        endif

        if (select) selected = int(dsexnu('$SEL',ierr))
        if (ierr .ne. 0) then
           write(6,*) 'V0FIT : Error, dsexnu ierr = ',ierr
c 10        call ds_exit(10)
           call ds_exit(10)
        endif

        do n_events=1,nev

           if (stand_alone) then
              call rz_get_event(ievent,ierr)
              if (ierr .eq. 2) goto 900
           endif

***
*   call up no. of vees found and book err. matrix
***

      ivfirst = 0
           first_vee_p = idsget_ds('v0_first_vee',ivfirst,nent_first)
           if (first_vee_p .eq. 0) then
             write(6,*) 'V0FIT : Warning, idsget v0_first_vee not there'
c 20          call ds_exit(0)
             call ds_exit(0)
           endif
           no_vees=first_v0%vt1vees+first_v0%vt2vees+first_v0%vt0vees
           isize = ids_size('-',ivfirst)

      ivsver = 0
           svertex_p = idsget_ds('vertex',ivsver,nent1)
           if (svertex_p .eq. 0) then
              write(6,*) 'V0FIT : Error, idsget_ds vertex'
c 21           call ds_exit(21)
              call ds_exit(21)
           endif

           errmat_p = ids_add_entry('covar_v0_fit',iverrmat,
     >         no_vees,no_vees,first_p,ierr)
           if (ierr .ne. 0) then
              write(6,*) 'V0FIT : Error, ids_add_entry ierr = ',ierr
c 30           call ds_exit(30)
              call ds_exit(30)
           endif

           call dsget('fvtpar_t',iv_fvtpar,1,nent1,fvtpar,ierr)
           if (ierr .ne. 0) then
              write(6,*) 'V0FIT : Error, dsget ierr=',ierr
c 40           call ds_exit(40)
              call ds_exit(40)
           endif

           ifvtpar_p = loc(fvtpar)
           call dsget('v0_cuts_vt0_t',iv_cuts,1,nent1,cuts(0),ierr)
           if (ierr .ne. 0) then
              write(6,*) 'V0FIT : Error, dsget ierr=',ierr
c 41           call ds_exit(41)
              call ds_exit(41)
           endif

      iv_all = 0
           all_p = idsget_ds('all',iv_all,nall)
           if (global) then
              if (all_p.eq.0) then
                 write(6,*) ' V0FIT: Error, idsget all'
c 22              call ds_exit(22)
                 call ds_exit(22)
              elseif (nall.eq.0) then
                write(6,*) ' V0FIT: Error, structure all has no entries'
c 50             call ds_exit(50)
                call ds_exit(50)
              endif
           else
              if (all_p.ne.0) then
                 write(6,*) 'V0FIT : Error, global tracks used.'
c 60              call ds_exit(60)
                 call ds_exit(60)
              endif
           endif

           pmult = 0.0

***
*   Loop over vees to be fitted, selecting only the set of vees
*   specified by the -v option.
***
           do 101 j=1,nent_first

              if (select) then

                 if (selected.eq.0) then
                    write(6,*)
     >              'V0FIT : Selected v0finder data set'//
     >              'must be greater than zero.'
c 70                 call ds_exit(70)
                    call ds_exit(70)
                 elseif (selected.gt.nent_first) then
                    write(6,*)
     >              'V0FIT : Only',nent_first,' v0finder'//
     >              ' data sets available.'
c 71                 call ds_exit(71)
                    call ds_exit(71)
                 endif

                 if (j .ne. selected) goto 200

              endif

              ii = 1
              no_vees = 0
              no_vt1_vees = first_v0%vt1vees
              no_vt2_vees = first_v0%vt2vees
              no_vt0_vees = first_v0%vt0vees
              if ((no_vt1_vees .ne. 0).and.(idet.lt.2)) then
                 no_vees = no_vt1_vees
                 no_vt1_vees = 0
                 svertex_p = first_v0%vt1_p
              elseif ((no_vt2_vees .ne. 0).and.(idet.ne.1)) then
                 no_vees = no_vt2_vees
                 no_vt2_vees = 0
                 svertex_p = first_v0%vt2_p
              elseif ((no_vt0_vees .ne. 0).and.(idet.eq.0)) then
                 no_vees = no_vt0_vees
                 no_vt0_vees = 0
                 svertex_p = first_v0%vt0_p
              endif

***
*  loop over vees and call fitting routines
***
 102          continue

              do 100 i=ii,no_vees

                 svertex%covar_p = loc(errmat(i))
                 ierrmat_p = loc(errmat(i))
                 isvertex_p = loc(svertex)
                 ivee_p = svertex%avertex_p
                 daughter_p = svertex%daughter_p
                 n1 = svertex%daughter_p
                 n2 = daughter%next_daughter_p

                 call vfit(n1,n2,isvertex_p,ierrmat_p)
                 call v0fit(n1,n2,ivee_p,cuts,isvertex_p,0)

                 svertex_p = svertex%next_p

 100          continue

              ii = no_vees + 1
              if ((no_vt1_vees .ne. 0).and.(idet.lt.2)) then
                 no_vees = no_vees + no_vt1_vees
                 no_vt1_vees = 0
                 svertex_p = first_v0%vt1_p
                 goto 102
              elseif ((no_vt2_vees .ne. 0).and.(idet.ne.1)) then
                 no_vees = no_vees + no_vt2_vees
                 no_vt2_vees = 0
                 svertex_p = first_v0%vt2_p
                 goto 102
              elseif ((no_vt0_vees .ne. 0).and.(idet.eq.0)) then
                 no_vees = no_vees + no_vt0_vees
                 no_vt0_vees = 0
                 svertex_p = first_v0%vt0_p
                 goto 102
              endif

 200          continue
              first_vee_p = first_vee_p + isize
 101       continue

c 800       if (stand_alone) then
           if (stand_alone) then
              call rz_output
           endif

        enddo

        call ds_resize('-',ivfirst,0,ierr)

 900    continue

c 999    end
        end














