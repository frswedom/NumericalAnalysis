        subroutine gaussj_v0fit(a,n,np,b,m,mp,ierr)
        implicit none
***
* Linear equation solution by Gauss-Jordan elimination
* W.H.Press et al - NUMERICAL RECIPES, Cambridge UP 86, page 28
* IERR = 1 if singular matrix, 0 otherwise
***
        integer i,j,k,m,n,np,mp,irow,icol,NMAX
        integer ierr,l,ll
      parameter ( NMAX = 100 )
      double precision a(np,np),b(np,mp)
        double precision big,dum,pivinv
      integer ipiv(NMAX), indxr(NMAX), indxc(NMAX)
*
      ierr = 0
      do  11  j = 1, n
        ipiv(j) = 0
 11     continue
*
      do  22  i = 1, n
        big = 0.0
        do  13  j = 1, n
          if( ipiv(j) .ne. 1 ) then
            do  12  k = 1, n
              if( ipiv(k) .eq. 0 ) then
                if( abs(a(j,k)) .ge. big ) then
                  big = abs(a(j,k))
                  irow = j
                  icol = k
                end if
              elseif( ipiv(k) .gt. 1) then
                ierr = 1
                return
              endif
 12          continue
          endif
 13      continue
        ipiv(icol) = ipiv(icol) + 1
*
        if ( irow .NE. icol ) then
          do  14  l = 1, n
            dum = a(irow,l)
            a(irow,l) = a(icol,l)
            a(icol,l) = dum
 14        continue
          do  15  l = 1, m
            dum = b(irow,l)
            b(irow,l) = b(icol,l)
            b(icol,l) = dum
 15        continue
        endif
*
        indxr(i) = irow
        indxc(i) = icol
        if( abs(a(icol,icol)) .lt. 1.D-20 ) then
          ierr = 1
          return
        endif
        pivinv = 1.0/a(icol,icol)
        a(icol,icol) = 1.0
        do  16  l = 1, n
          a(icol,l) = a(icol,l)*pivinv
 16       continue
        do  17  l = 1, m
          b(icol,l) = b(icol,l)*pivinv
 17      continue
*
        do  21  ll = 1, n
          if( ll .ne. icol ) then
            dum = a(ll,icol)
            a(ll,icol) = 0.0
            do  18  l = 1, n
* It had underflows when multiplying by DUM below - this should help
*             if( abs(a(icol,l)) .lt. 1.0D-20 .and.
*    >            abs(dum) .lt. 1.0D-15    ) then
*               ierr = 1
*               return
*             endif
              a(ll,l) = a(ll,l) - a(icol,l)*dum
 18          continue
            do  19  l = 1, m
              b(ll,l) = b(ll,l) - b(icol,l)*dum
 19          continue
          endif
 21      continue
 22    continue
*
      do  24 l = n, 1, -1
        if( indxr(l) .ne. indxc(l) ) then
          do  23  k = 1,n
            dum = a(k,indxr(l))
            a(k,indxr(l)) = a(k,indxc(l))
            a(k,indxc(l)) = dum
 23        continue
        endif
 24    continue
      end
