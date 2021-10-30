        subroutine covsrt_v0fit(covar,ncvm,ma,lista,mfit)
        implicit none
***
* Used by MRQMIN
* W.H.Press et al - NUMERICAL RECIPES, Cambridge UP 86, page 515
***
        integer i,j
        integer ncvm,ma,mfit
      double precision covar(ncvm,ncvm),swap
      integer lista(mfit)
*
      do  12  j = 1, ma-1
        do  11  i = j+1, ma
          covar(i,j) = 0.0
 11      continue
 12    continue
*
      do  14  i = 1, mfit-1
        do  13  j = i+1, mfit
          if( lista(j) .gt. lista(i) ) then
            covar(lista(j),lista(i)) = covar(i,j)
          else
            covar(lista(i),lista(j)) = covar(i,j)
          endif
 13      continue
 14    continue
*
      swap = covar(1,1)
      do  15  j = 1, ma
        covar(1,j) = covar(j,j)
        covar(j,j) = 0.0
 15    continue
*
      covar(lista(1),lista(1)) = swap
      do  16  j = 2, mfit
        covar(lista(j),lista(j)) = covar(1,j)
 16    continue
      do  18  j = 2, ma
        do  17  i = 1, j-1
          covar(i,j) = covar(j,i)
 17      continue
 18    continue
      end
