      subroutine dostore( n, x, store, j )
c
c         arguments
c
      integer n, i, j
      integer*8 rectmp, tmp1, tmp2
      double precision    x(n)
      real*4 tmpx(n) 
      logical store

      common /xxstore/ itape, ntape, isize
      integer itape, ntape, isize

cph(
cph      print *, 'pathei in dostore ', 
cph     &   store, n, ntape, j
cph)

      tmp1 = n
      tmp2 = isize
      rectmp = tmp1*tmp2

      if (store) then
        do i = 1, n
          tmpx(i) = x(i)
        enddo
        write( ntape, POS=(j-1)*rectmp +1) tmpx
      else
        read(  ntape, POS=(j-1)*rectmp +1) tmpx
	do i = 1, n
          x(i) = tmpx(i)
        enddo
      endif

      return
      end
