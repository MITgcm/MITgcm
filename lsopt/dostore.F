      subroutine dostore( n, x, store, j )
c
c         arguments
c
      integer n, i, j
      double precision    x(n)
      real*4 tmpx(n) 
      logical store

      common /xxstore/ itape, ntape, isize
      integer itape, ntape, isize

cph(
cph      print *, 'pathei in dostore ', 
cph     &   store, n, ntape, j
cph)

      if (store) then
        do i = 1, n
          tmpx(i) = x(i)
        enddo
        write( ntape, rec=j ) tmpx
      else
        read(  ntape, rec=j ) tmpx
	do i = 1, n
          x(i) = tmpx(i)
        enddo
      endif

      return
      end
