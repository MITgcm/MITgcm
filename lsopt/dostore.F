      subroutine dostore( n, x, store, j )
c
c         arguments
c
      integer n, j
      double precision    x(n)
cgf      real*4 x(n) 
      logical store

      common /xxstore/ itape, ntape, isize
      integer itape, ntape, isize

cph(
      print *, 'pathei in dostore ', 
     &   store, n, ntape, j
cph)
      if (store) then
        write( ntape, rec=j ) x
      else
        read(  ntape, rec=j ) x
      endif

      return
      end
