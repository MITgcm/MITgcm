      subroutine dostore( n, x, store, j )
c
c         arguments
c
      integer n, j
      double precision    x(n)
      logical store

      common /xxstore/ itape, ntape, isize
      integer itape, ntape, isize

      if (store) then
        write( ntape, rec=j ) x
      else
        read(  ntape, rec=j ) x
      endif

      return
      end
