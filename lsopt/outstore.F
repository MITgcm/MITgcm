      subroutine outstore( n, fc, gnorm0, m, jmin, jmax )
c
c         arguments
c
      integer n, m, jmin, jmax
      double precision    fc, gnorm0
c----
c
      common /xxstore/ itape, ntape, isize
      integer itape, ntape, isize

      open(  itape
     $     , file   = 'OPWARMI'
     $     , access = 'sequential'
     $     , form   = 'formatted'  )
      rewind (itape)

      write( itape, * ) n, fc, gnorm0, isize, m, jmin, jmax

      close(itape)

      return
      end
