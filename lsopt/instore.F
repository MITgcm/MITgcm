      subroutine instore( n, fc, gnorm0, is, m, jmin, jmax,
     &                    cold, ifail )
c
c         arguments
c
      integer n, is, m, jmin, jmax, ifail
      double precision    fc, gnorm0
      logical cold
      integer*8 rectmp, tmp1, tmp2
c----
c
      common /xxstore/ itape, ntape, isize
      integer itape, ntape, isize

      ifail = 0
      itape = 91
      ntape = 92
      isize = is
      cold  = .true.

      print *, 'pathei: in instore '

cc one record = two arrays of real*(isize)

      open(  itape
     $    , file   = 'OPWARMI'
     $    , status = 'unknown'
     $    , form   = 'formatted'
     $    , access = 'sequential'
     $    )

      read( itape, *, end=800, err=900 ) n, fc, gnorm0,
     $                                   isize, m, jmin, jmax
      cold  = .false.

 800  continue
      close(itape)

      print*
      write(*,'(a,i10,a,i2)')
     $        '  opening direct access with recl = ',n,' * ',isize
      print*

      tmp1 = n
      tmp2 = isize
      rectmp = tmp1*tmp2
cph(
      print *, 'pathei rectmp ', rectmp, n, isize
cph)

      open( ntape
     $    , file   = 'OPWARMD'
     $    , status = 'unknown'
     $    , form   = 'unformatted'
     $    , access = 'direct'
     $    , recl   = rectmp
     $    )

      return

 900  continue
      close(itape)
      ifail = 1

      return
      end
