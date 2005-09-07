
      subroutine lswri( isize, phniter, NN, xx, gg, lphprint )

c     ==================================================================
c     SUBROUTINE lswri
c     ==================================================================
c
c     o writes formatted output of updated controls and 
c       adjoint variables
c
c     o started: Patrick Heimbach, MIT/EAPS
c                01-Apr-1999
c
c     ==================================================================
c     SUBROUTINE lswri
c     ==================================================================

      implicit none

C     ========= Routine arguments ======================================

      INTEGER   isize, phniter, NN
      real*4 xx(NN), gg(NN)
cph      DOUBLE PRECISION      xx(NN), gg(NN)
      LOGICAL   lphprint

cph      CHARACTER yitnum*3
c

C     ========= Local variables ========================================


C=======================================================================

c      if (lphprint) 
c     &   print *, 'pathei: formatted output of xx, gg, in lswri: ',
c     &   phniter
c
c      open(95,
c     &     FILE='PH_X.dat',
c     &     FORM='UNFORMATTED',
c     &     ACCESS='DIRECT',
c     &     RECL=NN*isize)
c      write(95,REC=phniter+1) xx
c      close(95)
c
c      open(95,
c     &     FILE='PH_G.dat',
c     &     FORM='UNFORMATTED',
c     &     ACCESS='DIRECT',
c     &     RECL=NN*isize)
c      write(95,REC=phniter+1) gg
c      close(95)

      end
