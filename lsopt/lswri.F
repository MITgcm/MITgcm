
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

      if (lphprint) 
     &   print *, 'pathei: formatted output of xx, gg, in lswri: ',
     &   phniter
c
      open(95,
     &     FILE='PH_X.dat',
     &     FORM='UNFORMATTED',
     &     ACCESS='DIRECT',
     &     RECL=NN*isize)
      write(95,REC=phniter+1) xx
      close(95)
c
      open(95,
     &     FILE='PH_G.dat',
     &     FORM='UNFORMATTED',
     &     ACCESS='DIRECT',
     &     RECL=NN*isize)
      write(95,REC=phniter+1) gg
      close(95)

      end
