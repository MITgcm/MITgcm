
c     ==================================================================
c
c     prgopti.F: Routines for doing an off-line optimization after the
c                ECCO forward and adjoint model have been run.
c
c     main     - Driver routine.
c     opti     - Mid-level routine to do the spin up and spin down.
c     optimum  - Routine that calls the minimization.
c
c     Documentation:
c
c     The collection of these routines originated mainly from Ralf
c     Giering. Patrick Heimbach improved and corrected considerable
c     parts of the original code. Christian Eckert contributed the
c     interface to the ECCO release of the MITgcmUV in order to get
c     the offline version going.
c
c     How to use the off-line optimization.
c
c       Doing an off-line optimization means that one alternately
c       calls the adjoint model and the optimization routines.
c
c       The adjoint model yields at iteration i the cost function
c       value and the gradient of the cost function with respect to
c       the control variables. The optimization routines then use
c       this information to reduce the cost function and give a
c       new estimate of the control variables which can then be used
c       in the next cycle to yield a new cost function and the
c       corresponding gradient.
c
c     started:  Ralf Giering      (lsoptv1)
c
c               Patrick Heimbach  heimbach@mit.edu  28-Feb-2000
c
c               - Corrected and restructured the original lsoptv1
c                 code.
c
c               Christian Eckert  eckert@mit.edu    15-Feb-2000
c
c               - Off-line capability and some cosmetic changes
c                 of the optimization wrapper.
c
c     changed:
c
c     ==================================================================


      program optim_main

c     ==================================================================
c     PROGRAM optim_main
c     ==================================================================
c
c     o Driver routine for the ECCO optimization package.
c
c     started: Christian Eckert eckert@mit.edu 15-Feb-2000
c
c     changed: Christian Eckert eckert@mit.edu 10-Mar-2000
c
c              - Added ECCO layout.
c
c     ==================================================================
c     SUBROUTINE
c     ==================================================================

      implicit none

c     == global variables ==

#include "blas1.h"

c     == routine arguments ==

c     == local variables ==

      integer nn

c     == end of interface ==

c--   Headline.
      print*
      print*,' =================================================='
      print*,' Large Scale Optimization with off-line capability.'
      print*,' =================================================='
      print*
      print*,'                Version 2.1.0'
      print*

c--   Get the number of control variables.
      call optim_numbmod( nn )

cph(
      print *, 'pathei: vor optim_sub'
cph)
c--   Call the subroutine.
      call optim_sub( nn )

c--   Succesful termination.
      print*
      print*,' ======================================'
      print*,' Large Scale Optimization run finished.'
      print*,' ======================================'
      print*

      end

CStartOfInterface
      INTEGER FUNCTION IFNBLNK( string )
C     /==========================================================\
C     | FUNCTION IFNBLNK                                         |
C     | o Find first non-blank in character string.              |
C     \==========================================================/
      IMPLICIT NONE
C
      CHARACTER*(*) string
CEndOfInterface
C
      INTEGER L, LS
C
      LS     = LEN(string)
      IFNBLNK = 0
      DO 10 L = 1, LS
       IF ( string(L:L) .EQ. ' ' ) GOTO 10
        IFNBLNK = L
        GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

CStartOfInterface
      INTEGER FUNCTION ILNBLNK( string )
C     /==========================================================\
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     \==========================================================/
      IMPLICIT NONE
      CHARACTER*(*) string
CEndOfInterface
      INTEGER L, LS
C
      LS      = LEN(string)
      ILNBLNK = LS
      DO 10 L = LS, 1, -1
        IF ( string(L:L) .EQ. ' ' ) GOTO 10
         ILNBLNK = L
         GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

