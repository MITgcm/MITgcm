C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_export_saltfx.F,v 1.1 2004/05/21 19:59:38 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_EXPORT_SALTFX( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_EXPORT_SALTFX                             |
C     | o Routine for exporting atmos. saltFlux from thSIce pkg  |
C     |   to coupling layer.                                     |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMCPL.h"
#include "ATMIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     recipAvT :: Temp. for holding reciprocal of averaging period.
C     I,J,K,II :: Loop counters
C     bi, bj   ::
      _RL recipAvT
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER II
      INTEGER bi
      INTEGER bj

C     Convert time integrated heatflux to mean value ready for
C     export.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( ABS(saltFxTime(bi,bj) -1. _d 0).GT. 1. _d -12
     &     .AND. saltFxTime(bi,bj) .NE. 0. ) THEN 
         recipAvT = 1. _d 0/saltFxTime(bi,bj)
         DO J=1,sNy
          DO I=1,sNx
           iceSaltFlx(I,J,bi,bj) = iceSaltFlx(I,J,bi,bj)*recipAvT
          ENDDO
         ENDDO
         WRITE(errorMessageUnit,*)'ATM_EXPORT_SALTFX: 1-CountTime=',
     &                             bi,bj,1.-saltFxTime(bi,bj)
        ENDIF
       ENDDO
      ENDDO

C     o Send freshwater flux to coupling layer.
      CALL COMPSEND_R8TILES( atmSaltFxName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, iceSaltFlx )

C     Reset fresh water flux to start accumulating again.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        saltFxTime(bi,bj) = 0.
        DO J=1,sNy
         DO I=1,sNx
          iceSaltFlx(I,J,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
