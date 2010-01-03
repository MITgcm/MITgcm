C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_ini_vars.F,v 1.6 2010/01/03 19:26:54 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_INI_VARS
C     !INTERFACE:
      SUBROUTINE CPL_INI_VARS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CPL_INI_VARS
C     | o Initialisation routine for arrays that are used to
C     |   buffer data that interfaces to the coupling layer.
C     | - Oceanic version -
C     *==========================================================*
C     | For now this routine sets these arrays to the right
C     | state for an initial start-up. Eventually it should
C     | include some way to load this data from non-volatile
C     | external storage if they are needed during a restart.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNCPL.h"
#include "CPL_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id Number
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE

C     !LOCAL VARIABLES:
C     I,J,bi,bj :: Loop counters
      INTEGER I,J,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myByLo(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C.        Export fields
          ocMxlD2cpl(I,J,bi,bj) = 0.
          SSTocn2cpl(I,J,bi,bj) = 0.
          SSSocn2cpl(I,J,bi,bj) = 0.
          vSqocn2cpl(I,J,bi,bj) = 0.
          fluxCO2cpl(I,J,bi,bj) = 0.
C-        Import fields
          atmSLPr   (I,J,bi,bj) = 0.
          HeatFlux  (I,J,bi,bj) = 0.
          qShortWave(I,J,bi,bj) = 0.
c         qLatent   (I,J,bi,bj) = 0.
c         qSensible (I,J,bi,bj) = 0.
c         qLongWave (I,J,bi,bj) = 0.
c         uVelGround(I,J,bi,bj) = 0.
c         vVelGround(I,J,bi,bj) = 0.
          tauX      (I,J,bi,bj) = 0.
          tauY      (I,J,bi,bj) = 0.
          FWflux    (I,J,bi,bj) = 0.
          iceSaltFlx(I,J,bi,bj) = 0.
          seaIceMass(I,J,bi,bj) = 0.
          airCO2    (I,J,bi,bj) = 0.
          surfWSpeed(I,J,bi,bj) = 0.
          fracIce   (I,J,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( startTime.NE.baseTime .OR. nIter0.NE.0
     &                   .OR. pickupSuff .NE. ' ' ) THEN
       CALL OCN_CPL_READ_PICKUP( nIter0, myThid )
      ENDIF

#ifdef ALLOW_TIMEAVE
C     Initialise diagnostic counters ( these are cleared on model start
C      i.e. not loaded from history file for now ).
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET( SLPtave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( HFtave ,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( QSWtave,  1, bi, bj, myThid )
c        CALL TIMEAVE_RESET( QLTtave,  1, bi, bj, myThid )
c        CALL TIMEAVE_RESET( QSNtave,  1, bi, bj, myThid )
c        CALL TIMEAVE_RESET( QLWtave,  1, bi, bj, myThid )
c        CALL TIMEAVE_RESET( UGtave ,  1, bi, bj, myThid )
c        CALL TIMEAVE_RESET( VGtave ,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( TXtave ,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( TYtave ,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( FWtave ,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( SFxtave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( SICtave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( MXLtave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( SSTtave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( SSStave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( vSqtave,  1, bi, bj, myThid )
         CALL TIMEAVE_RESET( aCO2tave, 1, bi, bj, myThid )
         CALL TIMEAVE_RESET( sWSpdtave,1, bi, bj, myThid )
         CALL TIMEAVE_RESET( iceftave, 1, bi, bj, myThid )
         CALL TIMEAVE_RESET( fCO2tave, 1, bi, bj, myThid )

         CPL_timeAve(bi,bj) = 0.
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

#endif /* COMPONENT_MODULE */

      RETURN
      END
