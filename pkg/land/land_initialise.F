C $Header: /u/gcmpack/MITgcm/pkg/land/Attic/land_initialise.F,v 1.1 2003/06/12 17:54:22 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_INITIALISE
C     !INTERFACE:
      SUBROUTINE LAND_INITIALISE( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_INITIALISE
C     | o Read Land package parameters (from file data.land)
C     | o Initialize Land package variables
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE

C     == Global variables ===

C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==       
C     myThid ::  Number of this instance
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C     == Local Variables == 
C     msgBuf      :: Informational/error meesage buffer
C     i,j,k,bi,bj :: loop indices
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,k,bi,bj

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    for now, Land package works only with AIM package:
      IF ( .NOT.useAIM ) THEN
        WRITE(msgBuf,'(A)')
     &  'LAND_INITIALISE: land pkg only implemented for AIM physics'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(A)')
     &  'DO NOT use Land (useLand=T) without AIM (useAIM=F, data.pkg)' 
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_INITIALISE'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Read LAND parameters (from file data.land):
      CALL LAND_READPARMS( myThid )

C--   Initialize Land package variables

C-    Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,land_nLev
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           land_groundT(i,j,k,bi,bj) = 0. _d 0
           land_groundW(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
           land_runOff(i,j,bi,bj)  = 0. _d 0
           land_HeatFLx(i,j,bi,bj) = 0. _d 0
           land_Pr_m_Ev(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C-     end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_LAND */

      RETURN
      END
