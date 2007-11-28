C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_forcing_surf.F,v 1.2 2007/11/28 17:55:36 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_FORCING_SURF(
     I                            bi, bj, iMin, iMax, jMin, jMax,
     I                            myTime,myIter,myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SALT_PLUME_FORCING_SURF
C     | o saltPlume is the amount of salt rejected by ice while freezing;
C     |   it is here subtracted from surfaceForcingS and will be redistributed
C     |   to multiple vertical levels later on as per Duffy et al. (GRL 1999)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
C     bi,bj                :: tile indices
C     myTime               :: model time
C     myIter               :: time-step number
C     myThid               :: thread number
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
C     i,j                  :: loop indices
C     ks                   :: surface level index
      INTEGER i, j, ks

      IF ( usingPCoords ) THEN
         ks = Nr
      ELSE
         ks = 1
      ENDIF

      DO j = jMin, jMax
         DO i = iMin, iMax
            surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &           - saltPlumeFlux(i,j,bi,bj) * mass2rUnit
         ENDDO
      ENDDO
 
#endif /* ALLOW_SALT_PLUME */
    
      RETURN
      END
