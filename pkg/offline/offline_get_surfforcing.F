C $Header: /u/gcmpack/MITgcm/pkg/offline/Attic/offline_get_surfforcing.F,v 1.1 2013/07/04 22:59:18 jmc Exp $
C $Name:  $

#include "OFFLINE_OPTIONS.h"
#ifdef ALLOW_DIC
#include "DIC_OPTIONS.h"
#endif
#ifdef ALLOW_DARWIN
#include "DARWIN_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OFFLINE_GET_SURFFORCING
C     !INTERFACE:
      SUBROUTINE OFFLINE_GET_SURFFORCING( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OFFLINE_GET_SURFFORCING
C     | o Interpolate in time surface forcing fields that has
C     |   been loaded from file
C     *==========================================================*
C     | Note: Only used for pkg/dic when ALLOW_OLD_VIRTUALFLUX
C     |       is defined
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "OFFLINE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime  :: Simulation time
C     myIter  :: Simulation timestep number
C     myThid  :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OLD_VIRTUALFLUX
#if ( (defined ALLOW_DIC) || (defined ALLOW_DARWIN) )
C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j
      _RL aWght, bWght

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Interpolate surface forcing
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        bWght = offline_Wght(1,bi,bj)
        aWght = offline_Wght(2,bi,bj)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
c         surfaceForcingT(i,j,bi,bj) = bWght*hflx0(i,j,bi,bj)
c    &                               + aWght*hflx1(i,j,bi,bj)
c         surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
c    &                                *recip_Cp*mass2rUnit
          surfaceForcingS(i,j,bi,bj) = bWght*sflx0(i,j,bi,bj)
     &                               + aWght*sflx1(i,j,bi,bj)
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &                                *mass2rUnit
c         ICEM(i,j,bi,bj) = bWght*icem0(i,j,bi,bj)
c    &                    + aWght*icem1(i,j,bi,bj)
         ENDDO
        ENDDO
C--   end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_DIC or ALLOW_DARWIN */
#endif /* ALLOW_OLD_VIRTUALFLUX */

      RETURN
      END
