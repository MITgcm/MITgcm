C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_ocean.F,v 1.3 2013/05/02 20:03:12 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif /* ALLOW_SEAICE */

CBOP
C     !ROUTINE: THSICE_GET_OCEAN
C     !INTERFACE:
      SUBROUTINE THSICE_GET_OCEAN(
     I                        bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_GET_OCEAN
C     | o Get mixed layer properties from ocean main
C     |   variables (surface level) and store them
C     |   into this package local mixed-layer arrays
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
c#include "THSICE_PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif /* ALLOW_SEAICE */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi, bj    :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
      INTEGER ks
#ifdef ALLOW_SEAICE
      _RL uRel1, uRel2, vRel1, vRel2
#endif /* ALLOW_SEAICE */

C--     Mixed layer thickness: take the 1rst layer
        ks = 1
#ifdef NONLIN_FRSURF
        IF ( staggerTimeStep .AND. nonlinFreeSurf.GT.0 ) THEN
         IF ( select_rStar.GT.0 ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             hOceMxL(i,j,bi,bj) = drF(ks)*h0FacC(i,j,ks,bi,bj)
     &                                   *rStarFacC(i,j,bi,bj)
           ENDDO
          ENDDO
         ELSE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( kSurfC(i,j,bi,bj).EQ.1 ) THEN
             hOceMxL(i,j,bi,bj) = drF(ks)*hFac_surfC(i,j,bi,bj)
            ELSE
             hOceMxL(i,j,bi,bj) = drF(ks)*hFacC(i,j,ks,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ELSE
#else /* ndef NONLIN_FRSURF */
        IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             hOceMxL(i,j,bi,bj) = drF(ks)*hFacC(i,j,ks,bi,bj)
           ENDDO
          ENDDO
        ENDIF

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          tOceMxL(i,j,bi,bj) = theta(i,j,ks,bi,bj)
          sOceMxL(i,j,bi,bj) = salt (i,j,ks,bi,bj)
          v2ocMxL(i,j,bi,bj) = 0. _d 0
          icFrwAtm(i,j,bi,bj) = 0. _d 0
          icFlxAtm(i,j,bi,bj) = 0. _d 0
          icFlxSW (i,j,bi,bj) = 0. _d 0
          siceAlb (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        IF ( .NOT.useSEAICE ) THEN
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx-1
           v2ocMxL(i,j,bi,bj) =
     &              ( uVel(i,j,ks,bi,bj) * uVel(i,j,ks,bi,bj)
     &              + uVel(i+1,j,ks,bi,bj)*uVel(i+1,j,ks,bi,bj)
     &              + vVel(i,j+1,ks,bi,bj)*vVel(i,j+1,ks,bi,bj)
     &              + vVel(i,j,ks,bi,bj) * vVel(i,j,ks,bi,bj)
     &              )*0.5 _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_SEAICE
        ELSE
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx-1
           uRel1 = uVel( i, j,ks,bi,bj)-uIce( i, j,bi,bj)
           uRel2 = uVel(i+1,j,ks,bi,bj)-uIce(i+1,j,bi,bj)
           vRel1 = vVel(i, j, ks,bi,bj)-vIce(i, j, bi,bj)
           vRel2 = vVel(i,j+1,ks,bi,bj)-vIce(i,j+1,bi,bj)
           v2ocMxL(i,j,bi,bj) =
     &              ( ( uRel1*uRel1 + uRel2*uRel2 )
     &              + ( vRel1*vRel1 + vRel2*vRel2 )
     &              )*0.5 _d 0
          ENDDO
         ENDDO
#endif /* ALLOW_SEAICE */
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif  /*ALLOW_THSICE*/

      RETURN
      END
