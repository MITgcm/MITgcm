#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PSURF
C     !INTERFACE:
      SUBROUTINE INI_PSURF( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PSURF                                     |
C     | o Set model initial free-surface height/pressure.        |
C     *==========================================================*
C     | There are several options for setting the initial        |
C     | surface displacement (r unit) field.                     |
C     |  1. Inline code                                          |
C     |  2. Two-dimensional data from a file.                    |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_CD_CODE
# include "CD_CODE_VARS.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi,bj  :: tiles indices
C     i, j   :: Loop counters
      INTEGER bi, bj
      INTEGER i, j
      _RL omegaPrime, snFac, psFac
CEOP

C--   Initialise surface position anomaly to zero
      omegaPrime = 80. _d 0 / rSphere
c     psFac = -(rSphere**2)*omegaPrime*(Omega+omegaPrime)
C     previous expression above was missing one "half" factor:
      psfac = -(rSphere*rSphere)*omegaPrime
     &                          *( Omega + omegaPrime*0.5 _d 0 )
      snFac = 1. _d 0 / (4. _d 0*Omega*Omega)
C     recip_Bo is constant here, Z-coord: =1/gBaro
C                                P-coord: if uniformLin_PhiSurf=T, =rhoConst
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          etaN(i,j,bi,bj) = 0. _d 0
     &     + psFac*( snFac*fCori(i,j,bi,bj)*fCori(i,j,bi,bj)
     &             - 1. _d 0 / 3. _d 0 )*recip_Bo(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Read an initial state
      IF ( pSurfInitFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RL( pSurfInitFile, ' ', etaN, 0, myThid )
C      fill the overlap (+ BARRIER)
       _EXCH_XY_RL(etaN, myThid)
      ENDIF

#ifdef ALLOW_CD_CODE
C--   By default, initialize etaNm1 with etaN :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          etaNm1(i,j,bi,bj) = etaN(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

#ifdef EXACT_CONSERV
C--   By default, initialize etaH with etaN :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          etaH(i,j,bi,bj) = etaN(i,j,bi,bj)
          etaHnm1(i,j,bi,bj) = etaN(i,j,bi,bj)
          dEtaHdt(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* EXACT_CONSERV */

#ifdef ALLOW_SHELFICE
      IF ( useShelfIce .AND. usingZCoords ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi0surf(i,j,bi,bj) = phi0surf(i,j,bi,bj)
     &          + shelficeLoadAnomaly(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_SHELFICE */

      RETURN
      END
