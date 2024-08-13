#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_VEL
C     !INTERFACE:
      SUBROUTINE INI_VEL( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE INI_VEL
C     | o Initialize flow field (either to zero or from input files)
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j,k  :: Loop counters
C     bi,bj  :: tile indices
      INTEGER i, j, k
      INTEGER bi, bj
      INTEGER ip1, jp1
      _RL psi, omegaprime, fac
CEOP

C     internal function:
      psi(i,j,bi,bj) = fac*fCoriG(i,j,bi,bj)

C--   Initialise velocity fields to zero
C
C     If you want to specify an analytic initial state for the flow
C     field then customize the following section of code.
C     It is, however, often easier to generate initial conditions
C     off-line and read them from input files...
C
C     this relative rotation corresponds to a 12.day rotation period:
      omegaprime = 38.60328935834681 _d 0 / rSphere
      fac = -(rSphere*rSphere)*omegaprime/(2. _d 0*Omega)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          jp1 = MIN(j+1,sNy+OLy)
          DO i=1-OLx,sNx+OLx
           ip1 = MIN(i+1,sNx+OLx)
           uVel(i,j,k,bi,bj) = 0. _d 0
     &      + (psi(i,j,bi,bj)-psi(i,jp1,bi,bj))*recip_dyG(i,j,bi,bj)
           vVel(i,j,k,bi,bj) = 0. _d 0
     &      + (psi(ip1,j,bi,bj)-psi(i,j,bi,bj))*recip_dxG(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c     IF ( uVelInitFile .NE. ' ' .OR. vVelInitFile .NE. ' ' ) THEN
C     Read an initial state for each component if required
       IF ( uVelInitFile .NE. ' ' )
     & CALL READ_FLD_XYZ_RL( uVelInitFile, ' ', uVel, 0, myThid )

       IF ( vVelInitFile .NE. ' ' )
     & CALL READ_FLD_XYZ_RL( vVelInitFile, ' ', vVel, 0, myThid )

       CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
c     ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uVel(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
           vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
