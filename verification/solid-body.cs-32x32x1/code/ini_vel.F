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
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_VEL 
      INTEGER myThid

C     == Local variables ==
C     bi,bj,i,j,k - Loop counters
      INTEGER bi,bj,i,j,k
      INTEGER ip1,jp1
      _RL psi,omegaprime,fac

      psi(i,j,bi,bj)=fac*fCoriG(i,j,bi,bj)

C--   Initialise velocity fields to zero
C
C     If you want to specify an analytic initial state for the flow
C     field then customize the following section of code.
C     It is, however, often easier to generate initial conditions
C     off-line and read them from input files...
C
      omegaprime=38.60328935834681d0/rSphere
      fac=-(rSphere**2)*omegaprime/(2.d0*Omega)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          jp1=MIN(j+1,sNy+Oly)
          DO i=1-Olx,sNx+Olx
           ip1=MIN(i+1,sNx+Olx)
           uVel(i,j,k,bi,bj)=0.
     &      +(psi(i,j,bi,bj)-psi(i,jp1,bi,bj))*recip_dyg(i,j,bi,bj)
           vVel(i,j,k,bi,bj)=0.
     &      +(psi(ip1,j,bi,bj)-psi(i,j,bi,bj))*recip_dxg(i,j,bi,bj)
           wVel(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Read an initial state for each component if required
      IF (uVelInitFile .NE. ' ') THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( uVelInitFile, ' ', uVel, 0, myThid )
       _END_MASTER(myThid)
c      _EXCH_XYZ_RL(uVel  , myThid )
      ENDIF

      IF (vVelInitFile .NE. ' ') THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( vVelInitFile, ' ', vVel, 0, myThid )
       _END_MASTER(myThid)
c      _EXCH_XYZ_RL(vVel  , myThid )
      ENDIF

c     IF (uVelInitFile .NE. ' ' .OR. vVelInitFile .NE. ' ') THEN
       CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
c     ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           uVel(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
           vVel(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
