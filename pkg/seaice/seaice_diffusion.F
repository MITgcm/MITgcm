C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_diffusion.F,v 1.1 2006/02/16 10:41:48 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_DIFFUSION( 
     U     HEFF, 
     I     HEFFM, DELTT, myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE advect                                        |
C     | o Calculate ice advection                                |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"
CML#include "SEAICE_GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL     DELTT
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, bi, bj

      _RL DIFFA(1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff  = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */


C NOW DO DIFFUSION ON H(I,J,3)
C NOW CALCULATE DIFFUSION COEF ROUGHLY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DIFFA(I,J,bi,bj)=
     &         DIFF1*MIN( _dxF(I,J,bi,bj), _dyF(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL DIFFUS(HEFF,DIFFA,HEFFM,DELTT, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,1,bi,bj)=(HEFF(I,J,1,bi,bj)+HEFF(I,J,3,bi,bj))
     &                      *HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C NOW CALCULATE DIFFUSION COEF ROUGHLY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DIFFA(I,J,bi,bj)=
     &         -(MIN( _dxF(I,J,bi,bj),  _dyF(I,J,bi,bj)))**2/DELTT
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL DIFFUS(HEFF,DIFFA,HEFFM,DELTT, myThid)
 
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,1,bi,bj)=(HEFF(I,J,1,bi,bj)+HEFF(I,J,3,bi,bj))
     &                      *HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
