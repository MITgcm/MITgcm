C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_diffusion.F,v 1.3 2006/05/06 19:13:51 heimbach Exp $
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
cphCADJ STORE heff  = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */


C--   This would be the natural way to do diffusion (explicitly)
C     For now we stick to the modified Eulerian time step
CML      DO bj=myByLo(myThid),myByHi(myThid)
CML       DO bi=myBxLo(myThid),myBxHi(myThid)
CML        CALL GAD_DIFF_X(bi,bj,k,xA,diff1,localT,df,myThid)
CML        DO j=1-Oly,sNy+Oly
CML         DO i=1-Olx,sNx+Olx
CML          fZon(i,j) = fZon(i,j) + df(i,j)
CML         ENDDO
CML        ENDDO
CML        CALL GAD_DIFF_Y(bi,bj,k,yA,diff1,localT,df,myThid)
CML        DO j=1-Oly,sNy+Oly
CML         DO i=1-Olx,sNx+Olx
CML          fMer(i,j) = fMer(i,j) + df(i,j)
CML         ENDDO
CML        ENDDO
CMLC--   Divergence of fluxes: update scalar field
CML        DO j=1-Oly,sNy+Oly-1
CML         DO i=1-Olx,sNx+Olx-1
CML          HEFF(i,j,1,bi,bj)=HEFF(i,j,1,bi,bj) + DELTT *
CML     &         maskC(i,j,kSurface,bi,bj)*recip_rA(i,j,bi,bj)
CML     &       *( (fZon(i+1,j)-fZon(i,j))
CML     &         +(fMer(i,j+1)-fMer(i,j))
CML     &                       )
CML     &         )
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML      ENDDO

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
