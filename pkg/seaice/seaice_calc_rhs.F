C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_rhs.F,v 1.1 2006/06/05 22:46:50 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_calc_rhs( 
     I     uc, vc, iceFld, iceMask, xA, yA,
     I     advScheme, tracerIdentity, bi, bj, 
     O     gFld,
     I     myTime, myIter, myThid )
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
      _RL uc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS xA     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER advScheme
      INTEGER tracerIdentity
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j - Loop counters

      INTEGER i, j, k
      _RL locFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      k = 1
C--   Make a local copy of the scalar field
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        locFld(i,j) = iceFld(i,j,bi,bj)
        fZon  (i,j) = 0. _d 0
        fMer  (i,j) = 0. _d 0
       ENDDO
      ENDDO
C--   Tendency due to advection
      CALL SEAICE_ADVECTION(
     I     advScheme, tracerIdentity, 
     I     uc, vc, iceFld,
     O     gfld,
     I     bi, bj, myTime, myIter, myThid)
      IF ( diff1 .GT. 0. _d 0 ) THEN
C--   Tendency due to horizontal diffusion
C--   X-direction
       CALL GAD_DIFF_X(bi,bj,k,xA,diff1,locFld,fZon,myThid)
C--   Y-direction
       CALL GAD_DIFF_Y(bi,bj,k,yA,diff1,locFld,fMer,myThid)
C--   Divergence of fluxes: update scalar field
C--   Ugly:
C--   Apply factor min(DX,DY) to effectively end up with approximately 
C--   the same diffusion coefficient as in subroutine ADVECT.
C--   One day, I would like to rewrite the second order central difference 
C--   part, too, so that the value of DIFF1 has the same meaning as,
C--   say, diffKhT
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         gFld(i,j,bi,bj)= gFld(i,j,bi,bj) 
     &        + iceMask(i,j,bi,bj)*recip_rA(i,j,bi,bj)
     &        *( (fZon(i+1,j)-fZon(i,j))
     &         + (fMer(i,j+1)-fMer(i,j)) )
     &        *MIN( _dxF(I,J,bi,bj), _dyF(I,J,bi,bj))
        ENDDO
       ENDDO
C     endif do horizontal diffusion
      ENDIF

      RETURN
      END
