C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_uv_boundary.F,v 1.1 2007/10/28 21:38:21 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_UV_BOUNDARY

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_UV_BOUNDARY (
     I               bi,bj,k,
     I               uFld, vFld,
     O               uBnd, vBnd,
     I               myTime, myIter, myThid )

C !DESCRIPTION:
C Set velocity at a boundary for a momentum conserving advection
C  Note: really conserve momentum when "steps" (vertical plane)
C  or coastline (horizontal plane) are only 1 grid-point wide.

C !USES: ===============================================================
C     == Global variables ==
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj          :: tile indices
C  k              :: vertical level
C  uFld           :: zonal      velocity
C  vFld           :: meridional velocity
C  myTime         :: current time
C  myIter         :: current iteration number
C  myThid         :: My Thread Id. number
      INTEGER bi,bj
      INTEGER k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uBnd           :: zonal      velocity extended to boundaries
C  vBnd           :: meridional velocity extended to boundaries
      _RL uBnd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vBnd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef MOM_BOUNDARY_CONSERVE
C !LOCAL VARIABLES: ====================================================
C  i,j            :: loop indices
      INTEGER i,j
      INTEGER km1,kp1
      _RL maskM1, maskP1
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  aBndU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  aBndV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  aBndW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpVar
      LOGICAL useMomBndConserve
      PARAMETER ( useMomBndConserve = .TRUE. )
CEOP

C     Initialise output array
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uBnd(i,j) = uFld(i,j,k,bi,bj)
         vBnd(i,j) = vFld(i,j,k,bi,bj)
       ENDDO
      ENDDO

      IF ( useMomBndConserve ) THEN

C-    Initialise intermediate arrays:
        km1 = MAX( k-1, 1 )
        kp1 = MIN( k+1, Nr )
        maskM1 = 1.
        maskP1 = 1.
        IF ( k.EQ.1  ) maskM1 = 0.
        IF ( k.EQ.Nr ) maskP1 = 0.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          aBndU(i,j) = 0.
          aBndV(i,j) = 0.
          aBndW(i,j) = 0.
         ENDDO
        ENDDO

C-      Calculate Divergence in 3 directions:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uTrans(i,j) = uFld(i,j,k,bi,bj)
     &                * dyG(i,j,bi,bj)*deepFacC(k)
     &                * drF(k)*hFacW(i,j,k,bi,bj)*rhoFacC(k)
          vTrans(i,j) = vFld(i,j,k,bi,bj)
     &                * dxG(i,j,bi,bj)*deepFacC(k)
     &                * drF(k)*hFacS(i,j,k,bi,bj)*rhoFacC(k)
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          aBndU(i,j) = uTrans(i+1,j)-uTrans(i,j)
          aBndV(i,j) = vTrans(i,j+1)-vTrans(i,j)
          aBndW(i,j) = ABS(aBndU(i,j)+aBndV(i,j))
          aBndU(i,j) = ABS(aBndU(i,j))
          aBndV(i,j) = ABS(aBndV(i,j))
         ENDDO
        ENDDO
C-      Normalise by the sum:
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          tmpVar = aBndU(i,j)+aBndV(i,j)+aBndW(i,j)
          IF ( tmpVar.GT.0. ) THEN
            tmpVar = 1. _d 0 / tmpVar
            aBndU(i,j) = aBndU(i,j)*tmpVar
            aBndV(i,j) = aBndV(i,j)*tmpVar
            aBndW(i,j) = aBndW(i,j)*tmpVar
          ENDIF
         ENDDO
        ENDDO

C-      At a boundary, replace uFld,vFld by a weighted average
C       Note: multiply by 2 to cancel the 1/2 factor in advections S/R
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF (maskW(i,j,k,bi,bj).EQ.0.) THEN
C       Note: only 1 set of aBnd_U,V,W is non-zero (either i-1 or i)
C        and  only 1 uFld is non-zero (either i-1 or i+1)
C        and  only 1 uFld is non-zero (either k-1 or k+1)
            uBnd(i,j) = (
     &        (aBndU(i-1,j)+aBndU(i,j))
     &                     *(uFld(i-1,j,k,bi,bj)+uFld(i+1,j,k,bi,bj))
     &       +(aBndV(i-1,j)+aBndV(i,j))
     &                     *(uFld(i,j-1,k,bi,bj)+uFld(i,j+1,k,bi,bj))
     &       +(aBndW(i-1,j)+aBndW(i,j))
     &                     *(uFld(i,j,km1,bi,bj)*maskM1
     &                      +uFld(i,j,kp1,bi,bj)*maskP1)
     &                  )*2. _d 0
          ENDIF
          IF (maskS(i,j,k,bi,bj).EQ.0.) THEN
C       Note: only 1 set of aBnd_U,V,W is non-zero (either j-1 or j)
C        and  only 1 vFld is non-zero (either j-1 or j+1)
C        and  only 1 vFld is non-zero (either k-1 or k+1)
            vBnd(i,j) = (
     &        (aBndU(i,j-1)+aBndU(i,j))
     &                     *(vFld(i-1,j,k,bi,bj)+vFld(i+1,j,k,bi,bj))
     &       +(aBndV(i,j-1)+aBndV(i,j))
     &                     *(vFld(i,j-1,k,bi,bj)+vFld(i,j+1,k,bi,bj))
     &       +(aBndW(i,j-1)+aBndW(i,j))
     &                     *(vFld(i,j,km1,bi,bj)*maskM1
     &                      +vFld(i,j,kp1,bi,bj)*maskP1)
     &                  )*2. _d 0
          ENDIF
         ENDDO
        ENDDO
      ENDIF
#endif /* MOM_BOUNDARY_CONSERVE */

      RETURN
      END
