C $Header: /u/gcmpack/MITgcm/model/src/integrate_for_w.F,v 1.7 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INTEGRATE_FOR_W
C     !INTERFACE:
      SUBROUTINE INTEGRATE_FOR_W( 
     I        bi,bj,k,uFld,vFld,
     O        wFld,
     I        myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_COMMON_FACTORS                            
C     | o Integrate for vertical velocity.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld, vFld :: Zonal and meridional flow
C     wFld       :: Vertical flow
      INTEGER bi,bj,k
      _RL  uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     uTrans, vTrans :: Temps. for volume transports
      INTEGER i,j
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C--   Calculate velocity field "volume transports" through
C     tracer cell faces.
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          uTrans(i,j) = uFld(i,j,k,bi,bj)*
     &     _dyG(i,j,bi,bj)
     &     *drF(k)*_hFacW(i,j,k,bi,bj)
          vTrans(i,j) = vFld(i,j,k,bi,bj)*
     &     _dxG(i,j,bi,bj)
     &     *drF(k)*_hFacS(i,j,k,bi,bj)
        ENDDO
      ENDDO

C--   Calculate vertical "volume transport" through face k
C     between tracer cell k-1 & k
      IF (rigidLid) THEN
C-  o Rigid-Lid case: zero at lower and upper boundaries
        IF (k.eq.1) THEN
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx-1
             wFld(i,j,k,bi,bj) = 0.
           ENDDO
          ENDDO
        ELSEIF (k.eq.Nr) THEN
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx-1
             wFld(i,j,k,bi,bj) = 
     &                -( uTrans(i+1,j)-uTrans(i,j)
     &                  +vTrans(i,j+1)-vTrans(i,j)
     &                 )*recip_rA(i,j,bi,bj) 
     &          *maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
           ENDDO
          ENDDO
        ELSE
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx-1
             wFld(i,j,k,bi,bj) = 
     &        ( wFld(i,j,k+1,bi,bj)
     &                -( uTrans(i+1,j)-uTrans(i,j)
     &                  +vTrans(i,j+1)-vTrans(i,j)
     &                 )*recip_rA(i,j,bi,bj) 
     &         )*maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
           ENDDO
          ENDDO
        ENDIF
      ELSE
C-  o Linear Free Surface case: 
C      non zero at surface ; zero under-ground and at r_lower boundary
        IF (k.eq.Nr) THEN
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx-1
             wFld(i,j,k,bi,bj) = 
     &                -( uTrans(i+1,j)-uTrans(i,j)
     &                  +vTrans(i,j+1)-vTrans(i,j)
     &                 )*recip_rA(i,j,bi,bj) 
     &          *maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
        ELSE
          DO j=1-Oly,sNy+Oly-1
           DO i=1-Olx,sNx+Olx-1
             wFld(i,j,k,bi,bj) = 
     &        ( wFld(i,j,k+1,bi,bj)
     &                -( uTrans(i+1,j)-uTrans(i,j)
     &                  +vTrans(i,j+1)-vTrans(i,j)
     &                 )*recip_rA(i,j,bi,bj) 
     &         )*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
        ENDIF
C-  endif - rigid-lid / linear Free-Surf.
      ENDIF

      RETURN
      END
