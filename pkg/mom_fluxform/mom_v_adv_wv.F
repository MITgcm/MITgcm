C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_adv_wv.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_ADV_WV

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_ADV_WV(
     I        bi,bj,k,
     I        vFld,wFld,
     O        advectiveFluxWV,
     I        myThid)

C !DESCRIPTION:
C Calculates the vertical advective flux of meridional momentum:
C \begin{equation*}
C F^r = \overline{W}^j \overline{v}^{k}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  rTrans               :: vertical transport
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  advectiveFluxWV      :: advective flux
      _RL advectiveFluxWV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

      IF ( k.GT.Nr .OR.
     &    (k.EQ.1.AND.rigidLid) ) THEN
C     Advective flux = 0  at k=Nr+1 ; = 0 at k=1 if rigid-lid

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        advectiveFluxWV(i,j) = 0.
       ENDDO
      ENDDO

      ELSEIF (k.EQ.1) THEN
C     (linear) Free-surface correction at k=1

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        advectiveFluxWV(i,j) =
     &     0.5*(
     &        wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &       +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &         )*vFld(i,j,k,bi,bj)
       ENDDO
      ENDDO

      ELSE

C     Vertical advection - interior ; assume vFld & wFld are masked 
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        advectiveFluxWV(i,j) =
     &    0.25*(
     &        wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &       +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &         )*( vFld(i,j,k,bi,bj)+vFld(i,j,k-1,bi,bj) )
       ENDDO
      ENDDO

      IF (.NOT. rigidLid) THEN
C     (linear) Free-surface correction at k>1
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx,sNx+Olx
          advectiveFluxWV(i,j) = advectiveFluxWV(i,j)
     &     +0.25*(
     &          wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)*
     &          (maskC(i,j,k,bi,bj) - maskC(i,j,k-1,bi,bj))
     &         +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)*
     &          (maskC(i,j-1,k,bi,bj)-maskC(i,j-1,k-1,bi,bj))
     &           )*vFld(i,j,k,bi,bj)
         ENDDO
        ENDDO
C- endif NOT rigidLid
      ENDIF

      ENDIF

      RETURN
      END
