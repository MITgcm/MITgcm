C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_adv_wv.F,v 1.5 2003/01/24 17:15:59 jmc Exp $
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
#ifdef NONLIN_FRSURF
#include "SURFACE.h"
#endif

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
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

#ifdef NONLIN_FRSURF
      IF ( k.EQ.Nr+1 .AND.
     &     useRealFreshWaterFlux .AND.
     &     buoyancyRelation .EQ. 'OCEANICP' ) THEN
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx,sNx+Olx
         advectiveFluxWV(i,j) = convertEmP2rUnit*
     &      ( PmEpR(i, j ,bi,bj)*rA(i, j ,bi,bj)
     &       +PmEpR(i,j-1,bi,bj)*rA(i,j-1,bi,bj)
     &      )*0.5 _d 0 * vFld(i,j,k-1,bi,bj)
        ENDDO
       ENDDO

      ELSEIF ( k.GT.Nr .OR. (k.EQ.1.AND.rigidLid) ) THEN
#else /* NONLIN_FRSURF */
      IF ( k.GT.Nr .OR.
     &    (k.EQ.1.AND.rigidLid) ) THEN
#endif /* NONLIN_FRSURF */
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
c       advectiveFluxWV(i,j) =
c    &     0.5*(
c    &        wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
c    &       +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
c    &         )*vFld(i,j,k,bi,bj)
        tmpFld(i,j) = 0.5 _d 0*(
     &        wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &       +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj) )
        advectiveFluxWV(i,j) = tmpFld(i,j)*vFld(i,j,k,bi,bj)
       ENDDO
      ENDDO

      ELSE

C     Vertical advection - interior ; assume vFld & wFld are masked 
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
c       advectiveFluxWV(i,j) =
c    &    0.25*(
c    &        wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
c    &       +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
c    &         )*( vFld(i,j,k,bi,bj)+vFld(i,j,k-1,bi,bj) )
        tmpFld(i,j) = 0.5 _d 0*(
     &        wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &       +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj) )
        advectiveFluxWV(i,j) = tmpFld(i,j)*0.5 _d 0
     &          *( vFld(i,j,k,bi,bj)+vFld(i,j,k-1,bi,bj) )
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
