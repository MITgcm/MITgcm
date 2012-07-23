C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_v_bottomdrag.F,v 1.12 2012/07/23 15:52:48 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_BOTTOMDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_BOTTOMDRAG(
     I        bi,bj,k,
     I        vFld, KE, KappaRV,
     O        vDragTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag due to friction and the no-slip condition at bottom:
C \begin{equation*}
C G^v_{drag} = - \frac{1}{\Delta r_f} ( r_b + C_D |v| + \frac{2}{\Delta r_c} ) v
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_BOTTOMDRAG_CONTROL
# include "DYNVARS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vFld                 :: meridional flow
C  KE                   :: Kinetic energy
C  KappaRV              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vDragTerms           :: drag term
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j,kDown,kLowF,kBottom
      _RL viscFac, dragFac
      _RL recDrC
      _RL recDrF_bot(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C-  No-slip BCs impose a drag at bottom
      viscFac = 0.
      IF (no_slip_bottom) viscFac = 2.
      IF ( usingZCoords ) THEN
       kBottom = Nr
       kDown   = MIN(k+1,Nr)
       kLowF   = kDown
c      dragFac = mass2rUnit*rhoConst
c      dragFac = wUnit2rVel(k+1)
       dragFac = 1. _d 0
      ELSE
       kBottom = 1
       kDown   = MAX(k-1,1)
       kLowF   = k
       dragFac = mass2rUnit*rhoConst
c      dragFac = wUnit2rVel(k)
      ENDIF
      IF ( k.EQ.kBottom ) THEN
       recDrC = recip_drF(k)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          recDrF_bot(i,j) = _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
        ENDDO
       ENDDO
      ELSE
       recDrC = recip_drC(kLowF)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          recDrF_bot(i,j) = _recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &                    * ( 1. _d 0 -_maskS(i,j,kDown,bi,bj) )
        ENDDO
       ENDDO
      ENDIF

C-    viscous (no-slip BC) and linear bottom drag
      DO j=1-OLy+1,sNy+OLy-1
       DO i=1-OLx,sNx+OLx-1
         vDragTerms(i,j)=
     &    - recDrF_bot(i,j)
     &    *(
     &       KappaRV(i,j,kLowF)*recDrC*viscFac
#ifdef ALLOW_BOTTOMDRAG_CONTROL
     &     + bottomdragfld(i,j,bi,bj)*dragFac
#endif
     &     + bottomDragLinear*dragFac
     &     )*vFld(i,j)
       ENDDO
      ENDDO

C-    Add quadratic bottom drag
      IF ( bottomDragQuadratic.NE.0. _d 0 ) THEN
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         IF ( (KE(i,j)+KE(i,j-1)) .GT. 0. ) THEN
          vDragTerms(i,j) = vDragTerms(i,j)
     &     - recDrF_bot(i,j)
     &      *bottomDragQuadratic*SQRT(KE(i,j)+KE(i,j-1))*dragFac
     &      *vFld(i,j)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(vDragTerms,'VBotDrag',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
