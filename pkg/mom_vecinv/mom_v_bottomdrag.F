C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_v_bottomdrag.F,v 1.3 2001/07/13 13:49:05 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_BOTTOMDRAG(
     I        bi,bj,k,
     I        vFld, KE, KappaRV,
     O        vDragTerms,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate bottom drag terms for U eqn
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscFac
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1
      _RL rdrckp1,maskDown

C-  No-slip BCs impose a drag at bottom
      Kp1=min(k+1,Nr)
      rdrckp1=recip_drC(kp1)
      viscFac=1.
      IF (.NOT. no_slip_bottom) viscFac=0.
      IF (k.EQ.Nr) rdrckp1=recip_drF(k)
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        maskDown=_maskS(i,j,kp1,bi,bj)
        IF (k.EQ.Nr) maskDown=0.
        vDragTerms(i,j)=
     &   -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &   *( 
     &      2.*KappaRV(i,j,kp1)*rkFac*rdrckp1*viscFac
     &     + bottomDragLinear
     &    )*(1.-maskDown)*vFld(i,j)
        IF ( (KE(i,j)+KE(i,j-1)) .NE. 0. ) THEN
         vDragTerms(i,j)=vDragTerms(i,j)
     &    -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    *bottomDragQuadratic*sqrt(KE(i,j)+KE(i,j-1))
     &    *(1.-maskDown)*vFld(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
