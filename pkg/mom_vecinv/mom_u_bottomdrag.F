C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_u_bottomdrag.F,v 1.3 2001/07/13 13:49:05 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_BOTTOMDRAG(
     I        bi,bj,k,
     I        uFld, KE, KappaRU,
     O        uDragTerms,
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
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscFac
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1
      _RL rdrckp1,maskDown

C-  No-slip BCs impose a drag at bottom
      Kp1=min(k+1,Nr)
      rdrckp1=recip_drC(kp1)
      IF (k.EQ.Nr) rdrckp1=recip_drF(k)
      viscFac=1.
      IF (.NOT. no_slip_bottom) viscFac=0.
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        maskDown=_maskW(i,j,kp1,bi,bj)
        IF (k.EQ.Nr) maskDown=0.
        uDragTerms(i,j)=
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *( 
     &      2.*KappaRU(i,j,kp1)*rkFac*rdrckp1*viscFac
     &     + bottomDragLinear
     &    )*(1.-maskDown)*uFld(i,j)
        IF ( (KE(i,j)+KE(i-1,j)) .NE. 0. ) THEN
         uDragTerms(i,j)=uDragTerms(i,j)
     &    -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &    *bottomDragQuadratic*sqrt(KE(i,j)+KE(i-1,j))
     &    *(1.-maskDown)*uFld(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
