C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_eta.F,v 1.1 2002/01/30 04:22:31 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_ETA( bi, bj,
     U                          etaFld,
     I                          myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_ETA                                       |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS
#ifdef NONLIN_FRSURF

C     == Local variables ==
      INTEGER I,J,I_obc,J_obc,K
      _RL obc_mask

      K = 1 
      IF (groundAtK1) K = Nr

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
       J_obc = OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        etaFld(I,J_obc,bi,bj)=OBNeta(I,bi,bj)*obc_mask
       ENDIF
C Southern boundary
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        etaFld(I,J_obc,bi,bj)=OBSeta(I,bi,bj)*obc_mask
       ENDIF
      ENDDO

C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc = OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        etaFld(I_obc,J,bi,bj)=OBEeta(J,bi,bj)*obc_mask
       ENDIF
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        etaFld(I_obc,J,bi,bj)=OBWeta(J,bi,bj)*obc_mask
       ENDIF
      ENDDO

#endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS */
      RETURN
      END
