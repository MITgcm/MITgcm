C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_apply_tloc.F,v 1.1 2001/09/10 01:22:48 adcroft Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_TLOC( bi, bj, K,
     U                           tFld,
     I                           myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_TLOC                                      |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj,K
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I,J,I_obc,J_obc
      _RL obc_mask

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
       J_obc = OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        tFld(I,J_obc)=OBNt(I,K,bi,bj)*obc_mask
       ENDIF
C Southern boundary
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        tFld(I,J_obc)=OBSt(I,K,bi,bj)*obc_mask
       ENDIF
      ENDDO

C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc = OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        tFld(I_obc,J)=OBEt(J,K,bi,bj)*obc_mask
       ENDIF
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        tFld(I_obc,J)=OBWt(J,K,bi,bj) * obc_mask
       ENDIF
      ENDDO

#endif
      RETURN
      END
