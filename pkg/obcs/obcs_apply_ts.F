C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_ts.F,v 1.4 2004/12/03 16:47:30 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_TS( bi, bj, K,
     U                          tFld, sFld,
     I                          myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_TS                                        |
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
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I,J,I_obc,J_obc
      _RL obc_mask

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      DO I=1-Olx,sNx+Olx
C Northern boundary
       J_obc = OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        DO J = J_obc, J_obc+Oly
         tFld(I,J,K,bi,bj)=OBNt(I,K,bi,bj)*obc_mask
         sFld(I,J,K,bi,bj)=OBNs(I,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
      DO I=1-Olx,sNx+Olx
C Southern boundary
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        DO J = J_obc-Oly, J_obc
         tFld(I,J,K,bi,bj)=OBSt(I,K,bi,bj)*obc_mask
         sFld(I,J,K,bi,bj)=OBSs(I,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc = OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        DO I = I_obc, I_obc+Olx
         tFld(I,J,K,bi,bj)=OBEt(J,K,bi,bj)*obc_mask
         sFld(I,J,K,bi,bj)=OBEs(J,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_OBCS_WEST
      DO J=1-Oly,sNy+Oly
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        DO I = I_obc-Olx, I_obc
         tFld(I,J,K,bi,bj)=OBWt(J,K,bi,bj) * obc_mask
         sFld(I,J,K,bi,bj)=OBWs(J,K,bi,bj) * obc_mask
        ENDDO
       ENDIF
      ENDDO
#endif

#endif
      RETURN
      END
