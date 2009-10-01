C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_apply_sloc.F,v 1.4 2009/10/01 20:54:34 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_SLOC( bi, bj, K,
     U                           sFld,
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
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I,J,I_obc,J_obc
      _RL obc_mask

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
#ifdef ALLOW_OBCS_NORTH
       J_obc = OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        DO J = J_obc, J_obc+Oly
         sFld(I,J)=OBNs(I,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
#endif
C Southern boundary
#ifdef ALLOW_OBCS_SOUTH
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        DO J = J_obc-Oly, J_obc
         sFld(I,J)=OBSs(I,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
#endif
      ENDDO

C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
#ifdef ALLOW_OBCS_EAST
       I_obc = OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        DO I = I_obc, I_obc+Olx
         sFld(I,J)=OBEs(J,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
#endif
C Western boundary
#ifdef ALLOW_OBCS_WEST
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        DO I = I_obc-Olx, I_obc
         sFld(I,J)=OBWs(J,K,bi,bj)*obc_mask
        ENDDO
       ENDIF
#endif
      ENDDO

#endif
      RETURN
      END
