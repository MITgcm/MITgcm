C $Header: /u/gcmpack/MITgcm/model/src/Attic/apply_obcs3.F,v 1.2 2000/06/29 18:28:20 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R APPLY_OBCS                                           |
C     | o Calculate the gradient of the surface pressure         |
C     \==========================================================/
      SUBROUTINE APPLY_OBCS3( bi, bj, K, Kup, rTrans, rVel,
     I                        myThid )
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#endif

C     == Routine Arguments ==
      INTEGER bi,bj,K,Kup
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rVel    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER I,J,Kp1,I_obc,J_obc
      _RL wMaskFac,obc_mask

#ifdef ALLOW_NONHYDROSTATIC

      Kp1=K+1
      wMaskFac=1.
      IF (Kp1.GT.Nr) THEN
       Kp1=Nr
       wMaskFac=0.
      ENDIF

C     Set model variables to OB values on North/South Boundaries
      DO I=1-Olx,sNx+Olx
C Northern boundary
       J_obc=OB_Jn(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc,K,bi,bj)
        rVel(I,J_obc,Kup)=OBNw(I,K,bi,bj) * obc_mask
        rTrans(I,J_obc) = rVel(I,J_obc,Kup) *
     &      _rA(i,J_obc,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
        wVel(I,J_obc,K,bi,bj) = rVel(I,J_obc,Kup)
#endif
       ENDIF
C Southern boundary
       J_obc=OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        rVel(I,J_obc,Kup)=OBSw(I,K,bi,bj) * obc_mask
        rTrans(I,J_obc) = rVel(I,J_obc,Kup) *
     &      _rA(i,J_obc,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
        wVel(I,J_obc,K,bi,bj) = rVel(I,J_obc,Kup)
#endif
       ENDIF
      ENDDO

C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc=OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        rVel(I_obc,J,Kup)=OBEw(J,K,bi,bj) * obc_mask
        rTrans(I_obc,J) = rVel(I_obc,J,Kup) *
     &      _rA(I_obc,j,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
        wVel(I_obc,J,K,bi,bj) = rVel(I_obc,J,Kup)
#endif
       ENDIF
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        rVel(I_obc,J,Kup)=OBWw(J,K,bi,bj) * obc_mask
        rTrans(I_obc,J) = rVel(I_obc,J,Kup) *
     &      _rA(I_obc,j,bi,bj)
#ifdef ALLOW_NONHYDROSTATIC
        wVel(I_obc,J,K,bi,bj) = rVel(I_obc,J,Kup)
#endif
       ENDIF
      ENDDO

#endif

#endif
      RETURN
      END
