C $Header: /u/gcmpack/MITgcm/model/src/Attic/apply_obcs1.F,v 1.5 2000/06/29 18:26:37 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE APPLY_OBCS1( bi, bj, K,
     I                        myThid )
C     /==========================================================\
C     | S/R APPLY_OBCS1                                          |
C     | o                                                        |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#include "DYNVARS.h"

C     == Routine Arguments ==
      INTEGER bi,bj,K
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
        uVel(I,J_obc,K,bi,bj)=OBNu(I,K,bi,bj)*obc_mask
     &                              *_maskW(I,J_obc,K,bi,bj)
        vVel(I,J_obc,K,bi,bj)=OBNv(I,K,bi,bj)*obc_mask
        theta(I,J_obc,K,bi,bj)=OBNt(I,K,bi,bj)*obc_mask
        vVel(I,J_obc+1,K,bi,bj)=vVel(I,J_obc,K,bi,bj)
       ENDIF
C Southern boundary
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
       obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        uVel(I,J_obc,K,bi,bj)=OBSu(I,K,bi,bj)*obc_mask
     &                              *_maskW(I,J_obc,K,bi,bj)
        vVel(I,J_obc+1,K,bi,bj)=OBSv(I,K,bi,bj)*obc_mask
        theta(I,J_obc,K,bi,bj)=OBSt(I,K,bi,bj)*obc_mask
        vVel(I,J_obc,K,bi,bj)=vVel(I,J_obc+1,K,bi,bj)
       ENDIF
      ENDDO

C     Set model variables to OB values on East/West Boundaries
      DO J=1-Oly,sNy+Oly
C Eastern boundary
       I_obc = OB_Ie(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc,J,K,bi,bj)
        uVel(I_obc,J,K,bi,bj)=OBEu(J,K,bi,bj)*obc_mask
        vVel(I_obc,J,K,bi,bj)=OBEv(J,K,bi,bj)*obc_mask
     &                              *_maskS(I_obc,J,K,bi,bj)
        theta(I_obc,J,K,bi,bj)=OBEt(J,K,bi,bj)*obc_mask
        uVel(I_obc+1,J,K,bi,bj)=uVel(I_obc,J,K,bi,bj)
       ENDIF
C Western boundary
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        uVel(I_obc+1,J,K,bi,bj)=OBWu(J,K,bi,bj) * obc_mask
        vVel(I_obc,J,K,bi,bj)=OBWv(J,K,bi,bj) * obc_mask
     &                         *_maskS(I_obc,J,K,bi,bj)
        theta(I_obc,J,K,bi,bj)=OBWt(J,K,bi,bj) * obc_mask
        uVel(I_obc,J,K,bi,bj)=uVel(I_obc+1,J,K,bi,bj)
       ENDIF
      ENDDO

#endif
      RETURN
      END
