C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_ptracer.F,v 1.1 2005/10/10 05:53:48 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_PTRACER( 
     I                            bi, bj, K, iTracer,
     U                            pFld,
     I                            myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_PLOC                                      |
C     | In this routine the open boundary values are applied     |
C     | that have been calculated in OBCS_CALC. If you want to   |
C     | have true homogeneous v.Neumann conditions, comment in   |
C     | the lines that start with CML. However, this is very     |
C     | unflexible and immediately applied to ALL tracers, even  |
C     | if you have defined ALLOW_OBCS_PRESCRIBE. If you want    |
C     | to mixed the true v.Neumann conditions with prescribing  |
C     | You would need add code here to discriminate between     |
C     | different tracers.                                       |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#ifdef ALLOW_PTRACERS.h
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     == Routine Arguments ==
      INTEGER bi,bj,K,iTracer
      _RL pFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#if ( defined ALLOW_OBCS && defined ALLOW_PTRACERS )

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
        pFld(I,J_obc)=OBNptr(I,K,bi,bj,iTracer)*obc_mask
CML        pFld(I,J_obc)=pFld(I,J_obc-1)*obc_mask
       ENDIF
#endif
C Southern boundary
#ifdef ALLOW_OBCS_SOUTH
       J_obc = OB_Js(I,bi,bj)
       IF (J_obc.NE.0) THEN
        obc_mask = _maskS(I,J_obc+1,K,bi,bj)
        pFld(I,J_obc)=OBSptr(I,K,bi,bj,iTracer)*obc_mask
CML        pFld(I,J_obc)=pFld(I,J_obc+1)*obc_mask
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
        pFld(I_obc,J)=OBEptr(J,K,bi,bj,iTracer)*obc_mask
CML        pFld(I_obc,J)=pFld(I_obc-1,J)*obc_mask
       ENDIF
#endif
C Western boundary
#ifdef ALLOW_OBCS_WEST
       I_obc=OB_Iw(J,bi,bj)
       IF (I_obc.NE.0) THEN
        obc_mask = _maskW(I_obc+1,J,K,bi,bj)
        pFld(I_obc,J)=OBWptr(J,K,bi,bj,iTracer)*obc_mask
CML        pFld(I_obc,J)=pFld(I_obc+1,J)*obc_mask
       ENDIF
#endif
      ENDDO

#endif /* ALLOW_OBCS and ALLOW_PTRACERS */
      RETURN
      END
