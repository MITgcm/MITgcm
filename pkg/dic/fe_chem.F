C $Header: /u/gcmpack/MITgcm/pkg/dic/fe_chem.F,v 1.15 2008/06/23 22:26:32 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE Fe_CHEM(
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           fe, freefe,
     I           myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE Fe_chem                                       |
C     |                                                          |
C     | o Calculate L,FeL,Fe concentration                       |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"

C     == Routine arguments ==                
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL  freefe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myIter,myThid
#ifdef AD_SAFE
      _RL thx, thy, theps
#endif
CEndOfInterface

#ifdef ALLOW_FE

      INTEGER I,J,K
      _RL  lig, FeL
      _RL  tmpfe

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
CC
CC ADAPTED FROM PAYAL
CC
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc

C ligand balance in surface layer
C in surface layer

       DO j=jmin,jmax
        DO i=imin,imax
         DO k=1,Nr
          IF (hFacC(i,j,k,bi,bj) .GT. 0. _d 0) THEN

#ifdef DIC_NO_NEG
              tmpfe=max(0. _d 0 , fe (i,j,k,bi,bj))
#else
              tmpfe=fe (i,j,k,bi,bj)
#endif
            
C   Ligand,FeL,Fe calculation
              lig=(-ligand_stab*tmpfe +
     &              ligand_stab*ligand_tot-1. _d 0
     &             +((ligand_stab*tmpfe 
     &                -ligand_stab*ligand_tot+1. _d 0)**2
     &               +4. _d 0*ligand_stab*ligand_tot)**0.5 _d 0
     &            )/(2. _d 0*ligand_stab)
 
              FeL = ligand_tot-lig
              if (tmpfe.ne.0. _d 0) then
                freefe(i,j,k,bi,bj) = tmpfe -FeL
              else
                freefe(i,j,k,bi,bj) = 0. _d 0
              endif
#ifdef MINFE
#ifdef AD_SAFE
              thx=freefe(i,j,k,bi,bj)
              thy=freefemax
              theps=1. _d -8
              freefe(i,j,k,bi,bj) =
     &                 ( 1. _d 0 - tanh((thx-thy)/theps) ) * thx/2.+
     &                 ( 1. _d 0 + tanh((thx-thy)/theps) ) * thy/2.

#else
              freefe(i,j,k,bi,bj) = min(freefe(i,j,k,bi,bj),freefemax)
#endif
              fe(i,j,k,bi,bj) = FeL+freefe(i,j,k,bi,bj)
#endif
          END IF  
         ENDDO
        ENDDO
       ENDDO    
c
#endif 
        RETURN
        END
