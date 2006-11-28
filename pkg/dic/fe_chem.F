C $Header: /u/gcmpack/MITgcm/pkg/dic/fe_chem.F,v 1.9 2006/11/28 21:16:03 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

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
#include "DIC_BIOTIC.h"

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
         DO k=1,nR
          IF(hFacC(i,j,k,bi,bj) .gt. 0.0)THEN

C   Ligand,FeL,Fe calculation
            
              lig=(-ligand_stab*fe (i,j,k,bi,bj)+
     &              ligand_stab*ligand_tot-1
     &             +((ligand_stab*fe (i,j,k,bi,bj)
     &                -ligand_stab*ligand_tot+1)**2+4
     &                *ligand_stab*ligand_tot)**0.5)/(2*ligand_stab)
 
              FeL = ligand_tot-lig
              freefe(i,j,k,bi,bj) = fe (i,j,k,bi,bj)-FeL
#ifdef MINFE
#ifdef AD_SAFE
              thx=freefe(i,j,k,bi,bj)
              thy=freefemax
              theps=1.d-8
              freefe(i,j,k,bi,bj) =
     &                 ( 1.d0 - tanh((thx-thy)/theps) ) * thx/2 +
     &                 ( 1.d0 + tanh((thx-thy)/theps) ) * thy/2

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
