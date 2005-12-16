C $Header: /u/gcmpack/MITgcm/pkg/dic/fe_chem.F,v 1.8 2005/12/16 21:07:53 stephd Exp $
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
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C     == Routine arguments ==                
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL  freefe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myIter,myThid
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
              freefe(i,j,k,bi,bj) = min(freefe(i,j,k,bi,bj),freefemax)
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
