C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_exact_eta.F,v 1.3 2001/08/27 18:43:46 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#undef DBUG_EXACT_ETA

      SUBROUTINE CALC_EXACT_ETA( bi,bj, uFld,vFld,
     I                           myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_EXACT_ETA                                |
C     | o Compute again the surface "r-anomaly" (eta) to satisfy |
C     |   exactly the convervation of the Total Volume           |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C     == Routine arguments ==
C     bi,bj   - tile index
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER bi,bj
      _RL uFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy) 

#ifdef EXACT_CONSERV
C     Local variables in common block
C     hDivFlow - Div. Barotropic Flow at current time_step [transport unit m3/s]
      COMMON /EXACT_ETA_LOCAL/ hDivFlow
      _RL hDivFlow(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

C     Local variables
      INTEGER i,j,k
      _RL uTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL vTrans(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

#ifdef DBUG_EXACT_ETA
C     sumArea - Work variable used to compute the total Domain Area
C     eta_sum1,2 - Eta mean value before and after this routine
C     eta_dif1,2,3 - Mean change of Eta: 1=before this call - iterNm1
C                                        2=this call
C                                        3=iterN-iterNm1
      _RL eta_tmp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL  sumArea, eta_sum1, eta_sum2, eta_dif1, eta_dif2, eta_dif3
      SAVE sumArea, eta_sum1, eta_sum2, eta_dif1, eta_dif2, eta_dif3
      _RL  recip_sumArea
      DATA recip_sumArea / 0. _d 0 / 
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Change of Eta <== -Divergence of The Barotropic Flow : Explicit part
      IF ( myTime.NE.StartTime ) THEN
        DO j=1,sNy
         DO i=1,sNx
#ifdef DBUG_EXACT_ETA
           eta_tmp(i,j) = etaN(i,j,bi,bj) 
#endif
           etaN(i,j,bi,bj) = etaNm1(i,j,bi,bj) 
     &      - (1. - implicDiv2Dflow)*hDivFlow(i,j,bi,bj)
     &                              *recip_rA(i,j,bi,bj)*DeltaTmom
         ENDDO
        ENDDO
      ENDIF
        
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Compute the Divergence of The Barotropic Flow :

C-    Initialise 
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
         hDivFlow(i,j,bi,bj) = 0.
       ENDDO
      ENDDO

      DO k=1,Nr
       
C-    Calculate velocity field "volume transports" through tracer cell faces
        DO j=1,sNy+1
         DO i=1,sNx+1
          uTrans(i,j) = uFld(i,j,k,bi,bj)*_dyG(i,j,bi,bj)
     &                 *drF(k)*_hFacW(i,j,k,bi,bj)
          vTrans(i,j) = vFld(i,j,k,bi,bj)*_dxG(i,j,bi,bj)
     &                 *drF(k)*_hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C-    Integrate vertically the Horizontal Divergence 
        DO j=1,sNy
         DO i=1,sNx
           hDivFlow(i,j,bi,bj) = hDivFlow(i,j,bi,bj)
     &       +maskC(i,j,k,bi,bj)*( uTrans(i+1,j)-uTrans(i,j)
     &                            +vTrans(i,j+1)-vTrans(i,j) )
         ENDDO
        ENDDO

C-    End DO k=1,Nr
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Change of Eta <== -Divergence of The Barotropic Flow : Implicit part
      IF ( myTime.NE.StartTime ) THEN
        DO j=1,sNy
         DO i=1,sNx
           etaN(i,j,bi,bj) = etaN(i,j,bi,bj) 
     &      - implicDiv2Dflow*hDivFlow(i,j,bi,bj)
     &                       *recip_rA(i,j,bi,bj)*DeltaTmom
         ENDDO
        ENDDO
#ifdef DBUG_EXACT_ETA
        IF (bi.EQ.1 .AND. bj.EQ.1) THEN
          eta_sum1 = 0.
          eta_sum2 = 0.
          eta_dif1 = 0.
          eta_dif2 = 0.
          eta_dif3 = 0.
        ENDIF
        DO j=1,sNy
         DO i=1,sNx
           eta_sum1 = eta_sum1
     &              + rA(i,j,bi,bj)*eta_tmp(i,j)
           eta_sum2 = eta_sum2
     &              + rA(i,j,bi,bj)*etaN(i,j,bi,bj)
           eta_dif1 = eta_dif1
     &              + rA(i,j,bi,bj)*abs(etaNm1(i,j,bi,bj)-eta_tmp(i,j))
           eta_dif2 = eta_dif2
     &              + rA(i,j,bi,bj)*abs( etaN(i,j,bi,bj) -eta_tmp(i,j))
           eta_dif3 = eta_dif3
     &           + rA(i,j,bi,bj)*abs(etaN(i,j,bi,bj)-etaNm1(i,j,bi,bj))
         ENDDO
        ENDDO
        IF (bi.EQ.nSx .AND. bj.EQ.nSy) THEN
          eta_sum1 = recip_sumArea*eta_sum1
          eta_sum2 = recip_sumArea*eta_sum2
          eta_dif1 = recip_sumArea*eta_dif1
          eta_dif2 = recip_sumArea*eta_dif2
          eta_dif3 = recip_sumArea*eta_dif3
         write(6,'(A,I9,I2,1P5E14.6)') 'ETA_SUM,DIF:',
     &    myIter,myThid, eta_sum1,eta_sum2, eta_dif1,eta_dif2,eta_dif3
        ENDIF
      ELSE
        IF (bi.EQ.1 .AND. bj.EQ.1) sumArea = 0.
        DO j=1,sNy
         DO i=1,sNx
           k = ksurfC(i,j,bi,bj)
           if (k.le.Nr) sumArea = sumArea + rA(i,j,bi,bj)
         ENDDO
        ENDDO
        IF (bi.EQ.nSx .AND. bj.EQ.nSy .AND. recip_sumArea.EQ.0.
     &                .AND. sumArea.GT.0.) THEN
          recip_sumArea=1./sumArea
          write(6,'(2A,I9,I2,1P2E22.14)') 'ETA_SUM,DIF:',
     &     ' sumArea,recip_=', myIter,myThid, sumArea, recip_sumArea
        ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* DBUG_EXACT_ETA */
      ENDIF

#endif /* EXACT_CONSERV */

      RETURN
      END
