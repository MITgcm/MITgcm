C $Header: /u/gcmpack/MITgcm/model/src/calc_surf_dr.F,v 1.1 2001/08/27 18:50:10 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CALC_SURF_DR(etaFld,
     I                        myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_SURF_DR                                  |
C     | o Calculate the new surface level thickness according to |
C     |   the surface r-position  (Non-Linear Free-Surf)         |
C     | o take decision if grid box becomes too thin or too thick|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
C     etaFld - current eta field used to update the hFactor
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      _RL etaFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

#ifdef NONLIN_FRSURF

C     Local variables
C     i,j,k,bi,bj - loop counter
      INTEGER i,j,k,bi,bj
      INTEGER ks
      _RL hFactmp
      _RS hhm, hhp
      CHARACTER*(MAX_LEN_MBUF) suff

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Initialise arrays :
      IF (myIter.EQ.nIter0) THEN
       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx 
           hFac_surfC(i,j,bi,bj) = 0.
           hFac_surfW(i,j,bi,bj) = 0.
           hFac_surfS(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Compute the new fractional thickness of surface level (ksurfC):
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
c       DO j=1-Oly,sNy+Oly
c        DO i=1-Olx,sNx+Olx 
        DO j=1,sNy
         DO i=1,sNx
          ks = ksurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFac_surfC(i,j,bi,bj) = hFacC(i,j,ks,bi,bj)
           hFactmp = ( Ro_surf(i,j,bi,bj)+etaFld(i,j,bi,bj)
     &                -MAX( rF(ks+1), R_low(i,j,bi,bj) )
     &               )*recip_drF(ks)
           IF ( hFactmp.GE.hFacInf .AND. hFactmp.LE.hFacSup ) THEN
             hFac_surfC(i,j,bi,bj) = maskC(i,j,ks,bi,bj)*hFactmp 
           ELSE
C---------- needs to do something :
             IF (hFactmp.LT.hFacInf) THEN
              write(0,'(2A,6I4,I10)') 'WARNING: hFacC < hFacInf at:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
             ENDIF
             IF (hFactmp.GT.hFacSup) THEN
              write(0,'(2A,6I4,I10)') 'WARNING: hFacC > hFacSup at:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
             ENDIF
              write(0,'(A,2F10.6,1PE14.6)') 'hFac_n-1,hFac_n,eta =',
     &          hfacC(i,j,ks,bi,bj), hFactmp, etaFld(i,j,bi,bj)
              write(0,'(2A)') 'STOP in CALC_SURF_DR :',
     &                        ' too SMALL or too LARGE hFac !'
              STOP 'ABNORMAL END: S/R CALC_SURF_DR' 
C----------
           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_R4(hFac_surfC, myThid ) 

C-- Apply the mask after the exchange : <-- no need here
c     DO bj=myByLo(myThid), myByHi(myThid)
c      DO bi=myBxLo(myThid), myBxHi(myThid)  
c       DO j=1-Oly,sNy+Oly
c        DO i=1-Olx,sNx+Olx 
c         ks = ksurfC(i,j,bi,bj)
c         IF (ks.LE.Nr) THEN
c           hFac_surfC(i,j,bi,bj) = hFac_surfC(i,j,bi,bj)
c    &                              * maskC(i,j,ks,bi,bj)
c         ELSE
c           hFac_surfC(i,j,bi,bj) = 0.
c         ENDIF
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Compute fractional thickness of surface level, for U & V point:

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)  
        DO j=1,sNy
         DO i=1,sNx+1
          ks = ksurfW(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
            hhm = hFacC(i-1,j,ks,bi,bj)
            IF(ks.EQ.ksurfC(i-1,j,bi,bj)) hhm=hFac_surfC(i-1,j,bi,bj)
            hhp = hFacC(i,j,ks,bi,bj)
            IF(ks.EQ.ksurfC(i,j,bi,bj))   hhp=hFac_surfC(i,j,bi,bj)
            hFac_surfW(i,j,bi,bj) = MIN(hhm,hhp)*maskW(i,j,ks,bi,bj)
          ENDIF
         ENDDO
        ENDDO
        DO j=1,sNy+1
         DO i=1,sNx
          ks = ksurfS(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
            hhm = hFacC(i,j-1,ks,bi,bj)
            IF(ks.EQ.ksurfC(i,j-1,bi,bj)) hhm=hFac_surfC(i,j-1,bi,bj)
            hhp = hFacC(i,j,ks,bi,bj)
            IF(ks.EQ.ksurfC(i,j,bi,bj)) hhp = hFac_surfC(i,j,bi,bj)
            hFac_surfS(i,j,bi,bj) = MIN(hhm,hhp)*maskS(i,j,ks,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_UV_XY_RS(hFac_surfW,hFac_surfS,.FALSE.,myThid)

C-- Apply the mask after the exchange : <-- no need here
c     DO bj=myByLo(myThid), myByHi(myThid)
c      DO bi=myBxLo(myThid), myBxHi(myThid)  
c       DO j=1-Oly,sNy+Oly
c        DO i=1-Olx,sNx+Olx 

c         ks = ksurfW(i,j,bi,bj)
c         IF (ks.LE.Nr) THEN
c          hFac_surfW(i,j,bi,bj) = hFac_surfW(i,j,bi,bj)
c    &                             * maskW(i,j,ks,bi,bj)
c         ELSE
c          hFac_surfW(i,j,bi,bj) = 0.
c         ENDIF

c         ks = ksurfS(i,j,bi,bj)
c         IF (ks.LE.Nr) THEN
c          hFac_surfS(i,j,bi,bj) = hFac_surfS(i,j,bi,bj)
c    &                             * maskS(i,j,ks,bi,bj)
c         ELSE
c          hFac_surfS(i,j,bi,bj) = 0.
c         ENDIF

c        ENDDO
c       ENDDO
c      ENDDO
c     ENDDO

      WRITE(suff,'(I10.10)') myIter
c     CALL WRITE_FLD_XY_RS('hFac_surfC.',suff,hFac_surfC,
c    &                     myIter,myThid)                    

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
