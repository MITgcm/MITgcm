C $Header: /u/gcmpack/MITgcm/model/src/calc_r_star.F,v 1.4 2005/06/19 21:43:49 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_R_STAR
C     !INTERFACE:
      SUBROUTINE CALC_R_STAR( etaFld,
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_R_STAR                                   
C     | o Calculate new column thickness & scaling factor for r*
C     |    according to the surface r-position (Non-Lin Free-Surf)          
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
C     etaFld :: current eta field used to update the hFactor
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      _RL etaFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
C     numbWrite    :: count the Number of warning written on STD-ERR file
C     numbWrMax    ::  maximum  Number of warning written on STD-ERR file
      INTEGER i,j,k,bi,bj
      INTEGER ii,jj
      INTEGER km, numbWrite, numbWrMax
      _RL tmpfldW, tmpfldS
c     CHARACTER*(MAX_LEN_MBUF) suff
CEOP
      DATA numbWrite / 0 /
      numbWrMax = Nx*Ny

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('CALC_R_STAR',myThid)
#endif

      IF (groundAtK1) THEN
       km = 1
      ELSE
       km = Nr
      ENDIF 

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    1rst bi,bj loop :

        IF (myIter.EQ.-1) THEN
C-- Initialise arrays :
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx 
            rStarFacC(i,j,bi,bj) = 1.
            rStarFacW(i,j,bi,bj) = 1.
            rStarFacS(i,j,bi,bj) = 1.
            rStarExpC(i,j,bi,bj) = 1.
            rStarExpW(i,j,bi,bj) = 1.
            rStarExpS(i,j,bi,bj) = 1.
            rStarDhCDt(i,j,bi,bj) = 0.
            rStarDhWDt(i,j,bi,bj) = 0.
            rStarDhSDt(i,j,bi,bj) = 0.
            PmEpR(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx 
            h0FacC(i,j,k,bi,bj) = hFacC(i,j,k,bi,bj)
            h0FacW(i,j,k,bi,bj) = hFacW(i,j,k,bi,bj)
            h0FacS(i,j,k,bi,bj) = hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSE
C-- copy rStarFacX -> rStarExpX
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx 
            rStarExpC(i,j,bi,bj) = rStarFacC(i,j,bi,bj)
            rStarExpW(i,j,bi,bj) = rStarFacW(i,j,bi,bj)
            rStarExpS(i,j,bi,bj) = rStarFacS(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF

C-- Compute the new column thikness :
        DO j=0,sNy+1
         DO i=0,sNx+1
          IF (maskH(i,j,bi,bj).EQ.1. ) THEN
           rStarFacC(i,j,bi,bj) = 
     &      (etaFld(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
     &      *recip_Rcol(i,j,bi,bj)
          ELSE
           rStarFacC(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx+1
          IF (maskW(i,j,km,bi,bj).EQ.1. ) THEN
           tmpfldW = MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
     &             - MAX( R_low(i-1,j,bi,bj), R_low(i,j,bi,bj) )
           rStarFacW(i,j,bi,bj) = 
     &       ( 0.5 _d 0 *( etaFld(i-1,j,bi,bj)*rA(i-1,j,bi,bj)
     &                    +etaFld(i,j,bi,bj)*rA(i,j,bi,bj)
     &                   )*recip_rAw(i,j,bi,bj)
     &        +tmpfldW )/tmpfldW
          ELSE
           rStarFacW(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO
        DO j=1,sNy+1
         DO i=1,sNx
          IF (maskS(i,j,km,bi,bj).EQ.1. ) THEN
           tmpfldS = MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
     &             - MAX( R_low(i,j-1,bi,bj), R_low(i,j,bi,bj) )
           rStarFacS(i,j,bi,bj) = 
     &       ( 0.5 _d 0 *( etaFld(i,j-1,bi,bj)*rA(i,j-1,bi,bj)
     &                    +etaFld(i,j,bi,bj)*rA(i,j,bi,bj)
     &                   )*recip_rAs(i,j,bi,bj)
     &        +tmpfldS )/tmpfldS
          ELSE
           rStarFacS(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO

C-    Needs to do something when r* ratio is too small ;
C     for now, just stop
        DO j=1,sNy+1
         DO i=1,sNx+1
          IF ( rStarFacC(i,j,bi,bj).LT.hFacInf ) THEN  
            numbWrite = numbWrite + 1
            WRITE(errorMessageUnit,'(2A,5I4,I10)')
     &       'WARNING: r*FacC < hFacInf at:',
     &       ' i,j,bi,bj,Thid,Iter=',i,j,bi,bj,myThid,myIter
            WRITE(errorMessageUnit,'(A,1F10.6,1P2E14.6)')
     &       'rStarFac,H,eta =', rStarFacC(i,j,bi,bj), 
     &       Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj), etaFld(i,j,bi,bj)
            WRITE(errorMessageUnit,'(A)')
     &       'STOP in CALC_R_STAR : too SMALL rStarFacC !'
             STOP 'ABNORMAL END: S/R CALC_SURF_DR'  
          ENDIF
          IF ( rStarFacW(i,j,bi,bj).LT.hFacInf ) THEN  
            numbWrite = numbWrite + 1
            tmpfldW = MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
     &              - MAX( R_low(i-1,j,bi,bj), R_low(i,j,bi,bj) )
            WRITE(errorMessageUnit,'(2A,5I4,I10)')
     &       'WARNING: r*FacW < hFacInf at:',
     &       ' i,j,bi,bj,Thid,Iter=',i,j,bi,bj,myThid,myIter
            WRITE(errorMessageUnit,'(A,1F10.6,1P3E14.6)')
     &       'rStarFac,H,eta =', rStarFacW(i,j,bi,bj), tmpfldW,
     &        etaFld(i-1,j,bi,bj), etaFld(i,j,bi,bj)
            WRITE(errorMessageUnit,'(A)')
     &       'STOP in CALC_R_STAR : too SMALL rStarFacW !'
             STOP 'ABNORMAL END: S/R CALC_SURF_DR'  
          ENDIF
          IF ( rStarFacS(i,j,bi,bj).LT.hFacInf ) THEN  
            numbWrite = numbWrite + 1
            tmpfldS = MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
     &              - MAX( R_low(i,j-1,bi,bj), R_low(i,j,bi,bj) )
            WRITE(errorMessageUnit,'(2A,5I4,I10)')
     &       'WARNING: r*FacS < hFacInf at:',
     &       ' i,j,bi,bj,Thid,Iter=',i,j,bi,bj,myThid,myIter
            WRITE(errorMessageUnit,'(A,1F10.6,1P3E14.6)')
     &       'rStarFac,H,eta =', rStarFacS(i,j,bi,bj), tmpfldS,
     &        etaFld(i,j-1,bi,bj), etaFld(i,j,bi,bj)
            WRITE(errorMessageUnit,'(A)')
     &       'STOP in CALC_R_STAR : too SMALL rStarFacS !'
             STOP 'ABNORMAL END: S/R CALC_R_STAR'  
          ENDIF
C-- Usefull warning when r*Fac becomes very large:
          IF ( numbWrite.LE.numbWrMax .AND.
     &         rStarFacC(i,j,bi,bj).GT.hFacSup ) THEN
            numbWrite = numbWrite + 1
            WRITE(errorMessageUnit,'(2A,5I4,I10)')
     &       'WARNING: hFacC > hFacSup at:',
     &       ' i,j,bi,bj,Thid,Iter=',i,j,bi,bj,myThid,myIter
            WRITE(errorMessageUnit,'(A,1F10.6,1P2E14.6)')
     &       'rStarFac,H,eta =', rStarFacC(i,j,bi,bj), 
     &       Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj), etaFld(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C-    end 1rst bi,bj loop.
       ENDDO
      ENDDO

       _EXCH_XY_RL( rStarFacC, myThid ) 
      CALL EXCH_UV_XY_RL(rStarFacW,rStarFacS,.FALSE.,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    2nd bi,bj loop :

#ifdef ALLOW_EXCH2
C- Note: rStarFacC was non-zero EVERYWHERE before exch, but exch2 put zeros
C        in the corner regions of the tile (e.g.:[1-Olx:0,1-Oly:0])
C       => need to add those lines (or to fix exch2):
        DO j=1,Oly
         DO i=1,Olx
          ii = sNx+i
          jj = sNy+j

          IF (maskH(1-i,1-j,bi,bj).EQ.0.) rStarFacC(1-i,1-j,bi,bj) = 1.
          IF (maskH(ii, 1-j,bi,bj).EQ.0.) rStarFacC(ii, 1-j,bi,bj) = 1.
          IF (maskH(1-i,jj, bi,bj).EQ.0.) rStarFacC(1-i,jj, bi,bj) = 1.
          IF (maskH(ii, jj, bi,bj).EQ.0.) rStarFacC(ii, jj, bi,bj) = 1.

c         IF (ksurfW(1-i,1-j,bi,bj).LE.Nr) rStarFacW(1-i,1-j,bi,bj)=1.
          IF (maskW(1-i,1-j,km,bi,bj).EQ.0.) rStarFacW(1-i,1-j,bi,bj)=1.
          IF (maskW(ii, 1-j,km,bi,bj).EQ.0.) rStarFacW(ii, 1-j,bi,bj)=1.
          IF (maskW(1-i,jj, km,bi,bj).EQ.0.) rStarFacW(1-i,jj, bi,bj)=1.
          IF (maskW(ii, jj, km,bi,bj).EQ.0.) rStarFacW(ii, jj, bi,bj)=1.

          IF (maskS(1-i,1-j,km,bi,bj).EQ.0.) rStarFacS(1-i,1-j,bi,bj)=1.
          IF (maskS(ii, 1-j,km,bi,bj).EQ.0.) rStarFacS(ii, 1-j,bi,bj)=1.
          IF (maskS(1-i,jj, km,bi,bj).EQ.0.) rStarFacS(1-i,jj, bi,bj)=1.
          IF (maskS(ii, jj, km,bi,bj).EQ.0.) rStarFacS(ii, jj, bi,bj)=1.
         ENDDO
        ENDDO
#endif /* ALLOW_EXCH2 */

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx 
           rStarDhCDt(i,j,bi,bj)=(rStarFacC(i,j,bi,bj) 
     &                           -rStarExpC(i,j,bi,bj))/deltaTfreesurf
           rStarDhWDt(i,j,bi,bj)=(rStarFacW(i,j,bi,bj)
     &                           -rStarExpW(i,j,bi,bj))/deltaTfreesurf
           rStarDhSDt(i,j,bi,bj)=(rStarFacS(i,j,bi,bj)
     &                           -rStarExpS(i,j,bi,bj))/deltaTfreesurf
           rStarExpC(i,j,bi,bj) = rStarFacC(i,j,bi,bj)
     &                          / rStarExpC(i,j,bi,bj)
           rStarExpW(i,j,bi,bj) = rStarFacW(i,j,bi,bj)
     &                          / rStarExpW(i,j,bi,bj)
           rStarExpS(i,j,bi,bj) = rStarFacS(i,j,bi,bj)
     &                          / rStarExpS(i,j,bi,bj)
         ENDDO
        ENDDO

C-    end 2nd bi,bj loop.
        ENDDO
       ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('CALC_R_STAR',myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
