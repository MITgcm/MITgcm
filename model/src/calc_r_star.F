C $Header: /u/gcmpack/MITgcm/model/src/calc_r_star.F,v 1.23 2014/08/15 08:21:57 atn Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_EXCH2
# include "W2_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CALC_R_STAR
C     !INTERFACE:
      SUBROUTINE CALC_R_STAR( etaFld,
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_R_STAR
C     | o Calculate new column thickness & scaling factor for r*
C     |   according to the surface r-position (Non-Lin Free-Surf)
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
C     etaFld    :: current eta field used to update the hFactor
C     myTime    :: current time in simulation
C     myIter    :: current iteration number in simulation
C     myThid    :: thread number for this instance of the routine.
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
C     Local variables
C     rStarAreaWeight :: use area weighted average for rStarFac at U & V point
C     i,j, bi,bj      :: loop counter
C     numbWrite       :: count the Number of warning written on STD-ERR file
C     numbWrMax       ::  maximum  Number of warning written on STD-ERR file
      LOGICAL rStarAreaWeight
      _RL maxhFacC
      INTEGER i,j,bi,bj
      INTEGER numbWrite, numbWrMax
      INTEGER icntc1, icntc2, icntw, icnts
      _RL tmpfldW, tmpfldS
CEOP

#ifdef W2_FILL_NULL_REGIONS
      INTEGER ii,jj
#endif
      DATA numbWrite / 0 /
      numbWrMax = Nx*Ny

      maxhFacC    = 0. _d 0

      rStarAreaWeight = .TRUE.
C-    Area-weighted average consistent with KE (& vert. advection):
      IF ( vectorInvariantMomentum .AND.
     &     (selectKEscheme.EQ.1 .OR. selectKEscheme.EQ.3)
     &   ) rStarAreaWeight =.FALSE.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('CALC_R_STAR',myThid)
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C--   before updating rStarFacC/S/W save current fields
         DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             rStarFacNm1C(i,j,bi,bj) = rStarFacC(i,j,bi,bj)
             rStarFacNm1S(i,j,bi,bj) = rStarFacS(i,j,bi,bj)
             rStarFacNm1W(i,j,bi,bj) = rStarFacW(i,j,bi,bj)
           ENDDO
         ENDDO

C-    1rst bi,bj loop :

C-- copy rStarFacX -> rStarExpX
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            rStarExpC(i,j,bi,bj) = rStarFacC(i,j,bi,bj)
            rStarExpW(i,j,bi,bj) = rStarFacW(i,j,bi,bj)
            rStarExpS(i,j,bi,bj) = rStarFacS(i,j,bi,bj)
          ENDDO
        ENDDO

C-- Compute the new column thikness :
        DO j=0,sNy+1
         DO i=0,sNx+1
          IF (kSurfC(i,j,bi,bj).LE.Nr ) THEN
           rStarFacC(i,j,bi,bj) =
     &      (etaFld(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
     &      *recip_Rcol(i,j,bi,bj)
          ELSE
           rStarFacC(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO
       IF ( rStarAreaWeight ) THEN
C-     Area weighted average
        DO j=1,sNy
         DO i=1,sNx+1
          IF ( kSurfW(i,j,bi,bj).LE.Nr ) THEN
           tmpfldW = rSurfW(i,j,bi,bj) - rLowW(i,j,bi,bj)
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
          IF ( kSurfS(i,j,bi,bj).LE.Nr ) THEN
           tmpfldS = rSurfS(i,j,bi,bj) - rLowS(i,j,bi,bj)
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
       ELSE
C-     Simple average
        DO j=1,sNy
         DO i=1,sNx+1
          IF ( kSurfW(i,j,bi,bj).LE.Nr ) THEN
           tmpfldW = rSurfW(i,j,bi,bj) - rLowW(i,j,bi,bj)
           rStarFacW(i,j,bi,bj) =
     &       ( 0.5 _d 0 *( etaFld(i-1,j,bi,bj) + etaFld(i,j,bi,bj) )
     &        +tmpfldW )/tmpfldW
          ELSE
           rStarFacW(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO
        DO j=1,sNy+1
         DO i=1,sNx
          IF ( kSurfS(i,j,bi,bj).LE.Nr ) THEN
           tmpfldS = rSurfS(i,j,bi,bj) - rLowS(i,j,bi,bj)
           rStarFacS(i,j,bi,bj) =
     &       ( 0.5 _d 0 *( etaFld(i,j-1,bi,bj) + etaFld(i,j,bi,bj) )
     &        +tmpfldS )/tmpfldS
          ELSE
           rStarFacS(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO
       ENDIF
#ifdef ALLOW_OBCS
       IF (useOBCS) THEN
         CALL OBCS_APPLY_R_STAR(
     I                    bi, bj, etaFld,
     U                    rStarFacC, rStarFacW, rStarFacS,
     I                    myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_OBCS */

C-    Needs to do something when r* ratio is too small ;
C     for now, just stop
        icntc1 = 0
        icntc2 = 0
        icntw  = 0
        icnts  = 0
        DO j=1,sNy+1
         DO i=1,sNx+1
          IF ( rStarFacC(i,j,bi,bj).LT.hFacInf ) THEN
            icntc1 = icntc1 + 1
          ENDIF
          IF ( rStarFacW(i,j,bi,bj).LT.hFacInf ) THEN
            icntw = icntw + 1
          ENDIF
          IF ( rStarFacS(i,j,bi,bj).LT.hFacInf ) THEN
            icnts = icnts + 1
          ENDIF
          IF ( rStarFacC(i,j,bi,bj).GT.hFacSup ) THEN
            icntc2 = icntc2 + 1
            maxhFacC = max(rStarFacC(i,j,bi,bj),maxhFacC)
          ENDIF
         ENDDO
        ENDDO

        IF ( icntc1+icnts+icntw .GT. 0 ) THEN
C-    Print an error msg and then stop:
         DO j=1,sNy+1
          DO i=1,sNx+1
           IF ( rStarFacC(i,j,bi,bj).LT.hFacInf ) THEN
            WRITE(errorMessageUnit,'(A,2I4,A,1F10.6,1P2E14.6)')
     &       ' fail at i,j=',i,j,' ; rStarFacC,H,eta =',
     &       rStarFacC(i,j,bi,bj),
     &       Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj),
     &       etaFld(i,j,bi,bj)
           ENDIF
           IF ( rStarFacW(i,j,bi,bj).LT.hFacInf ) THEN
            tmpfldW = rSurfW(i,j,bi,bj) - rLowW(i,j,bi,bj)
            WRITE(errorMessageUnit,'(A,2I4,A,1F10.6,1P3E14.6)')
     &       ' fail at i,j=',i,j,' ; rStarFacW,H,eta =',
     &        rStarFacW(i,j,bi,bj), tmpfldW,
     &        etaFld(i-1,j,bi,bj), etaFld(i,j,bi,bj)
           ENDIF
           IF ( rStarFacS(i,j,bi,bj).LT.hFacInf ) THEN
            tmpfldS = rSurfS(i,j,bi,bj) - rLowS(i,j,bi,bj)
            WRITE(errorMessageUnit,'(A,2I4,A,1F10.6,1P3E14.6)')
     &       ' fail at i,j=',i,j,' ; rStarFacS,H,eta =',
     &        rStarFacS(i,j,bi,bj), tmpfldS,
     &        etaFld(i,j-1,bi,bj), etaFld(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         IF ( icntc1  .GT. 0 ) 
     &    WRITE(errorMessageUnit,'(A,I8,A,3I4,I10)')
     &     'WARNING: r*FacC < hFacInf at',icntc1,
     &     ' pts : bi,bj,Thid,Iter=',bi,bj,myThid,myIter
         IF ( icntw  .GT. 0 ) 
     &    WRITE(errorMessageUnit,'(A,I8,A,3I4,I10)')
     &     'WARNING: r*FacW < hFacInf at',icntw,
     &     ' pts : bi,bj,Thid,Iter=',bi,bj,myThid,myIter
         IF ( icnts  .GT. 0 ) 
     &    WRITE(errorMessageUnit,'(A,I8,A,3I4,I10)')
     &     'WARNING: r*FacS < hFacInf at',icnts,
     &     ' pts : bi,bj,Thid,Iter=',bi,bj,myThid,myIter
         WRITE(errorMessageUnit,'(A)')
     &    'STOP in CALC_R_STAR : too SMALL rStarFac[C,W,S] !'
         STOP 'ABNORMAL END: S/R CALC_R_STAR'
        ENDIF

C-- Usefull warning when r*Fac becomes very large:
        IF ( icntc2.GT.0 .AND. numbWrite.LE.numbWrMax ) THEN
         numbWrite = numbWrite + 1
         WRITE(errorMessageUnit,'(A,I8,A,3I4,I10)')
     &    'WARNING: r*FacC > hFacSup at',icntc2,
     &    ' pts : bi,bj,Thid,Iter=',bi,bj,myThid,myIter
         WRITE(errorMessageUnit,'(A,E14.6)')
     &    'WARNING: max(hFacC) is ',maxhFacC
        ENDIF

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
#ifdef W2_FILL_NULL_REGIONS
C- Note: rStarFacC was non-zero EVERYWHERE before exch, but exch2 put zeros
C        in the corner regions of the tile (e.g.:[1-OLx:0,1-OLy:0])
C       => need to add those lines (or to fix exch2):
        DO j=1,OLy
         DO i=1,OLx
          ii = sNx+i
          jj = sNy+j

          IF (kSurfC(1-i,1-j,bi,bj).GT.Nr) rStarFacC(1-i,1-j,bi,bj)= 1.
          IF (kSurfC(ii, 1-j,bi,bj).GT.Nr) rStarFacC(ii, 1-j,bi,bj)= 1.
          IF (kSurfC(1-i,jj, bi,bj).GT.Nr) rStarFacC(1-i,jj, bi,bj)= 1.
          IF (kSurfC(ii, jj, bi,bj).GT.Nr) rStarFacC(ii, jj, bi,bj)= 1.

          IF (kSurfW(1-i,1-j,bi,bj).GT.Nr) rStarFacW(1-i,1-j,bi,bj)= 1.
          IF (kSurfW(ii, 1-j,bi,bj).GT.Nr) rStarFacW(ii, 1-j,bi,bj)= 1.
          IF (kSurfW(1-i,jj, bi,bj).GT.Nr) rStarFacW(1-i,jj, bi,bj)= 1.
          IF (kSurfW(ii, jj, bi,bj).GT.Nr) rStarFacW(ii, jj, bi,bj)= 1.

          IF (kSurfS(1-i,1-j,bi,bj).GT.Nr) rStarFacS(1-i,1-j,bi,bj)= 1.
          IF (kSurfS(ii, 1-j,bi,bj).GT.Nr) rStarFacS(ii, 1-j,bi,bj)= 1.
          IF (kSurfS(1-i,jj, bi,bj).GT.Nr) rStarFacS(1-i,jj, bi,bj)= 1.
          IF (kSurfS(ii, jj, bi,bj).GT.Nr) rStarFacS(ii, jj, bi,bj)= 1.

         ENDDO
        ENDDO
#endif /* W2_FILL_NULL_REGIONS */
#endif /* ALLOW_EXCH2 */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           rStarDhCDt(i,j,bi,bj)=(rStarFacC(i,j,bi,bj)
     &                           -rStarExpC(i,j,bi,bj))/deltaTFreeSurf
           rStarDhWDt(i,j,bi,bj)=(rStarFacW(i,j,bi,bj)
     &                           -rStarExpW(i,j,bi,bj))/deltaTFreeSurf
           rStarDhSDt(i,j,bi,bj)=(rStarFacS(i,j,bi,bj)
     &                           -rStarExpS(i,j,bi,bj))/deltaTFreeSurf
           rStarExpC(i,j,bi,bj) = rStarFacC(i,j,bi,bj)
     &                          / rStarExpC(i,j,bi,bj)
           rStarExpW(i,j,bi,bj) = rStarFacW(i,j,bi,bj)
     &                          / rStarExpW(i,j,bi,bj)
           rStarExpS(i,j,bi,bj) = rStarFacS(i,j,bi,bj)
     &                          / rStarExpS(i,j,bi,bj)
         ENDDO
        ENDDO

        IF ( fluidIsAir ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           pStarFacK(i,j,bi,bj) = rStarFacC(i,j,bi,bj)**atm_kappa
          ENDDO
         ENDDO
#ifdef ALLOW_AUTODIFF
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           pStarFacK(i,j,bi,bj) = 1. _d 0
          ENDDO
         ENDDO
#endif
        ENDIF

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
