C $Header: /u/gcmpack/MITgcm/model/src/ini_sigma_hfac.F,v 1.1 2010/09/11 21:24:52 jmc Exp $
C $Name:  $

c#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_SIGMA_HFAC
C     !INTERFACE:
      SUBROUTINE INI_SIGMA_HFAC( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_SIGMA_HFAC
C     | o Initialise grid factors when using Sigma coordiante
C     *==========================================================*
C     | These arrays are used throughout the code and describe
C     | fractional height factors.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  ::  Number of this instance of INI_SIGMA_HFAC
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi, bj     :: tile indices
C     i, j, k    :: Loop counters
C     rEmpty     :: empty column r-position
C     rFullDepth :: maximum depth of a full column
C     tmpFld     :: Temporary array used to compute & write Total Depth
C     min_hFac   :: actual minimum of cell-centered hFac
C     msgBuf     :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, k
      _RS rEmpty
      _RL rFullDepth
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL min_hFac
      _RL hFactmp
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C     r(ij,k,t) = rLow(ij) + aHybSigm(k)*[rF(1)-rF(Nr+1)]
C               + bHybSigm(k)*[eta(ij,t)+Ro_surf(ij) - rLow(ij)]

      IF ( usingPCoords ) rEmpty = rF(Nr+1)
      IF ( usingZCoords ) rEmpty = rF(1)
      rFullDepth = rF(1)-rF(Nr+1)

C---  Calculate partial-cell factor hFacC :
      min_hFac = 1.
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    Remove column (mask=0) thinner than hFacMin*rFullDepth
C       ensures hFac > hFacMin (assuming we use pure Sigma)
C Note: because of unfortunate hFacMin default value (=1) (would produce
C       unexpected empty column), for now, use hFacInf instead of hFacMin
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           tmpFld(i,j) = Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj)
c          IF ( tmpFld(i,j).LT.hFacMin*rFullDepth )
           IF ( tmpFld(i,j).LT.hFacInf*rFullDepth )
     &       tmpFld(i,j) = 0. _d 0
         ENDDO
        ENDDO
c#ifdef ALLOW_SHELFICE
C--   Would need a specific call here similar to SHELFICE_UPDATE_MASKS
c     IF ( useShelfIce ) THEN
c     ENDIF
c#endif /* ALLOW_SHELFICE */
C-    Set (or reset) other 2-D cell-centered fields
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           IF ( tmpFld(i,j).GT.0. _d 0 ) THEN
             kSurfC (i,j,bi,bj) = 1
             kLowC  (i,j,bi,bj) = Nr
             maskInC(i,j,bi,bj) = 1.
             recip_Rcol(i,j,bi,bj) = 1. _d 0 / tmpFld(i,j)
           ELSE
             kSurfC (i,j,bi,bj) = Nr+1
             kLowC  (i,j,bi,bj) = 0
             maskInC(i,j,bi,bj) = 0.
             recip_Rcol(i,j,bi,bj) = 0. _d 0
             Ro_surf(i,j,bi,bj) = rEmpty
             R_low(i,j,bi,bj)   = rEmpty
           ENDIF
         ENDDO
        ENDDO
C-    Set 3-D hFacC
        DO k=1, Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( maskInC(i,j,bi,bj).NE.0. _d 0 ) THEN
             hFactmp = ( dAHybSigF(k)*rFullDepth
     &                 + dBHybSigF(k)*tmpFld(i,j)
     &                 )*recip_drF(k)
             hFacC(i,j,k,bi,bj) = hFactmp
             min_hFac = MIN( min_hFac, hFactmp )
           ELSE
             hFacC(i,j,k,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C-    end bi,bj loops.
       ENDDO
      ENDDO

      WRITE(msgBuf,'(A,1PE14.6)')
     &     'S/R INI_SIGMA_HFAC: minimum hFacC=', min_hFac
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c     CALL PLOT_FIELD_XYRS(R_low,
c    &         'Model R_low (ini_masks_etc)', 1, myThid)
c     CALL PLOT_FIELD_XYRS(Ro_surf,
c    &         'Model Ro_surf (ini_masks_etc)', 1, myThid)

C--   Set Western & Southern fields (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    set 2-D mask and rLow & reference rSurf at Western & Southern edges
        i = 1-OlX
        DO j=1-Oly,sNy+Oly
           rSurfW(i,j,bi,bj) = rEmpty
           rLowW (i,j,bi,bj) = rEmpty
           maskInW(i,j,bi,bj)= 0.
        ENDDO
        j = 1-Oly
        DO i=1-Olx,sNx+Olx
           rSurfS(i,j,bi,bj) = rEmpty
           rLowS (i,j,bi,bj) = rEmpty
           maskInS(i,j,bi,bj)= 0.
        ENDDO
        DO j=1-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
           maskInW(i,j,bi,bj)= maskInC(i-1,j,bi,bj)*maskInC(i,j,bi,bj)
           rSurfW(i,j,bi,bj) =
     &               ( Ro_surf(i-1,j,bi,bj)
     &               + Ro_surf( i, j,bi,bj) )*0.5 _d 0
           rLowW(i,j,bi,bj)  =
     &                 ( R_low(i-1,j,bi,bj)
     &                 + R_low( i, j,bi,bj) )*0.5 _d 0
c          rSurfW(i,j,bi,bj) =
c    &               ( Ro_surf(i-1,j,bi,bj)*rA(i-1,j,bi,bj)
c    &               + Ro_surf( i, j,bi,bj)*rA( i, j,bi,bj)
c    &               )*recip_rAw(i,j,bi,bj)*0.5 _d 0
c          rLowW(i,j,bi,bj)  =
c    &                 ( R_low(i-1,j,bi,bj)*rA(i-1,j,bi,bj)
c    &                 + R_low( i, j,bi,bj)*rA( i, j,bi,bj)
c    &                 )*recip_rAw(i,j,bi,bj)*0.5 _d 0
           IF ( maskInW(i,j,bi,bj).EQ.0. ) THEN
             rSurfW(i,j,bi,bj) = rEmpty
             rLowW (i,j,bi,bj) = rEmpty
           ENDIF
         ENDDO
        ENDDO
        DO j=2-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           maskInS(i,j,bi,bj)= maskInC(i,j-1,bi,bj)*maskInC(i,j,bi,bj)
           rSurfS(i,j,bi,bj) =
     &               ( Ro_surf(i,j-1,bi,bj)
     &               + Ro_surf(i, j, bi,bj) )*0.5 _d 0
           rLowS(i,j,bi,bj)  =
     &                 ( R_low(i,j-1,bi,bj)
     &                 + R_low(i, j, bi,bj) )*0.5 _d 0
c          rSurfS(i,j,bi,bj) =
c    &               ( Ro_surf(i,j-1,bi,bj)*rA(i,j-1,bi,bj)
c    &               + Ro_surf(i, j, bi,bj)*rA(i, j, bi,bj)
c    &               )*recip_rAs(i,j,bi,bj)*0.5 _d 0
c          rLowS(i,j,bi,bj)  =
c    &                 ( R_low(i,j-1,bi,bj)*rA(i,j-1,bi,bj)
c    &                 + R_low(i, j, bi,bj)*rA(i, j, bi,bj)
c    &                 )*recip_rAs(i,j,bi,bj)*0.5 _d 0
           IF ( maskInS(i,j,bi,bj).EQ.0. ) THEN
             rSurfS(i,j,bi,bj) = rEmpty
             rLowS (i,j,bi,bj) = rEmpty
           ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS( rSurfW,  rSurfS,  .FALSE., myThid )
      CALL EXCH_UV_XY_RS( rLowW,   rLowS,   .FALSE., myThid )
      CALL EXCH_UV_XY_RS( maskInW, maskInS, .FALSE., myThid )

C--   The following block allows thin walls representation of non-periodic
C     boundaries such as happen on the lat-lon grid at the N/S poles.
C     We should really supply a flag for doing this.
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           IF (dyG(i,j,bi,bj).EQ.0.) maskInW(i,j,bi,bj) = 0.
           IF (dxG(i,j,bi,bj).EQ.0.) maskInS(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Set hFacW and hFacS (at U and V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k=1, Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            hFactmp =
     &          ( dAHybSigF(k)*rFullDepth
     &          + dBHybSigF(k)*( rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj) )
     &          )*recip_drF(k)
            hFacW(i,j,k,bi,bj) = hFactmp*maskInW(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
        DO k=1, Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            hFactmp =
     &          ( dAHybSigF(k)*rFullDepth
     &          + dBHybSigF(k)*( rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj) )
     &          )*recip_drF(k)
            hFacS(i,j,k,bi,bj) = hFactmp*maskInS(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C-    Set surface k index for interface W & S (U & V points)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           kSurfW(i,j,bi,bj) = Nr+1
           kSurfS(i,j,bi,bj) = Nr+1
           IF ( maskInW(i,j,bi,bj).NE.0. ) kSurfW(i,j,bi,bj) = 1
           IF ( maskInS(i,j,bi,bj).NE.0. ) kSurfS(i,j,bi,bj) = 1
         ENDDO
        ENDDO
C-    end bi,bj loops.
       ENDDO
      ENDDO

      RETURN
      END
