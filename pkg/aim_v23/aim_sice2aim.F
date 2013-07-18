C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_sice2aim.F,v 1.14 2013/07/18 20:00:10 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_THSICE
#include "THSICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_SICE2AIM
C     !INTERFACE:
      SUBROUTINE AIM_SICE2AIM(
     I               land_frc,
     U               aimTsoce, aimSIfrc,
     O               aimTsice, aimAlb,
     I               myTime, myIter, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R AIM_SICE2AIM
C     | provide surface Boundary Conditions over sea-ice
C     | (from thsice pkg) to atmospheric physics package AIM
C     *================================================================*
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "com_forcon.h"

#ifdef ALLOW_THSICE
C-- Sea-Ice package
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"
      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )
#endif /* ALLOW_THSICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     aimTsoce :: sea surface temp [K], used in AIM
C     aimSIfrc :: sea-ice fraction [0-1]
C     aimTsice :: sea-ice (or snow) surface temp (K), used in AIM
C     aimAlb   :: sea-ice albedo [0-1], used in AIM
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     bi,bj    :: Tile index
C     myThid   :: Number of this instance of the routine
      _RS  land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aimTsoce(sNx,sNy)
      _RL  aimSIfrc(sNx,sNy)
      _RL  aimTsice(sNx,sNy)
      _RL  aimAlb(sNx,sNy)
      INTEGER myIter, bi, bj, myThid
      _RL myTime
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AIM
#ifdef ALLOW_THSICE

C     == Local variables ==
C     i,j          :: Loop counters
      INTEGER i,j

      IF ( .TRUE. ) THEN
C-    Use thsice-pkg output instead of prescribed Temp & ice fraction
        DO j=1,sNy
         DO i=1,sNx
           aimTsice(i,j) = Tsrf(i,j,bi,bj)+celsius2K
           aimSIfrc(i,j) = iceMask(i,j,bi,bj)
         ENDDO
        ENDDO
      ELSE
C-    Fill in thsice-pkg Temp. using AIM surf. fields
        DO j=1,sNy
         DO i=1,sNx
           Tsrf (i,j,bi,bj) = aimTsice(i,j)-celsius2K
           Tice1(i,j,bi,bj) = Tsrf (i,j,bi,bj)
           Tice2(i,j,bi,bj) = Tsrf (i,j,bi,bj)
           iceMask(i,j,bi,bj) = aimSIfrc(i,j)
         ENDDO
        ENDDO
      ENDIF

      IF ( .TRUE. ) THEN
C-     Compute albedo over sea-ice
        CALL THSICE_ALBEDO(
     I          bi, bj, siLo, siHi, sjLo, sjHi,
     I          1, sNx, 1, sNy,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     I          snowAge(siLo,sjLo,bi,bj),
     O          siceAlb(siLo,sjLo,bi,bj), icAlbNIR(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )
        DO j=1,sNy
         DO i=1,sNx
          aimAlb(i,j) = siceAlb(i,j,bi,bj)
         ENDDO
        ENDDO
      ELSE
C-    Surface Albedo : (from F.M. FORDATE S/R)
        DO j=1,sNy
         DO i=1,sNx
           aimAlb(i,j) = ALBICE
         ENDDO
        ENDDO
      ENDIF

C-- fill in ocean mixed layer variables
C   notes: this replace reading initial conditions from files.
C          needs to be done before call to phy_driver (since freezing
C          temp. is fct of salinity) ; but would be better somewhere else.
      IF ( tauRelax_MxL .EQ. -1. _d 0
     &     .OR. ( stepFwd_oceMxL  .AND. StartIceModel.NE.0
     &                            .AND. myIter.EQ.nIter0   )
     &     .OR. ( myIter.EQ.0 .AND. myTime.EQ.baseTime
     &                        .AND. .NOT.useCoupler )
     &   ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. _d 0 ) THEN
           tOceMxL(i,j,bi,bj) = aimTsoce(i,j)-celsius2K
           sOceMxL(i,j,bi,bj) = sMxL_default
          ENDIF
         ENDDO
        ENDDO
        IF ( myIter.EQ.nIter0 ) THEN
C-- Over-write the initial T,S_MxL files with the correct fields
          CALL WRITE_LOCAL_RL( 'ice_tOceMxL', 'I10', 1,
     &                         tOceMxL(1-OLx,1-OLy,bi,bj),
     &                         bi, bj, 1, myIter, myThid )
          CALL WRITE_LOCAL_RL( 'ice_sOceMxL', 'I10', 1,
     &                         sOceMxL(1-OLx,1-OLy,bi,bj),
     &                         bi, bj, 1, myIter, myThid )
        ENDIF
      ELSE
C-- Use ocean mixed layer Temp as Atmos. SST (instead of prescribed Temp)
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. _d 0 ) THEN
           aimTsoce(i,j) = tOceMxL(i,j,bi,bj)+celsius2K
          ENDIF
         ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_THSICE */
#endif /* ALLOW_AIM */

      RETURN
      END
