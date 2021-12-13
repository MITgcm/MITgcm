#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_APPLY_IMPORT
C     !INTERFACE:
      SUBROUTINE ATM_APPLY_IMPORT(
     I               land_frc,
     U               atmSST, atmSIfrc,
     I               myTime, myIter, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R ATM_APPLY_IMPORT
C     | o Apply imported coupling data to ATM surface BC over ocean
C     *================================================================*
C     | Note: when using sea-ice, fill in Mixed layer fields instead
C     |      to be used later as SST by Atmos. + Sea-Ice
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"

#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif

C-- Coupled to the Ocean :
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     atmSST   :: sea surface temp [K], used in ATM component
C     atmSIfrc :: sea-ice fraction [0-1], used in ATM component
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     bi,bj    :: Tile index
C     myThid   :: Number of this instance of the routine
      _RS  land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  atmSST  (sNx,sNy)
      _RL  atmSIfrc(sNx,sNy)
      _RL myTime
      INTEGER myIter, bi, bj, myThid
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     i,j    :: Loop counters
      INTEGER i,j

#ifdef ALLOW_THSICE
      IF ( useImportThSIce .AND. useThSIce ) THEN
       IF ( MOD(myIter,cplSendFrq_iter).EQ.0 ) THEN
C--   Put thSIce imported fields from the ocean component in thSIce common block
        DO j=1,sNy
         DO i=1,sNx
           iceMask   (i,j,bi,bj) = sIceFrac_cpl (i,j,bi,bj)
           iceHeight (i,j,bi,bj) = sIceThick_cpl(i,j,bi,bj)
           snowHeight(i,j,bi,bj) = sIceSnowH_cpl(i,j,bi,bj)
           Qice1     (i,j,bi,bj) = sIceQ1_cpl   (i,j,bi,bj)
           Qice2     (i,j,bi,bj) = sIceQ2_cpl   (i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
C--   end if useImportThSIce & useThSIce
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_THSICE
      IF ( useThSIce ) THEN
C--   Put fields from the ocean component in Mixed-layer arrays:

C-    fill in hOceMxL with Mixed-layer Depth from the ocean component
       IF ( useImportMxlD ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. ) THEN
            hOceMxL(i,j,bi,bj) = ocMxlD(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF

C-    fill in tOceMxL with Sea-Surface Temp. from the ocean component
       IF ( useImportSST ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. ) THEN
            tOceMxL(i,j,bi,bj) = SSTocn(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF

C-    fill in sOceMxL with Sea-Surf Salinity from the ocean component
       IF ( useImportSSS ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. ) THEN
            sOceMxL(i,j,bi,bj) = SSSocn(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF

C-    fill in v2ocMxL with surf. velocity^2  from the ocean component
       IF ( useImportVsq ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. ) THEN
            v2ocMxL(i,j,bi,bj) = vSqocn(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF

      ELSEIF ( useAtm_Phys ) THEN
#else /* ALLOW_THSICE */
      IF ( useAtm_Phys ) THEN
#endif /* ALLOW_THSICE */
C--   supply imported fields to Atm_Phys pkg:

       IF ( useImportSST ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( ocMxlD(i,j,bi,bj) .GT. 0. ) THEN
            atmSST(i,j) = SSTocn(i,j,bi,bj)+celsius2K
          ENDIF
         ENDDO
        ENDDO
       ENDIF

      ELSE
C--   supply imported fields to AIM (Phys) pkg:

       IF ( useImportSST ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj) .LT. 1. ) THEN

C--   take SST from the ocean compon where Sea-Ice fraction is zero
           IF ( atmSIfrc(i,j).EQ.0. ) THEN
            atmSST(i,j) = SSTocn(i,j,bi,bj)+celsius2K
           ELSEIF ( SSTocn(i,j,bi,bj).GE. -1. _d 0)  THEN
C--   take SST from the ocean compon if clearly warmer than freezing
C       then reset sea-ice fraction
            atmSST(i,j) = SSTocn(i,j,bi,bj)+celsius2K
            atmSIfrc(i,j) = 0.
           ENDIF

          ENDIF
         ENDDO
        ENDDO
       ENDIF

C-- if useThSIce / else / endif
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
