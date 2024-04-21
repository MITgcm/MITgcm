#include "STIC_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: STIC_INIT_FIXED

C !INTERFACE:
      SUBROUTINE STIC_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize STIC variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STIC.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j,bi,bj - Loop counters
      INTEGER i, j, k, bi, bj
      INTEGER CURI, CURJ, FRONT_K
C local variabls used to determine shelf-ice and ice-front masks
C     iceFrontCellThickness   :: the ratio of the horizontal length
C                         of the ice front in each model grid cell
C                         divided by the grid cell area.  The "thickness"
C                         of the colum perpendicular to the front
C     iceFrontWidth    :: the width of the ice front.
      _RL ice_bottom_Z_C
      _RL wet_top_Z_N, wet_bottom_Z_N
      _RL iceFrontWetContact_Z_max
      _RL iceFrontVertContactFrac, iceFrontCellThickness
      _RL iceFrontWidth, iceFrontFaceArea
      _RS fK_icefront (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER SI
      _RL epsilon_H

#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif /* ALLOW_DIAGNOSTICS */

C ow - 06/29/2018
C     pkg/shelfice maskSHI above is not consistent with the spirit of gencost.
C     Use mask2dSHI and mask3dSHI below instead.
C     mask2dSHI and mask3dSHI are the 2d and 3d mask for shelfice. They are
C     zero if there is no shelfice and one if otherwise. For any i,j,
C     if there is at least one non-zero mask3dSHI in the vertical, then
C     mask2dSHI at i,j is one.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           mask3dSHIICF(i,j,k,bi,bj) = 0. _d 0
           mask3dSHI(i,j,k,bi,bj) = 0. _d 0
           mask3dICF(i,j,k,bi,bj) = 0. _d 0
           IF (k.EQ.1) THEN
            mask2dSHIICF(i,j,bi,bj) = 0. _d 0
            mask2dSHI(i,j,bi,bj) = 0. _d 0
            mask2dICF(i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO

        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          sticfLength(i,j,bi,bj) = 0. _d 0
          DO SI = 1,4
           CURI_ARR(i,j,bi,bj,SI) = -9999
           CURJ_ARR(i,j,bi,bj,SI) = -9999
           sticfWidth_arr(i,j,bi,bj,SI) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

       ENDDO
      ENDDO

C--  STEEP_ICECAVITY parameters (BEGIN)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          kIcf(i,j,bi,bj) = 0
          DO k = 1 , Nr
           IF ( R_stic(i,j,bi,bj) .GT. ABS(rF(k))) kIcf(i,j,bi,bj) = k
          ENDDO
          fK_icefront(i,j,bi,bj) = FLOAT(kIcf(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C--  STEEP_ICECAVITY parameters (END)

C--   Create masks for shelf-ice and ice-front by modifyig code from
C     shelfice_thermodynamics.F minimum fraction of a cell adjacent to an
C     ice front that must be wet for exchange to happen
      epsilon_H = 1. _d -03

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   First ice front then ice shelf.  Loop through each i,j point
C--   process ice fronts in k, then process ice shelf.
        DO j = 1-OLy+1,sNy+OLy-1
         DO i = 1-OLx+1,sNx+OLx-1

C--   The k index where the ice front ends (0 if no ice front)
          FRONT_K = kIcf(i,j,bi,bj)

C--   If there is an ice front at this (i,j) continue
          IF (FRONT_K .GT. 0) THEN

C--   Loop through all depths where the ice front is fround
           DO k = 1, FRONT_K
C--   Loop around the four laterally neighboring cells of the ice front.
C--   If any neighboring points has wet volume in contact with the ice
C--   front at (i,j) then calculate ice-ocean exchanges.
C--   The four laterally neighboring point are at (CURI,CURJ)
            DO SI = 1,4
             IF     (SI .EQ. 1) THEN
C--   Looking to right
              CURI = i+1
              CURJ = j
              iceFrontWidth = dyG(i+1,j,bi,bj)
             ELSEIF (SI .EQ. 2) THEN
C--   Looking to LEFT
              CURI = i-1
              CURJ = j
              iceFrontWidth = dyG(i,j,bi,bj)
             ELSEIF (SI .EQ. 3) THEN
C--   Looking to NORTH
              CURI = i
              CURJ = j+1
              iceFrontWidth = dxG(i,j+1,bi,bj)
             ELSEIF (SI .EQ. 4) THEN
C--   Looking to south
              CURI = i
              CURJ = j-1
              iceFrontWidth = dxG(i,j,bi,bj)
             ENDIF

             CURI_ARR(i,j,bi,bj,SI) = CURI
             CURJ_ARR(i,j,bi,bj,SI) = CURJ
             sticfWidth_arr(i,j,bi,bj,SI) = iceFrontWidth

C--   cell depth describes the average distance perpendicular to the ice
C--   front fact

             iceFrontCellThickness = 0. _d 0
             IF(iceFrontWidth.NE.0. _d 0)
     &         iceFrontCellThickness = RA(CURI,CURJ,bi,bj)/iceFrontWidth
             iceFrontFaceArea  = drF(k)*iceFrontWidth

C--   First, make sure the adjacent point has at least some water in it.
             IF (_hFacC(CURI,CURJ,k,bi,bj) .GT. zeroRL) THEN

C--   we need to determine how much of the ice front is in contact with
C--   water in the neighboring grid cell at this depth level.

C--   1. Determine the top depth with water in the current cell
C--   2. Determine the top depth with water in the neighbor cell
C--   3. Determine the depth where water  gap between (1) and (2).
C--   4. If there is a gap then ice front is in contact with water in
C--      the neighboring cell

C--   ice_bottom_Z_C: the depth (m) of the bottom of the ice in the
C--               current cell.  Bounded between rF(k) and rF(k+1).
C--               * If the ice extends past the bottom of the cell then
C--                 ice_bottom_Z_C = rF(k+1)
C--               [rF(k) >= ice_bottom_Z_C >= rF(k+1)]  (rF is negative)
              ice_bottom_Z_C = MAX(rF(k+1),
     &             MIN(Ro_surf(i,j, bi,bj), rF(k)))

C--   wet_top_Z_N: the depth (m) of the bottom of the ice in the
C--              neighboring grid.  If the neighboring cell has ice in
C--              (in the form of a shelf or front) then wet_top_Z_N is
C--              the depth of this neighboring ice.
C--
C--              * If neighbor cell has no ice, then Ro_surf = 0 and
C--                wet_top_Z_N = rF(k)
C--              [rF(k) >= wet_top_Z_N >= rF(k+1)]     (rF is negative)

              wet_top_Z_N = MAX(rF(k+1),
     &             MIN(Ro_surf(CURI,CURJ, bi,bj), rF(k)))

C--   wet_bottom_Z_N: the depth (m) of the bottom of the wet part of the
C--              neighboring cell.  If the seafloor reaches into
C--              the grid cell then the bottom of the wet part of the
C--              grid cell is at the seafloor.
C--
C--              * If the seafloor is deeper than this grid cell then
C--                wet_bottom_Z = rF(k+1)
C--              * If the seafloor is shallower than this grid cell then
C--                wet_bottom_Z = rF(k)
C--              * If the seafloor reaches partly into this grid cell
C--                then wet_bottom_Z = R_low

C--              [rF(k) >= wet_bottom_Z >= rF(k+1)]     (rF is negative)

              wet_bottom_Z_N = MIN(rF(k),
     &             MAX(R_low(CURI,CURJ, bi,bj), rF(k+1)))

C--   iceFrontWetContact_Z_max:  The deepest point where the
C--              the ice front at (i,j) is in contact with water
C--              in the neighboring cell.  The shallower of
C--              wet_bottom_Z_N (seafloor depth of neighboring point) and
C--              ice_bottom_Z_C (bottom of ice front in this center cell).

C--              * wet_bottom_Z_N if the seafloor of the neighboring
C--                cell is shallower than the ice draft at (i,j).
C--              * ice_bottom_Z_C if the ice draft at (i,j) is shallower
C--                than the seafloor of the neighboring cell.

              IF (ice_bottom_Z_C .GT. wet_bottom_Z_N) THEN
               iceFrontWetContact_Z_max = ice_bottom_Z_C
              ELSE
               iceFrontWetContact_Z_max = wet_bottom_Z_N
              ENDIF

C--   The shallowest depth where the ice front at (i,j) is in contact
C--   with water in the neighboring cell.  If the neighboring cell has
C--   no ice draft then wet_top_Z_N = rF(k), the top of the cell.
C--   Otherwise, the shallowest depth where the ice front at (i,j) can
C--   be in in contact with water (not ice) in (CURI, CURJ)
C--   is wet_top_Z_N.

C--   the fraction of the grid cell height that has ice draft in contact
C--   with water in the neighboring cell.
              iceFrontVertContactFrac =
     &             (wet_top_Z_N - iceFrontWetContact_Z_max)/ drF(k)
C--   Only proceed if iceFrontVertContactFrac is > 0, the
C--   ice draft at (i,j)
C--   is in contact with some water in the neighboring grid cell.
              IF (iceFrontVertContactFrac .GT. epsilon_H) THEN
               mask3dSHIICF(CURI,CURJ,k,bi,bj) = 1. _d 0
               mask2dSHIICF(CURI,CURJ,  bi,bj) = 1. _d 0
               mask3dICF   (CURI,CURJ,k,bi,bj) = 1. _d 0
               mask2dICF   (CURI,CURJ,  bi,bj) = 1. _d 0
              ENDIF /* iceFrontVertContactFrac */
             ENDIF /* hFacC(CURI,CURJ,k,bi,bj) */
            ENDDO /* SI loop for adjacent cells */
           ENDDO /* k loop */
          ENDIF /* FRONT_K */

C--   ice shelf
          k = kTopC(i,j,bi,bj)

C--   If there is an ice front at this (i,j) continue
C--   I am assuming k is only .GT. when there is at least some
C--   nonzero wet point below the shelf in the grid cell.
          IF (k .GT. 0) THEN
           mask3dSHIICF(i,j,k,bi,bj) = 1. _d 0
           mask2dSHIICF(i,j,  bi,bj) = 1. _d 0
           mask3dSHI   (i,j,k,bi,bj) = 1. _d 0
           mask2dSHI   (i,j,  bi,bj) = 1. _d 0
          ENDIF /* SHELF k > 0 */
         ENDDO /* i */
        ENDDO /* j */
       ENDDO /* bi */
      ENDDO /* bj */

C     fill in the halos
      _EXCH_XY_RS(  mask2dSHIICF, myThid )
      _EXCH_XY_RS(  mask2dICF   , myThid )
      _EXCH_XY_RS(  mask2dSHI   , myThid )
      _EXCH_XYZ_RS( mask3dSHIICF, myThid )
      _EXCH_XYZ_RS( mask3dICF   , myThid )
      _EXCH_XYZ_RS( mask3dSHI   , myThid )

C     output the masks
      CALL WRITE_FLD_XY_RS ( 'mask2dSHIICF',' ',mask2dSHIICF,-1,myThid)
      CALL WRITE_FLD_XYZ_RS( 'mask3dSHIICF',' ',mask3dSHIICF, 0,myThid)
      CALL WRITE_FLD_XY_RS ( 'mask2dSHI',' ',mask2dSHI,-1,myThid)
      CALL WRITE_FLD_XYZ_RS( 'mask3dSHI',' ',mask3dSHI, 0,myThid)
      CALL WRITE_FLD_XY_RS ( 'mask2dICF',' ',mask2dICF,-1,myThid)
      CALL WRITE_FLD_XYZ_RS( 'mask3dICF',' ',mask3dICF, 0,myThid)
      CALL WRITE_FLD_XY_RS ( 'R_stic',' ',R_stic,-1,myThid)
      CALL WRITE_FLD_XY_RS ( 'kIcf',' ',fK_icefront,-1,myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

# ifdef ALLOW_SHITRANSCOEFF_3D
       diagName  = 'SHIgam3T'
       diagTitle = '3D-Ice shelf exchange coefficient for theta'
       diagUnits = 'm/s             '
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIgam3S'
       diagTitle = '3D-Ice shelf exchange coefficient for salt'
       diagUnits = 'm/s             '
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
# endif

       diagName  = 'SHIICFfw'
       diagTitle = 'total ice shelf and front FW flux (+ upward)'
       diagUnits = 'kg/m^2/s        '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIICFht'
       diagTitle = 'total ice shelf and ice front heat flux (+ upward)'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'STCfwFlx'
       diagTitle ='Ice front freshwater flux (+ve increases ocean salt)'
       diagUnits = 'kg/m^2/s        '
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'STChtFlx'
       diagTitle = 'Ice front heat flux  (+ve cools ocean)'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'STCForcT'
       diagTitle = 'Ice front forcing for theta, >0 increases theta'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'STCForcS'
       diagTitle = 'Ice front forcing for salt, >0 increases salt'
       diagUnits = 'g/m^2/s         '
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C     these diagnostics are never filled
c      diagName  = 'SHIICFFT'
c      diagTitle = 'total SHI and ICF forcing for T, >0 increases theta'
c      diagUnits = 'W/m^2           '
c      diagCode  = 'SM      L1      '
c      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c      diagName  = 'SHIICFFS'
c      diagTitle = 'total SHI and ICF forcing for S, >0 increases salt'
c      diagUnits = 'g/m^2/s         '
c      diagCode  = 'SM      L1      '
c      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      ENDIF
#endif

      RETURN
      END
