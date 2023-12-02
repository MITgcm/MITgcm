#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE SHELFICE_INIT_FIXED( myThid )
C     *============================================================*
C     | SUBROUTINE SHELFICE_INIT_FIXED
C     | o Routine to initialize SHELFICE parameters and variables.
C     *============================================================*
C     | Initialize SHELFICE parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
#ifdef ALLOW_COST
# include "cost.h"
# include "SHELFICE_COST.h"
#endif /* ALLOW_COST */

C     === Routine arguments ===
C     myThid ::  Number of this instance of SHELFICE_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     === Local variables ===
C     i, j, bi, bj :: Loop counters
      INTEGER i, j, bi, bj
#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_CTRL
      INTEGER k
# ifdef ALLOW_SHIFWFLX_COST_CONTRIBUTION
      _RL dummy
# endif
#endif

C local variabls used to determine shelf-ice and ice-front masks
C     iceFrontCellThickness   :: the ratio of the horizontal length
C                         of the ice front in each model grid cell
C                         divided by the grid cell area.  The "thickness"
C                         of the colum perpendicular to the front
C     iceFrontWidth    :: the width of the ice front.

      INTEGER CURI, CURJ, FRONT_K
      _RL ice_bottom_Z_C
      _RL wet_top_Z_N, wet_bottom_Z_N
      _RL iceFrontWetContact_Z_max
      _RL iceFrontContact_H
      _RL iceFrontVertContactFrac, iceFrontCellThickness
      _RL iceFrontWidth, iceFrontFaceArea
      _RS fK_icefront (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER SI
      _RL epsilon_H
      character*200 msgBuf

#ifdef ALLOW_MNC
C     Initialize MNC variable information for SHELFICE
      IF ( useMNC .AND. (shelfice_tave_mnc.OR.shelfice_dump_mnc)
     &    ) THEN
        CALL SHELFICE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

C-----------------------------------------------------------------------
C--   Initialize SHELFICE variables kTopC
C--   kTopC is the same as kSurfC, except outside ice-shelf area:
C--   kTop = 0 where there is no ice-shelf (where kSurfC=1)
C--   and over land (completely dry column) where kSurfC = Nr+1
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &         Ro_surf(i,j,bi,bj).LT.rF(1) ) THEN
            kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
          ELSE
            kTopC(i,j,bi,bj) = 0
          ENDIF
          shelficeMassInit   (i,j,bi,bj) = 0. _d 0
          shelficeLoadAnomaly(i,j,bi,bj) = 0. _d 0
          shelfIceMassDynTendency(i,j,bi,bj) = 0. _d 0
          icefrontlength(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_CTRL
C     maskSHI is a hack to play along with the general ctrl-package
C     infrastructure, where only the k=1 layer of a 3D mask is used
C     for 2D fields. We cannot use maskInC instead, because routines
C     like ctrl_get_gen and ctrl_set_unpack_xy require 3D masks.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           maskSHI(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( Ro_surf(i,j,bi,bj).LT.rF(1)
     &          .AND. maskC(i,j,k,bi,bj).NE.zeroRS ) THEN
            maskSHI(i,j,k,bi,bj) = 1. _d 0
            maskSHI(i,j,1,bi,bj) = 1. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_CTRL */

C 06/29/2018
C ow - maskSHI above is not consistent with the spirit of gencost. Use mask2dSHI and mask3dSHI below
C ow - instead.
C ow - mask2dSHI and mask3dSHI are the 2d and 3d mask for shelfice. They are zero if there is no
C ow - shelfice and one if otherwise. For any i,j, if there is at least one non-zero mask3dSHI in
C ow - the vertical, then mask2dSHI at i,j is one.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           mask3dSHIICF(i,j,k,bi,bj) = 0. _d 0
           mask3dSHI(i,j,k,bi,bj) = 0. _d 0
           mask3dICF(i,j,k,bi,bj) = 0. _d 0
           if(k.eq.1)then
             mask2dSHIICF(i,j,bi,bj) = 0. _d 0
             mask2dSHI(i,j,bi,bj) = 0. _d 0
             mask2dICF(i,j,bi,bj) = 0. _d 0
           endif
          ENDDO
         ENDDO
        ENDDO

        DO J = 1-OLy,sNy+OLy
          DO I = 1-OLx,sNx+OLx
             DO SI = 1,4
                CURI_ARR(I,J,bi,bj,SI) = -9999
                CURJ_ARR(I,J,bi,bj,SI) = -9999
                icefrontwidth_arr(I,J,bi,bj,SI) = 0. _d 0
             ENDDO /* SI */
          ENDDO /* I */
        ENDDO /* J */

       ENDDO
      ENDDO


#ifdef ALLOW_COST
#if  (defined (ALLOW_SHIFWFLX_COST_CONTRIBUTION) && \
      defined (ALLOW_SHIFWFLX_CONTROL))
      IF ( shifwflx_errfile .NE. ' ' ) THEN
       CALL READ_REC_XY_RL( shifwflx_errfile, wshifwflx, 1, 0, myThid )
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
c--   Test for missing values.
          IF (wshifwflx(i,j,bi,bj) .LT. -9900.) THEN
           wshifwflx(i,j,bi,bj) = 0. _d 0
          ENDIF
c--   use weight as mask
          wshifwflx(i,j,bi,bj) =
     &         max(wshifwflx(i,j,bi,bj),wshifwflx0)
     &         *maskSHI(i,j,1,bi,bj)
          IF (wshifwflx(i,j,bi,bj) .NE. 0.) THEN
           wshifwflx(i,j,bi,bj) =
     &          1./wshifwflx(i,j,bi,bj)/wshifwflx(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL ACTIVE_WRITE_XY_LOC( 'wshifwflx', wshifwflx,
     &                          1, 0, myThid, dummy )
#endif /* ALLOW_SHIFWFLX_COST_CONTRIBUTION and ALLOW_SHIFWFLX_CONTROL */
#endif /* ALLOW_COST */

      IF ( SHELFICEloadAnomalyFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RL( SHELFICEloadAnomalyFile, ' ',
     &                      shelficeLoadAnomaly, 0, myThid )
      ENDIF
      IF ( SHELFICEmassFile.NE.' ' ) THEN
       CALL READ_FLD_XY_RL( SHELFICEmassFile, ' ',
     &                      shelficeMassInit, 0, myThid )
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1, sNy
          DO i = 1, sNx
           shelficeMassInit(i,j,bi,bj) =
     &         shelficeLoadAnomaly(i,j,bi,bj)*recip_gravity
     &       - rhoConst*Ro_surf(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      _EXCH_XY_RL( shelficeMassInit, myThid )
      CALL WRITE_FLD_XY_RL ( 'shelficemassinit', ' ',
     &                       shelficeMassInit, 0, myThid )

c     IF ( SHELFICEloadAnomalyFile .EQ. ' ' ) THEN
C-   In case we need shelficeLoadAnomaly in phi0surf for initial pressure
C    calculation (if using selectP_inEOS_Zc=2 or 3)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           shelficeLoadAnomaly(i,j,bi,bj) = gravity
     &      *(shelficeMassInit(i,j,bi,bj)+rhoConst*Ro_surf(i,j,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c     ELSE
c      _EXCH_XY_RS( shelficeLoadAnomaly, myThid )
c     ENDIF
      IF ( debugLevel.GE.debLevC ) THEN
       CALL WRITE_FLD_XY_RL( 'SHICE_pLoadAnom', ' ',
     I                       shelficeLoadAnomaly, -1, myThid )
      ENDIF

      IF ( SHELFICEMassStepping .AND.
     &     SHELFICEMassDynTendFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( SHELFICEMassDynTendFile, ' ',
     &                      shelfIceMassDynTendency, 0, myThid )
      ENDIF

C--  ICEFRONT parameters (BEGIN)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          K_icefront(i,j,bi,bj) = 0
          DO K = 1 , Nr
           IF ( R_icefront(I,J,bi,bj) .GT. ABS(rF(K)))
     &          K_icefront(I,J,bi,bj) = K
          ENDDO
          fK_icefront(i,j,bi,bj) = 0.+K_icefront(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C--  ICEFRONT parameters (END)

C create masks for shelf-ice and ice-front by modifyig code from shelfice_thermodynamics.F
C--   minimum fraction of a cell adjacent to an ice front that must be
C--   wet for exchange to happen
      epsilon_H = 1. _d -03

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C--   First ice front then ice shelf.  Loop through each i,j point
C--   process ice fronts in k, then process ice shelf.
          DO J = 1-OLy+1,sNy+OLy-1
            DO I = 1-OLx+1,sNx+OLx-1

C--   The K index where the ice front ends (0 if no ice front)
              FRONT_K = K_icefront(I,J,bi,bj)

C--   If there is an ice front at this (I,J) continue
              IF (FRONT_K .GT. 0) THEN

C--   Loop through all depths where the ice front is fround
                DO K = 1, FRONT_K
C--   Loop around the four laterally neighboring cells of the ice front.
C--   If any neighboring points has wet volume in contact with the ice
C--   front at (I,J) then calculate ice-ocean exchanges.
C--   The four laterally neighboring point are at (CURI,CURJ)
                  DO SI = 1,4
                    IF     (SI .EQ. 1) THEN
C--   Looking to right
                      CURI = I+1
                      CURJ = J

                      iceFrontWidth     = dyG(I+1,J,bi,bj)

                    ELSEIF (SI .EQ. 2) THEN
C--   Looking to LEFT
                      CURI = I-1
                      CURJ = J

                      iceFrontWidth     = dyG(I,J,bi,bj)
                    ELSEIF (SI .EQ. 3) THEN
C--   Looking to NORTH
                      CURI = I
                      CURJ = J+1

                      iceFrontWidth     = dxG(I,J+1,bi,bj)
                    ELSEIF (SI .EQ. 4) THEN
C--   Looking to south
                      CURI = I
                      CURJ = J-1

                      iceFrontWidth     = dxG(I,J,bi,bj)
                    endif

                    CURI_ARR(I,J,bi,bj,SI) = CURI
                    CURJ_ARR(I,J,bi,bj,SI) = CURJ
                    iceFrontWidth_arr(I,J,bi,bj,SI) = iceFrontWidth

C--                 cell depth describes the average distance
C--                 perpendicular to the ice front fact

                    iceFrontCellThickness = RA(CURI,CURJ,bi,bj)
     &                                  /iceFrontWidth
                    iceFrontFaceArea  = DRF(K)*iceFrontWidth

C--   First, make sure the adjacent point has at least some water in it.
                    IF (_hFacC(CURI,CURJ,K,bi,bj) .GT. zeroRL) THEN

C--   we need to determine how much of the ice front is in contact with
C--   water in the neighboring grid cell at this depth level.

C--   1. Determine the top depth with water in the current cell
C--   2. Determine the top depth with water in the neighbor cell
C--   3. Determine the depth where water  gap between (1) and (2).
C--   4. If there is a gap then ice front is in contact with water in
C--      the neighboring cell

C--   ice_bottom_Z_C: the depth (m) of the bottom of the ice in the
C--               current cell.  Bounded between rF(K) and rF(K+1).
C--               * If the ice extends past the bottom of the cell then
C--                 ice_bottom_Z_C = rF(K+1)
C--               [rF(k) >= ice_bottom_Z_C >= rF(K+1)]  (rF is negative)
                      ice_bottom_Z_C = max(rF(K+1),
     &                  min(Ro_surf(I,J, bi,bj), rF(K)))

C--   wet_top_Z_N: the depth (m) of the bottom of the ice in the
C--              neighboring grid.  If the neighboring cell has ice in
C--              (in the form of a shelf or front) then wet_top_Z_N is
C--              the depth of this neighboring ice.
C--
C--              * If neighbor cell has no ice, then Ro_surf = 0 and
C--                wet_top_Z_N = rF(K)
C--              [rF(k) >= wet_top_Z_N >= rF(K+1)]     (rF is negative)

                      wet_top_Z_N = max(rF(K+1),
     &                 min(Ro_surf(CURI,CURJ, bi,bj), rF(K)))

C--   wet_bottom_Z_N: the depth (m) of the bottom of the wet part of the
C--              neighboring cell.  If the seafloor reaches into
C--              the grid cell then the bottom of the wet part of the
C--              grid cell is at the seafloor.
C--
C--              * If the seafloor is deeper than this grid cell then
C--                wet_bottom_Z = rF(K+1)
C--              * If the seafloor is shallower than this grid cell then
C--                wet_bottom_Z = rF(K)
C--              * If the seafloor reaches partly into this grid cell
C--                then wet_bottom_Z = R_low

C--              [rF(k) >= wet_bottom_Z >= rF(K+1)]     (rF is negative)

                      wet_bottom_Z_N = min(rF(K),
     &                  max(R_low(CURI,CURJ, bi,bj), rF(K+1)))

C--   iceFrontWetContact_Z_max:  The deepest point where the
C--              the ice front at (I,J) is in contact with water
C--              in the neighboring cell.  The shallower of
C--              wet_bottom_Z_N (seafloor depth of neighboring point) and
C--              ice_bottom_Z_C (bottom of ice front in this center cell).

C--              * wet_bottom_Z_N if the seafloor of the neighboring
C--                cell is shallower than the ice draft at (I,J).
C--              * ice_bottom_Z_C if the ice draft at (I,J) is shallower
C--                than the seafloor of the neighboring cell.

                      IF (ice_bottom_Z_C .GT. wet_bottom_Z_N) THEN
                        iceFrontWetContact_Z_max = ice_bottom_Z_C
                      ELSE
                        iceFrontWetContact_Z_max = wet_bottom_Z_N
                      ENDIF

C--   The shallowest depth where the ice front at (I,J) is in contact
C--   with water in the neighboring cell.  If the neighboring cell has
C--   no ice draft then wet_top_Z_N = rF(k), the top of the cell.
C--   Otherwise, the shallowest depth where the ice front at (I,J) can
C--   be in in contact with water (not ice) in (CURI, CURJ)
C--   is wet_top_Z_N.

C--   the fraction of the grid cell height that has ice draft in contact
C--   with water in the neighboring cell.
                      iceFrontVertContactFrac =
     &                  (wet_top_Z_N - iceFrontWetContact_Z_max)/ DRF(K)
C--   Only proceed if iceFrontVertContactFrac is > 0, the
C--   ice draft at (I,J)
C--   is in contact with some water in the neighboring grid cell.
      WRITE(msgBuf,'(A,3I4,L3,8e14.5)')
     &'ZZZ, CURI, CURJ, K', CURI,CURJ,K,
     & iceFrontVertContactFrac .GT. epsilon_H,
     &  wet_top_Z_N, iceFrontWetContact_Z_max,
     &  ice_bottom_Z_C, wet_bottom_Z_N,
     &  rF(K+1), Ro_surf(I,J, bi,bj), rF(K)

      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

                      IF (iceFrontVertContactFrac .GT. epsilon_H) THEN
                        mask3dSHIICF(CURI,CURJ,K,bi,bj) = 1. _d 0
                        mask2dSHIICF(CURI,CURJ,bi,bj) = 1. _d 0
                        mask3dICF(CURI,CURJ,K,bi,bj) = 1. _d 0
                        mask2dICF(CURI,CURJ,bi,bj) = 1. _d 0
                      ENDIF /* iceFrontVertContactFrac */
                    ENDIF /* hFacC(CURI,CURJ,K,bi,bj) */
                  ENDDO /* SI loop for adjacent cells */
                ENDDO /* K LOOP */
              ENDIF /* FRONT K */

C--   ice shelf
              K = kTopC(I,J,bi,bj)

C--   If there is an ice front at this (I,J) continue
C--   I am assuming K is only .GT. when there is at least some
C--   nonzero wet point below the shelf in the grid cell.
              IF (K .GT. 0) THEN
                   mask3dSHIICF(I,J,K,bi,bj) = 1. _d 0
                   mask2dSHIICF(I,J,bi,bj) = 1. _d 0
                   mask3dSHI(I,J,K,bi,bj) = 1. _d 0
                   mask2dSHI(I,J,bi,bj) = 1. _d 0
              ENDIF /* SHELF K > 0 */
            ENDDO /* i */
          ENDDO /* j */
        ENDDO /* bi */
      ENDDO /* bj */

c fill in the hilos
      _EXCH_XY_RS (mask2dSHIICF    , myThid )
      _EXCH_XY_RS (mask2dICF       , myThid )
      _EXCH_XY_RS (mask2dSHI       , myThid )
      _EXCH_XYZ_RS(mask3dSHIICF    , myThid )
      _EXCH_XYZ_RS(mask3dICF       , myThid )
      _EXCH_XYZ_RS(mask3dSHI       , myThid )

C output the masks
      CALL WRITE_FLD_XY_RS( 'mask2dSHIICF',' ',mask2dSHIICF,-1,myThid)
      CALL WRITE_FLD_XYZ_RS( 'mask3dSHIICF',' ',mask3dSHIICF, 0,myThid)
      CALL WRITE_FLD_XY_RS( 'mask2dSHI',' ',mask2dSHI,-1,myThid)
      CALL WRITE_FLD_XYZ_RS( 'mask3dSHI',' ',mask3dSHI, 0,myThid)
      CALL WRITE_FLD_XY_RS( 'mask2dICF',' ',mask2dICF,-1,myThid)
      CALL WRITE_FLD_XYZ_RS( 'mask3dICF',' ',mask3dICF, 0,myThid)
      CALL WRITE_FLD_XY_RS( 'R_icefront',' ',R_icefront,-1,myThid)
      CALL WRITE_FLD_XY_RS( 'K_icefront',' ',fK_icefront,-1,myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       diagName  = 'SHIfwFlx'
       diagTitle = 'Ice shelf fresh water flux (positive upward)'
       diagUnits = 'kg/m^2/s        '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIhtFlx'
       diagTitle = 'Ice shelf heat flux  (positive upward)'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

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

      diagName  = 'ICFfwFlx'
      diagTitle = 'Ice front freshwater flux (+ve increases ocean salt)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ICFhtFlx'
      diagTitle = 'Ice front heat flux  (+ve cools ocean)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHI_TauX'
       diagTitle =
     &     'Ice shelf bottom stress, zonal  comp., >0 increases uVel'
       diagUnits = 'N/m^2           '
       diagCode  = 'UU      L1      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'SHI_TauY'
       diagTitle =
     &     'Ice shelf bottom stress, merid. comp., >0 increases vVel'
       diagUnits = 'N/m^2           '
       diagCode  = 'VV      L1      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'SHIForcT'
       diagTitle = 'Ice shelf forcing for theta, >0 increases theta'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIForcS'
       diagTitle = 'Ice shelf forcing for salt, >0 increases salt'
       diagUnits = 'g/m^2/s         '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'ICFForcT'
       diagTitle = 'Ice front forcing for theta, >0 increases theta'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'ICFForcS'
       diagTitle = 'Ice front forcing for salt, >0 increases salt'
       diagUnits = 'g/m^2/s         '
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIICFFT'
       diagTitle = 'total SHI and ICF forcing for T, >0 increases theta'
       diagUnits = 'W/m^2           '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIICFFS'
       diagTitle = 'total SHI and ICF forcing for S, >0 increases salt'
       diagUnits = 'g/m^2/s         '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifndef ALLOW_shiTransCoeff_3d
       diagName  = 'SHIgammT'
       diagTitle = 'Ice shelf exchange coefficient for theta'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIgammS'
       diagTitle = 'Ice shelf exchange coefficient for salt'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#else
       diagName  = 'SHIgammT'
       diagTitle = 'Ice shelf exchange coefficient for theta'
       diagUnits = 'm/s             '
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHIgammS'
       diagTitle = 'Ice shelf exchange coefficient for salt'
       diagUnits = 'm/s             '
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

       diagName  = 'SHI_mass'
       diagTitle = 'dynamic ice shelf mass for surface load anomaly'
       diagUnits = 'kg/m^2          '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef SHI_ALLOW_GAMMAFRICT
       diagName  = 'SHIuStar'
       diagTitle = 'Friction velocity at bottom of ice shelf'
       diagUnits = 'm/s             '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SHICDrag'
       diagTitle = 'Shelfice drag coefficient for u* parameterization'
       diagUnits = '1               '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

#ifdef ALLOW_SHELFICE_REMESHING
       diagName  = 'SHIRshel'
       diagTitle = 'depth of shelfice'
       diagUnits = 'm               '
       diagCode  = 'SM      L1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

# ifdef ALLOW_AUTODIFF
#  ifndef SHI_ALLOW_GAMMAFRICT
       diagName  = 'ADJshict'
       diagTitle = 'dJ/dgammaT: Sens. to shelfice heat transfer coeff'
       diagUnits = 'dJ/(m/s)        '
       diagCode  = 'SM A    M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'ADJshics'
       diagTitle = 'dJ/dgammaS: Sens. to shelfice salt transfer coeff'
       diagUnits = 'dJ/(m/s)        '
       diagCode  = 'SM A    M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#  else
       diagName  = 'ADJshicd'
       diagTitle = 'dJ/dcDrag: Sensitivity to shelfice u* drag coeff'
       diagUnits = 'dJ/1            '
       diagCode  = 'SM A    M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#  endif
# endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
