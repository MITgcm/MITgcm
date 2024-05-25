#include "STIC_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: STIC_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE STIC_THERMODYNAMICS(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  STIC_THERMODYNAMICS
C     | o shelf-ice main routine.
C     |   compute temperature and (virtual) salt flux at the
C     |   shelf-ice ocean interface
C     |
C     | stresses at the ice/water interface are computed in separate
C     | routines that are called from mom_fluxform/mom_vecinv
C     |
CIGF  | ASSUMES
C---  |   * SHELFICEconserve = true
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
# include "SHELFICE_COST.h"
#endif
#include "STIC.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_DUMMY.h"
# ifdef ALLOW_GENTIM2D_CONTROL
#  include "CTRL_GENARR.h"
# endif
#endif /* ALLOW_CTRL */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES :
C     === Local variables ===
C     i,j,k,bi,bj      :: loop counters
C     tLoc, sLoc, pLoc :: local potential temperature, salinity, pressure
C     gammaT/SLoc      :: local heat and salt transfer coefficients
C     theta/saltFreeze :: temperature and salinity of water at the
C                         ice-ocean interface (at the freezing point)
C     iceFrontCellThickness   :: the ratio of the grid cell area to
C                         the horizontal length of the ice front.
C                         unit meters.  Approximately the length of the
C                         column perpendicular to the ice front extended
C                         to the far side of the tracer cell.
C     iceFrontWidth    :: the width of the ice front.  unit meters.

      INTEGER i, j, k
      INTEGER bi, bj
      INTEGER CURI, CURJ, FRONT_K

      _RL tLoc
      _RL sLoc
      _RL pLoc
      _RL gammaTLoc
      _RL gammaSLoc

      _RL ice_bottom_Z_C
      _RL wet_top_Z_N, wet_bottom_Z_N
      _RL iceFrontWetContact_Z_max
      _RL iceFrontVertContactFrac, iceFrontCellThickness
      _RL iceFrontWidth, iceFrontFaceArea
      _RL thermalConductionDistance, thermalConductionTemp
      _RL tmpHeatFlux, tmpFWFLX
      _RL tmpForcingT, tmpForcingS
      _RL tmpFac, icfgridareaFrac
      _RL tmpHeatFluxscaled, tmpFWFLXscaled
      INTEGER SI
      _RL insituT

#ifdef ALLOW_DIAGNOSTICS
# ifdef SHI_ALLOW_GAMMAFRICT
      _RL uStarDiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif
#ifdef NONLIN_FRSURF
      _RL tmpDiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpDiagT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      _RL tmpDiagIcfForcingT(1-OLx:sNx+OLx,
     & 1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpDiagIcfForcingS(1-OLx:sNx+OLx,
     & 1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpDiagShelficeForcingT(1-OLx:sNx+OLx,
     & 1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpDiagShelficeForcingS(1-OLx:sNx+OLx,
     & 1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIAGNOSTICS */

      _RL epsilon_H
      _RL stic_addMass(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
      _RL xx_shifwflx_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (depends on tiles)
C     ikey :: tape key (depends on ij-indices and tiles)
C     kkey :: tape key (depends on levels, ij-indices, and tiles)
      INTEGER tkey, ikey, kkey
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kTopC          = comlev1, key = ikey_dynamics
#endif

C--   minimum fraction of a cell adjacent to an ice front that must be
C--   wet for exchange to happen
      epsilon_H = 1. _d -03

C--   hard coded for now.
      thermalConductionDistance = 100.0 _d 0
      thermalConductionTemp     = -20.0 _d 0
      icfgridareaFrac = 1.0 _d 0
      insituT = 0. _d 0

C     heat flux into the ice shelf, default is diffusive flux
C     (Holland and Jenkins, 1999, eq.21)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          shelfIceHeatFlux        (i,j,bi,bj) = 0. _d 0
          shelfIceFreshWaterFlux  (i,j,bi,bj) = 0. _d 0
          sticfHeatFlux           (i,j,bi,bj) = 0. _d 0
          sticfFreshWaterFlux     (i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_GENTIM2D_CONTROL
          xx_shifwflx_loc         (i,j,bi,bj) = 0. _d 0
#endif /* ALLOW_GENTIM2D_CONTROL */
#ifndef ALLOW_SHITRANSCOEFF_3D
          shiTransCoeffS(i,j,bi,bj) = SHELFICEsaltToHeatRatio *
     &         shiTransCoeffT(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
        DO k = 1, Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
#ifdef ALLOW_SHITRANSCOEFF_3D
           shiTransCoeffS3d(i,j,k,bi,bj) = SHELFICEsaltToHeatRatio *
     &          shiTransCoeffT3d(i,j,k,bi,bj)
#endif
           icfHeatFlux(i,j,k,bi,bj)       = 0. _d 0
           icfFreshWaterFlux(i,j,k,bi,bj) = 0. _d 0
           stic_gT(i,j,k,bi,bj)           = 0. _d 0
           stic_gS(i,j,k,bi,bj)           = 0. _d 0
           stic_addMass(i,j,k,bi,bj)      = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           tmpDiagShelficeForcingT(i,j,bi,bj) = 0. _d 0
           tmpDiagShelficeForcingS(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
         DO k = 1, Nr
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
            tmpDiagIcfForcingT(i,j,k,bi,bj) = 0. _d 0
            tmpDiagIcfForcingS(i,j,k,bi,bj) = 0. _d 0
#ifdef NONLIN_FRSURF
            tmpDiag           (i,j,k,bi,bj) = 0. _d 0
            tmpDiagT          (i,j,k,bi,bj) = 0. _d 0
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
       ENDDO
      ENDDO

#if (defined ALLOW_CTRL && defined ALLOW_GENTIM2D_CONTROL)
      IF ( useCTRL ) THEN
       DO iarr = 1, maxCtrlTim2D
        IF (xx_gentim2d_file(iarr)(1:11).EQ.'xx_shifwflx') THEN
         DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             xx_shifwflx_loc(i,j,bi,bj)=xx_gentim2d(i,j,bi,bj,iarr)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
          tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif
C--   First ice front then ice shelf.  Loop through each i,j point
C--   process ice fronts in k, then process ice shelf.
          DO j = 1-OLy+1,sNy+OLy-1
            DO i = 1-OLx+1,sNx+OLx-1

#ifdef ALLOW_AUTODIFF_TAMC
              ikey = i + OLx + (j+OLy-1)*(sNx+2*OLx)
     &             + (tkey-1)*(sNx+2*OLx)*(sNy+2*OLy)
#endif

C--   The K index where the ice front ends (0 if no ice front)
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
                    CURI=CURI_ARR(i,j,bi,bj,SI)
                    CURJ=CURJ_ARR(i,j,bi,bj,SI)
                    iceFrontWidth=sticfWidth_arr(i,j,bi,bj,SI)

C--                 cell depth describes the average distance
C--                 perpendicular to the ice front fact

                    iceFrontCellThickness = 0. _d 0
                    IF(iceFrontWidth.NE.0. _d 0)
     &                iceFrontCellThickness = RA(CURI,CURJ,bi,bj)
     &                                  /iceFrontWidth
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
                      ice_bottom_Z_C = max(rF(k+1),
     &                  min(Ro_surf(i,j, bi,bj), rF(k)))

C--   wet_top_Z_N: the depth (m) of the bottom of the ice in the
C--              neighboring grid.  If the neighboring cell has ice in
C--              (in the form of a shelf or front) then wet_top_Z_N is
C--              the depth of this neighboring ice.
C--
C--              * If neighbor cell has no ice, then Ro_surf = 0 and
C--                wet_top_Z_N = rF(k)
C--              [rF(k) >= wet_top_Z_N >= rF(k+1)]     (rF is negative)

                      wet_top_Z_N = max(rF(k+1),
     &                 min(Ro_surf(CURI,CURJ, bi,bj), rF(k)))

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

                      wet_bottom_Z_N = min(rF(k),
     &                  max(R_low(CURI,CURJ, bi,bj), rF(k+1)))

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
     &                  (wet_top_Z_N - iceFrontWetContact_Z_max)/ drF(k)

C--   Only proceed if iceFrontVertContactFrac is > 0, the
C--   ice draft at (i,j)
C--   is in contact with some water in the neighboring grid cell.
                      IF (iceFrontVertContactFrac .GT. epsilon_H) THEN
                        tLoc = theta(CURI,CURJ,k,bi,bj)
                        sLoc = MAX(salt(CURI,CURJ,k,bi,bj), zeroRL)

C--   use pressure at the halfway point between the top and bottom of
C--   points of the ice front where the ice front is in contact with
C--   open water.
                        pLoc = 0.5 _d 0 * ABS(wet_top_Z_N +
     &                    iceFrontWetContact_Z_max)
#ifdef ALLOW_SHITRANSCOEFF_3D
                        gammaTLoc = shiTransCoeffT3d(CURI,CURJ,k,bi,bj)
                        gammaSLoc = shiTransCoeffS3d(CURI,CURJ,k,bi,bj)
#else
                        gammaTLoc = shiTransCoeffT(CURI,CURJ,bi,bj)
                        gammaSLoc = shiTransCoeffS(CURI,CURJ,bi,bj)
#endif

                        CALL STIC_SOLVE4FLUXES(
     I                    tLoc, sLoc, pLoc,
     I                    gammaTLoc, gammaSLoc,
     I                    thermalConductionDistance,
     I                    thermalConductionTemp,
     O                    tmpHeatFlux, tmpFWFLX,
     O                    tmpForcingT, tmpForcingS,
     O                    insituT,
     I                    bi, bj, myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
                        kkey = k + (ikey-1)*Nr
CADJ STORE tmpForcingT, tmpForcingS = comlev1_stic_bibj_ijk, key = kkey
#endif
C--   fluxes and forcing must be scaled by iceFrontVertContactFract and
C--   iceFrontContactFrac some fraction of the heigth and width of the
C--   grid cell face may not ice in contact with water.

C     tmpHeatFlux and tmpFWFLX come as W/m^2 and kg/m^2/s respectively
C--   but these rates only apply to the
C--   fraction of the grid cell that has ice in contact with seawater.
C--   we must scale by iceFrontVertContactFrac to get to the average
C--   fluxes in this grid cell.
C--   We also further scale by ratio of vertical to horizontal grid
C--   cell area so when comparing ice-front flux to ice-shelf flux we
C--   can just times them by the same area, i.e. horizontal grid cell area.

C--   ratio of vertical area to horizontal grid cell area
                        icfgridareaFrac =
     &                   iceFrontFaceArea/RA(CURI,CURJ,bi,bj)

C--   In units W/m^2
                        tmpHeatFluxscaled =
     &                    tmpHeatFlux*iceFrontVertContactFrac
     &                    *icfgridareaFrac
                        icfHeatFlux(CURI,CURJ,k,bi,bj) =
     &                    icfHeatFlux(CURI,CURJ,k,bi,bj) +
     &                    tmpHeatFluxscaled

C     In units of kg/s/m^2
                        tmpFWFLXscaled =
     &                    tmpFWFLX*iceFrontVertContactFrac
     &                    *icfgridareaFrac
                        icfFreshWaterFlux(CURI,CURJ,k,bi,bj) =
     &                    icfFreshWaterFlux(CURI,CURJ,k,bi,bj) +
     &                    tmpFWFLXscaled

C ow - 06/29/2018
C ow - Verticallly sum up the 3D icefront heat and freshwater fluxes to
C ow -  compute the total flux for the water column. The shelfice fluxes,
C ow -  which are 2D, will be added later. NOTE that only
C ow -  ice-front melts below shelf-ice are included to be consistent
C ow -  with Rignot's data
                  if(k.GE.kTopC(i,j,bi,bj))then
                   if(RA(CURI,CURJ,bi,bj).NE.0. _d 0)then
                        sticfHeatFlux(CURI,CURJ,bi,bj) =
     &                   sticfHeatFlux(CURI,CURJ,bi,bj) +
     &                   tmpHeatFluxscaled
                        sticfFreshWaterFlux(CURI,CURJ,bi,bj) =
     &                   sticfFreshWaterFlux(CURI,CURJ,bi,bj) +
     &                   tmpFWFLXscaled
                   endif
                  endif
C     stic_g[T,S] are tendency contributions due to ice front melt,
C--   but these constributions only apply to the
C--   fraction of the grid cell that has ice in contact with seawater.
C--   we must scale by iceFrontVertContactFrac to get to the average
C--   fluxes in this grid cell.  We must also divide the by the length
C--   of the grid cell perpendicular to the face.

                       IF (iceFrontCellThickness .NE. 0. _d 0) THEN
C     In units of K / s
                        stic_gT(CURI,CURJ,k,bi,bj) =
     &                    stic_gT(CURI,CURJ,k,bi,bj) +
     &                    tmpForcingT/iceFrontCellThickness*
     &                    iceFrontVertContactFrac*
     &                    _recip_hFacC(CURI,CURJ,k,bi,bj)
                        tmpDiagIcfForcingT(CURI,CURJ,k,bi,bj) =
     &                    tmpDiagIcfForcingT(CURI,CURJ,k,bi,bj) +
     &                    tmpForcingT/iceFrontCellThickness*
     &                    iceFrontVertContactFrac*
     &                    drF(k)
C     In units of psu /s
                        stic_gS(CURI,CURJ,k,bi,bj) =
     &                    stic_gS(CURI,CURJ,k,bi,bj) +
     &                    tmpForcingS/iceFrontCellThickness*
     &                    iceFrontVertContactFrac*
     &                    _recip_hFacC(CURI,CURJ,k,bi,bj)
                        tmpDiagIcfForcingS(CURI,CURJ,k,bi,bj) =
     &                    tmpDiagIcfForcingS(CURI,CURJ,k,bi,bj) +
     &                    tmpForcingS/iceFrontCellThickness*
     &                    iceFrontVertContactFrac*
     &                    drF(k)
                       ENDIF /* iceFrontCellThickness */
C     In units of kg /s
                         stic_addMass(CURI,CURJ,k,bi,bj) =
     &                     stic_addMass(CURI,CURJ,k,bi,bj) -
     &                     tmpFWFLX*iceFrontFaceArea*
     &                     iceFrontVertContactFrac
#ifdef ALLOW_DIAGNOSTICS
                       IF ( useDiagnostics ) THEN
#ifdef NONLIN_FRSURF
                        IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &                   .AND. useRealFreshWaterFlux ) THEN
                         tmpDiagT(CURI,CURJ,k,bi,bj) =
     &                     stic_addMass(CURI,CURJ,k, bi,bj)/
     &                     RA(CURI,CURJ,bi,bj)*
     &                     insituT*HeatCapacity_Cp
                        ENDIF
#endif /* NONLIN_FRSURF */
                       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
                      ENDIF /* iceFrontVertContactFrac */
                    ENDIF /* hFacC(CURI,CURJ,k,bi,bj) */
                  ENDDO /* SI loop for adjacent cells */
                ENDDO /* k Loop */
              ENDIF /* FRONT_K */

C--   ice shelf
              k = kTopC(i,j,bi,bj)

C--   If there is an ice front at this (i,j) continue
C--   I am assuming K is only .GT. when there is at least some
C--   nonzero wet point below the shelf in the grid cell.
              IF (k .GT. 0) THEN
C--   Initialize these values to zero
                pLoc = 0. _d 0
                tLoc = 0. _d 0
                sLoc = 0. _d 0
                gammaTLoc = 0. _d 0
                gammaSLoc = 0. _d 0

C--   make local copies of temperature, salinity and depth
C--   (pressure in deci-bar) underneath the ice
C--   for the ice shelf case we use hydrostatic pressure at the ice
C--   base of the ice shelf, top of the cavity.

                pLoc = ABS(R_shelfIce(i,j,bi,bj))
                tLoc = theta(i,j,k,bi,bj)
                sLoc = MAX(salt(i,j,k,bi,bj), zeroRL)
#ifdef ALLOW_SHITRANSCOEFF_3D
                gammaTLoc = shiTransCoeffT3d(i,j,k,bi,bj)
                gammaSLoc = shiTransCoeffS3d(i,j,k,bi,bj)
#else
                gammaTLoc = shiTransCoeffT(i,j,bi,bj)
                gammaSLoc = shiTransCoeffS(i,j,bi,bj)
#endif
                CALL STIC_SOLVE4FLUXES(
     I            tLoc, sLoc, pLoc,
     I            gammaTLoc, gammaSLoc,
     I            pLoc, thermalConductionTemp,
     O            tmpHeatFlux, tmpFWFLX,
     O            tmpForcingT, tmpForcingS,
     O            insituT,
     I            bi, bj, myTime, myIter, myThid )
C     In units of W/m^2
                shelficeHeatFlux(i,j,bi,bj) = tmpHeatFlux
#ifdef ALLOW_GENTIM2D_CONTROL
     &           - xx_shifwflx_loc(i,j,bi,bj)*SHELFICElatentHeat
#endif /*  ALLOW_GENTIM2D_CONTROL */
C     In units of kg/m^2/s
                shelfIceFreshWaterFlux(i,j,bi,bj) = tmpFWFLX
#ifdef ALLOW_GENTIM2D_CONTROL
     &           + xx_shifwflx_loc(i,j,bi,bj)
#endif /*  ALLOW_GENTIM2D_CONTROL */
C ow - 06/29/2018
C ow - Now add shelfice heat and freshwater fluxes
                        sticfHeatFlux(i,j,bi,bj) =
     &                   sticfHeatFlux(i,j,bi,bj) +
     &                   shelficeHeatFlux(i,j,bi,bj)
                        sticfFreshWaterFlux(i,j,bi,bj) =
     &                   sticfFreshWaterFlux(i,j,bi,bj) +
     &                   shelfIceFreshWaterFlux(i,j,bi,bj)
C     In units of K/s
                stic_gT(i,j,k,bi,bj) = stic_gT(i,j,k,bi,bj) +
     &              tmpForcingT*recip_drF(k)* _recip_hFacC(i,j,k,bi,bj)
                tmpDiagShelficeForcingT(i,j,bi,bj) = tmpForcingT
C     In units of psu/s
                stic_gS(i,j,k,bi,bj) = stic_gS(i,j,k,bi,bj) +
     &              tmpForcingS*recip_drF(k)* _recip_hFacC(i,j,k,bi,bj)
                tmpDiagShelficeForcingS(i,j,bi,bj) = tmpForcingS
C     In units of kg/s  -- multiplication of area required first
                stic_addMass(i,j,k,bi,bj) = stic_addMass(i,j,k,bi,bj) -
     &              tmpFWFLX*RA(i,j,bi,bj)
#ifdef ALLOW_DIAGNOSTICS
                IF ( useDiagnostics ) THEN
#ifdef NONLIN_FRSURF
                 IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &            .AND. useRealFreshWaterFlux ) THEN
                  tmpDiagT(i,j,k,bi,bj) =
     &                 stic_addMass(i,j,k,bi,bj) / rA(i,j,bi,bj)*
     &                 insituT*HeatCapacity_Cp
                 ENDIF
#endif /* NONLIN_FRSURF */
                ENDIF
#endif /* ALLOW_DIAGNOSTICS */
              ENDIF /* Shelf k > 0 */
            ENDDO /* i */
          ENDDO /* j */
        ENDDO /* bi */
      ENDDO /* bj */

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy+1,sNy+OLy-1
            DO i = 1-OLx+1,sNx+OLx-1
              DO k = 1,Nr
                 addMass(i,j,k,bi,bj) = addMass(i,j,k,bi,bj) +
     &               stic_addMass(i,j,k,bi,bj)
              ENDDO /* k */
            ENDDO /* i */
          ENDDO /* j */
        ENDDO /* bi */
      ENDDO /* bj */

#ifdef ALLOW_ADDFLUID
      IF ( selectAddFluid.NE.0 .AND. salt_addMass.NE.UNSET_RL ) THEN
        IF ( ( selectAddFluid.GE.1 .AND. nonlinFreeSurf.GT.0 )
     &       .OR. convertFW2Salt.EQ.-1. _d 0 ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              DO j=0,sNy+1
                DO i=0,sNx+1
                  DO k=1,Nr
                    stic_gT(i,j,k,bi,bj) = stic_gT(i,j,k,bi,bj)
     &                - stic_addMass(i,j,k,bi,bj)*mass2rUnit
     &                  *( temp_addMass - theta(i,j,k,bi,bj) )
     &                  *recip_rA(i,j,bi,bj)
     &                  *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &                  *recip_deepFac2C(k)*recip_rhoFacC(k)
                    stic_gS(i,j,k,bi,bj) = stic_gS(i,j,k,bi,bj)
     &                - stic_addMass(i,j,k,bi,bj)*mass2rUnit
     &                  *( salt_addMass - salt(i,j,k,bi,bj) )
     &                  *recip_rA(i,j,bi,bj)
     &                  *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &                  *recip_deepFac2C(k)*recip_rhoFacC(k)
                  ENDDO /* k */
                ENDDO /* i */
              ENDDO /* j */
            ENDDO /* bi */
          ENDDO /* bj */
        ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              DO j=0,sNy+1
                DO i=0,sNx+1
                  DO k=1,Nr
                    stic_gT(i,j,k,bi,bj) = stic_gT(i,j,k,bi,bj)
     &                - stic_addMass(i,j,k,bi,bj)*mass2rUnit
     &                  *( temp_addMass - tRef(k) )
     &                  *recip_rA(i,j,bi,bj)
     &                  *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &                  *recip_deepFac2C(k)*recip_rhoFacC(k)
                    stic_gS(i,j,k,bi,bj) = stic_gS(i,j,k,bi,bj)
     &                - stic_addMass(i,j,k,bi,bj)*mass2rUnit
     &                  *( salt_addMass - sRef(k) )
     &                  *recip_rA(i,j,bi,bj)
     &                  *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
C    &                  *recip_deepFac2C(k)*recip_rhoFacC(k)
                  ENDDO /* k */
                ENDDO /* i */
              ENDDO /* j */
            ENDDO /* bi */
          ENDDO /* bj */
        ENDIF
      ENDIF
#endif /* ALLOW_ADDFLUID */

C--  Calculate new loading anomaly (in case the ice-shelf mass was updated)
#ifndef ALLOW_AUTODIFF
c     IF ( SHELFICEloadAnomalyFile .EQ. ' ' ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           shelficeLoadAnomaly(i,j,bi,bj) = gravity
     &      *( shelficeMass(i,j,bi,bj) + rhoConst*Ro_surf(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c     ENDIF
#endif /* ndef ALLOW_AUTODIFF */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

#ifdef NONLIN_FRSURF
       IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &         .AND. useRealFreshWaterFlux ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, Nr
           DO j=1,sNy
            DO i=1,sNx
             tmpDiag(i,j,k,bi,bj) =
     &        stic_addMass(i,j,k,bi,bj)/RA(i,j,bi,bj)*
     &        theta(i,j,k,bi,bj)*HeatCapacity_Cp
            ENDDO
           ENDDO
          ENDDO
         ENDDO /* bi */
        ENDDO /* bj */
        CALL DIAGNOSTICS_FILL( tmpDiag,'TFLUXMLT',0,Nr,0,1,1,myThid )

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, Nr
           DO j=1,sNy
            DO i=1,sNx
             tmpDiag(i,j,k,bi,bj) =
     &        stic_addMass(i,j,k,bi,bj)/RA(i,j,bi,bj)*salt(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO /* bi */
        ENDDO /* bj */
        CALL DIAGNOSTICS_FILL( tmpDiag,'SFLUXMLT',0,Nr,0,1,1,myThid )

        CALL DIAGNOSTICS_FILL( tmpDiagT,'TFLUXMTI',0,Nr,0,1,1,myThid )
       ENDIF
#endif /* NONLIN_FRSURF */

       CALL DIAGNOSTICS_FILL_RS(shelfIceFreshWaterFlux,'SHIfwFlx',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL_RS(shelfIceHeatFlux,      'SHIhtFlx',
     &      0,1,0,1,1,myThid)

#ifdef ALLOW_STEEP_ICECAVITY
       CALL DIAGNOSTICS_FILL( sticfFreshWaterFlux, 'SHIICFfw',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL( sticfHeatFlux,       'SHIICFht',
     &      0,1,0,1,1,myThid)
#endif

        CALL DIAGNOSTICS_FILL(icfFreshWaterFlux, 'STCfwFlx',
     &      0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(icfHeatFlux, 'STChtFlx',
     &      0,Nr,0,1,1,myThid)

C     SHIForcT (Ice shelf forcing for theta [W/m2], >0 increases theta)
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(tmpDiagShelficeForcingT,tmpFac,1,
     &      'SHIForcT',0,1,0,1,1,myThid)
C     SHIForcS (Ice shelf forcing for salt [g/m2/s], >0 increases salt)
       tmpFac = rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(tmpDiagShelficeForcingS,tmpFac,1,
     &      'SHIForcS',0,1,0,1,1,myThid)

C     STCForcT (Ice front forcing for theta [W/m2], >0 increases theta)
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(tmpDiagIcfForcingT,tmpFac,1,
     &      'STCForcT',0,Nr,0,1,1,myThid)
C     STCForcS (Ice front forcing for salt [g/m2/s], >0 increases salt)
       tmpFac = rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(tmpDiagIcfForcingS,tmpFac,1,
     &      'STCForcS',0,Nr,0,1,1,myThid)

C     Transfer coefficients
#ifdef ALLOW_SHITRANSCOEFF_3D
       CALL DIAGNOSTICS_FILL(shiTransCoeffT3d,'SHIgam3T',
     &      0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(shiTransCoeffS3d,'SHIgam3S',
     &      0,Nr,0,1,1,myThid)
#else
       CALL DIAGNOSTICS_FILL(shiTransCoeffT,'SHIgammT',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(shiTransCoeffS,'SHIgammS',
     &      0,1,0,1,1,myThid)
#endif
C     Friction velocity
#ifdef SHI_ALLOW_GAMMAFRICT
       IF ( SHELFICEuseGammaFrict )
     &  CALL DIAGNOSTICS_FILL(uStarDiag,'SHIuStar',0,1,0,1,1,myThid)
#endif /* SHI_ALLOW_GAMMAFRICT */

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
