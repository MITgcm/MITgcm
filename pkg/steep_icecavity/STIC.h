CBOP
C     !ROUTINE: STIC.h
C     !INTERFACE:
C     #include "STIC.h"

C !DESCRIPTION:
C     *================================================================*
C     | STIC.h
C     | o Basic header thermodnynamic steep ice cavity package.
C     |   Contains all STEEP_ICECAVITY field declarations.
C     | o this package can only be used as an add-on to pkg/shelfice
C     |   because it uses most of the shelfice-infrastructure
C     *================================================================*

C-----------------------------------------------------------------------
C--   Constants that can be set in data.shelfice
C     icfThetaHorizDiffusionLength ::
C     icfThetaInterior  ::
C     STICdepthFile     :: name of icefront depth file (m)
C                          2D file containing depth of the ice front
C                          at each model grid cell
C     STIClengthFile    :: name of icefront length file (m/m^2)
C                          2D file containing the ratio of the horizontal
C                          length of the ice front in each model grid cell
C                          divided by the grid cell area
C-----------------------------------------------------------------------
CEOP

      COMMON /STIC_PARMS_R/
     &     icfThetaHorizDiffusionLength,
     &     icfThetaInterior
      _RL icfThetaHorizDiffusionLength
      _RL icfThetaInterior

      CHARACTER*(MAX_LEN_FNAM) STIClengthFile
      CHARACTER*(MAX_LEN_FNAM) STICdepthFile
      COMMON /STIC_PARM_C/
     &     STIClengthFile,
     &     STICdepthFile

C     ow - 06/29/2018
C     maskSHI of pkg/shelfice is not consistent with the spirit of gencost.
C       Use the following masks below instead.
C     mask2dSHIICF: 2d shelf-ice & ice-front mask:
C       1 for having shelf-ice and/or ice-front at one or more vertical levels
C       and 0 otherwise.
C     mask3dSHIICF: 3d shelf-ice & ice-front mask.
C     mask2dSHI: 2d shelf-ice mask
C     mask3dSHI: 3d shelf-ice mask
C     mask2dICF: 2d ice-front mask: 1 for having ice-front at one or more
C                vertical levels.
C     mask3dICF: 3d ice-front mask
      COMMON /STIC_MASKS/ mask2dSHIICF, mask3dSHIICF,
     &        mask2dSHI, mask3dSHI, mask2dICF, mask3dICF
      _RS mask2dSHIICF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask3dSHIICF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS mask2dSHI   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask3dSHI   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS mask2dICF   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask3dICF   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     INTEGER arrays
C     CURI_ARR       :: i-index for neighboring ice-front points
C     CURJ_ARR       :: j-index for neighboring ice-front points
C     sticfWidth_arr :: ice-front width in meters
C     kIcf           :: index of the bottommost ice front cell (2D)
      COMMON /STIC_ICEFRONT_I/ kIcf, CURI_ARR, CURJ_ARR
      INTEGER kIcf    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER CURI_ARR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)
      INTEGER CURJ_ARR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)

      COMMON /STIC_FIELDS_RL/
     &     stic_gT, stic_gS,
     &     sticfWidth_arr
      _RL stic_gT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL stic_gS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sticfWidth_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,4)

#ifdef ALLOW_SHITRANSCOEFF_3D
       COMMON /SHELFICE_GAMMA3D_RL/
     &     shiTransCoeffT3D, shiTransCoeffS3D
       _RL shiTransCoeffT3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
       _RL shiTransCoeffS3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

      COMMON /STIC_FIELDS_RS/
     &     R_stic, sticfLength,
     &     icfHeatFlux, icfFreshWaterFlux,
     &     sticfHeatFlux, sticfFreshWaterFlux
      _RS R_stic             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS sticfLength        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icfHeatFlux        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL icfFreshWaterFlux  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sticfHeatFlux      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sticfFreshWaterFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
