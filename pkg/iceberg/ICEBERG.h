C $Header: /u/gcmpack/MITgcm/pkg/iceberg/ICEBERG.h,v 1.20 2015/04/22 21:33:58 bdavison Exp $
C $Name: checkpoint65m $

#ifdef ALLOW_ICEBERG

CBOP
C !ROUTINE: ICEBERG.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | ICEBERG.h
C     | o Basic header thermodnynamic iceberg ice package.
C     |   Contains all ICEBERG field declarations.
C     |   Original Package developed by Ben Davision et al. 2020
C     |   https://doi.org/10.1038/s41467-020-19805-7
C     |   Extended to include drag Paul Summers et al. 2025
C     |   https://doi.org/10.5194/egusphere-2025-1555
C     |   Last major upate October 2025
C     |   paul.summers@tufts.edu for any questions
C     *==========================================================*

C==============================================================================
C     FILES AND FILE VARIABLES
C     ICEBERGmaskFile              :: File containing iceberg mask and flag for orientation
C     ICEBERGmaskNumsFile          :: File containing arbitrary numbers for each cell containing icebergs. Links to text files containing iceberg dimensions.
C     ICEBERGnumPerCellFile        :: File containing number of icebergs per cell.
C     ICEBERGdriftFile             :: File containing mask of where effect of iceberg drift on melting will be calculated (logical)
C     ICEBERGbarrierFile           :: File containing mask for where icebergs act as physical barrier to water flow
C     ICEBERGopenFracFile          :: File containing proportion of cell volume that is open (i.e. not taken up by icebergs)
C     ICEBERGareaFile              :: File containing total submerged iceberg surface area in each cell.
C     ICEBERGlengthFile            :: File containing lengths of all icebergs.
C     ICEBERGwidthsFile            :: File containing widths of all icebergs.
C     ICEBERGdepthsFile            :: File containing depths of all icebergs.
C     brg_Mask                     :: XY Mask for iceberg cells and iceberg orientation (1 = long axis oriented east-west)
C     brg_MaskNums                 :: XY field containing numbers corresponding to each column with icebergs
C     brg_NumBergs                 :: XY field containing number of icebergs per cell
C     brg_DriftMask                :: XY mask of where to calculate iceberg drift velocity
C     brg_MeltMask                 :: XY mask of where to calculate iceberg melt
C     brg_BarrierMask              :: XY mask of where to make icebergs physical barrier to water flow
C     brg_OpenFraction             :: XYZ field specifying proportion of cell that is open
C     brg_Area3D                   :: XYZ field of iceberg submerged surface area
C     brg_Length                   :: XY[maxBerg] containing lengths of all icebergs.
C     brg_Widths                   :: XY[maxBerg] containing widths of all icebergs.
C     brg_Depths                   :: XY[maxBerg] containing depths of all icebergs.
C
C===============================================================================
C-   CONSTANTS SET IN data.iceberg
C     brg_Rho                      :: Iceberg density (def: 917 kg m^-3 s^-1)
C     brg_iceTemp                  :: Surface temperature on the top of icefront (def: 0 degC). Interior temperature of the changes linearly from ICEFRONTthetaSurface at surface to 0 oC at the bottom
C     brg_L                        :: Latent heat of fusion (def: 334.0*10^3 J kg^-1)
C     brg_c_i                      :: heat capacity of icebergs (def: 2000 J K^-1 kg^-1)
C     brg_Cd                       :: quadratic drag coefficient (def: 0.0025)
C     brg_uMin                     :: Constant minimum background velocity applied to iceberg faces (m s^-1)
C     brg_lambda1                  :: Freezing point slope (def: -0.0573 degC psu^-1)
C     brg_lambda2                  :: Freezing point offset (def: 0.0832 degC)
C     brg_lambda3                  :: Freezing point depth (def: -7.61*10^-4)
C     brg_GamT                     :: Thermal turbulent transfer coeffcient (def: 0.022)
C     brg_GamS                     :: Salt turbulent transfer coefficient (def: 0.00062)
C     brg_c_w                      :: Heat capacity of water (def: 3974 J kg^-1 degC^-1)
C     brg_SelectDrag               :: select how drag is computed from velocity (def: 3)
C                                     (1:n = 2, 2:n = 2, 3: n = 1 + .75*hFacC, 4: 1 + .75*(hFacC)**3
C     brg_SelectFill               :: select how frontal area scales with hFacC (def: 3)
C                                     (1:linear, 2:quad, 3:quartic)
C     brg_DragForm                 :: form drag across iceberg (default = 0.0025)
C     brg_useInputPtracers         :: select if using ptracers input

C=============================================================================
C     FIELDS
C     brg_kBotC                    :: index of the bottom "berg cell" (2D), if 0 no berg drag
C     brg_HeatFlux3D               :: upward heat flux (W/m^2)
C     brg_FwFlux3D                 :: upward fresh water flux (virt. salt flux) (kg/m^2/s)
C     brg_MeltRate3D               :: Melt rate (m/d)
C     brg_TendT3D                  :: Temperature tendency (Kelvin/s)
C     brg_TendS3D                  :: Salinity tendency (psu/s)
#ifdef ALLOW_DIAGNOSTICS
C     brg_DragU                    :: iceberg stress (for diagnostics), Zonal comp. 
C                                    Units are N/m^2 ;   > 0 increase top uVel
C     brg_DragV                    :: iceberg stress (for diagnostics), Merid. comp. 
C                                    Units are N/m^2 ;   > 0 increase top vVel
#endif /* ALLOW_DIAGNOSTICS */
C==============================================================================
C \ev
CEOP

      COMMON /ICEBERG_PARMS_I/
     &     brg_SelectDrag,
     &     brg_SelectFill,
     &     brg_useInputPtracers
      INTEGER brg_SelectDrag
      INTEGER brg_SelectFill
      INTEGER brg_useInputPtracers

      COMMON /ICEBERG_PARMS_R/
     &     brg_Rho,
     &     brg_iceTemp,
     &     brg_uMin,
     &     brg_lambda1,
     &     brg_lambda2,
     &     brg_lambda3,
     &     brg_GamT,
     &     brg_GamS,
     &     brg_c_w,
     &     brg_c_i,
     &     brg_L,
     &     brg_Cd,
     &     brg_DragForm
      _RL brg_Rho
      _RL brg_iceTemp
      _RL brg_uMin
      _RL brg_lambda1
      _RL brg_lambda2
      _RL brg_lambda3
      _RL brg_GamT
      _RL brg_GamS
      _RL brg_c_w
      _RL brg_c_i
      _RL brg_L
      _RL brg_Cd
      _RL brg_DragForm

      COMMON /ICEBERG_FIELDS_I/ brg_kBotC
      INTEGER brg_kBotC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /ICEBERG_FIELDS_RL/
     &     brg_HeatFlux3D,
     &     brg_FwFlux3D,
     &     brg_MeltRate3D,
     &     brg_TendT3D,
     &     brg_TendS3D,
     &     brg_OpenFraction, 
     &     brg_DriftMask, 
     &     brg_MeltMask,
     &     brg_Mask,
     &     brg_BarrierMask, 
     &     brg_MaskNums, 
     &     brg_NumBergs,
     &     brg_Area3D,
     &     brg_Length,
     &     brg_Widths,
     &     brg_Depths
      _RL brg_HeatFlux3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)  
      _RL brg_FwFlux3D (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)  
      _RL brg_MeltRate3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)  
      _RL brg_TendT3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL brg_TendS3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL brg_OpenFraction(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL brg_DriftMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL brg_MeltMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL brg_Mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL brg_BarrierMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL brg_MaskNums(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL brg_NumBergs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL brg_Area3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL brg_Length(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &       brg_MaxBergCt,nSx,nSy) 
      _RL brg_Widths(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &       brg_MaxBergCt,nSx,nSy)
      _RL brg_Depths(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &       brg_MaxBergCt,nSx,nSy)

      COMMON /ICEBERG_PARM_C/
     &     ICEBERGmaskFile,
     &     ICEBERGmaskNumsFile,
     &     ICEBERGnumPerCellFile,
     &     ICEBERGdriftFile,
     &     ICEBERGmeltFile,
     &     ICEBERGopenFracFile,
     &     ICEBERGbarrierFile,
     &     ICEBERGareaFile,
     &     ICEBERGlengthFile,
     &     ICEBERGwidthsFile,
     &     ICEBERGdepthsFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGmaskFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGmaskNumsFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGnumPerCellFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGmeltFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGdriftFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGopenFracFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGbarrierFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGareaFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGlengthFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGwidthsFile
      CHARACTER*(MAX_LEN_FNAM) ICEBERGdepthsFile

#ifdef ALLOW_DIAGNOSTICS
      COMMON /ICEBERG_DIAG_DRAG/ brg_DragU, brg_DragV
      _RS brg_DragU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS brg_DragV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_PTRACERS
C Parameters relating to PTRACERS

      COMMON /ICEBERG_PTRACERS_RL/
     &     brg_ptr_addMass3D,
     &     brg_ptracerMask
      _RL brg_ptr_addMass3d
     &     (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL brg_ptracerMask
     &     (1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num,nSx,nSy)
   
      COMMON /ICEBERG_PTRACERS_FILES/
     &     brg_ptracerMaskFile
      CHARACTER*(512) 
     &     brg_ptracerMaskFile

#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_ICEBERG */
