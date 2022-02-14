#ifdef ALLOW_ICEFRONT

CBOP
C !ROUTINE: ICEFRONT.h

C !DESCRIPTION: \bv
C     *==========================================================*
C     | ICEFRONT.h                                               |
C     | o Basic header thermodnynamic shelf ice package.         |
C     |   Contains all ICEFRONT field declarations.              |
C     *==========================================================*
C \ev

C-----------------------------------------------------------------------
C--   Constants that can be set in data.icefront, namelist /ICEFRONT_PARM01/
C     ICEFRONTdepthFile        - name of icefront depth file (m)
C                                2D file containing depth of the ice front
C                                at each model grid cell
C     ICEFRONTlengthFile       - name of icefront length file (m/m^2)
C                                2D file containing the ratio of the horizontal
C                                length of the ice front in each model grid cell
C                                divided by the grid cell area
C     ICEFRONTheatTransCoeff   - heat transfer coefficient that determines
C                                 heat flux into icefront (m/s)
C     ICEFRONTsaltTransCoeff   - salinity transfer coefficient that determines
C                                salt flux into icefront (m/s)
C     ICEFRONTthetaSurface     - surface temperature on the top of icefront (oC)
C                                interior temperture of the ice changes linearly
C                                from ICEFRONTthetaSurface at surface to 0 oC at
C                                the bottom
C     ICEFRONTlatentHeat       - latent heat of fusion (J/kg)
C     applyIcefrontTendT/S     -
C
C     K_icefront          - # of icefront model levels at every horizontal location (2D)
C     R_icefront          - icefront depth [m] (2D)
C     icefrontlength      - icefront horizontal length divided
C                           by grid cell area [m/m^2] (2D)
C     icefront_TendT      - temperature tendency (Kelvin/s)
C     icefront_TendS      - salinity tendency (g/kg/s)
C
C--   Constants that can be set in data.icefront, namelist /ICEFRONT_EXF_PARM02/
C     SGRunOffFile           - name of subglacial runoff file (kg/s)
C                              2D file containing mass of subglacial runoff
C                              added at bottom of model ocean
C     SGRunOff               - subglacial runoff (kg/s)
C     Arrays *0 and *1 below are used for temporal interpolation.
C-----------------------------------------------------------------------
CEOP

      COMMON /ICEFRONT_PARMS_I/  K_icefront
      INTEGER K_icefront (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /ICEFRONT_PARMS_R/
     &     rhoIcefront, ICEFRONTkappa,
     &     ICEFRONTlatentHeat, recip_ICEFRONTlatentHeat,
     &     ICEFRONTheatCapacity_Cp,
     &     ICEFRONTthetaSurface
      _RL ICEFRONTlatentHeat
      _RL ICEFRONTheatCapacity_Cp
      _RL rhoIcefront
      _RL ICEFRONTkappa
      _RL recip_ICEFRONTlatentHeat
      _RL ICEFRONTthetaSurface

      COMMON /ICEFRONT_FIELDS_RL/
     &     icefront_TendT,
     &     icefront_TendS
      _RL icefront_TendT (1:sNx,1:sNy,Nr,nSx,nSy)
      _RL icefront_TendS (1:sNx,1:sNy,Nr,nSx,nSy)

      COMMON /ICEFRONT_FIELDS_RS/
     &     R_icefront,
     &     icefrontlength
      _RS R_icefront     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontlength (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      LOGICAL ICEFRONTisOn
      LOGICAL applyIcefrontTendT
      LOGICAL applyIcefrontTendS
      COMMON /ICEFRONT_PARMS_L/
     &     ICEFRONTisOn,
     &     applyIcefrontTendT,
     &     applyIcefrontTendS

      CHARACTER*(MAX_LEN_FNAM) ICEFRONTlengthFile
      CHARACTER*(MAX_LEN_FNAM) ICEFRONTdepthFile
      COMMON /ICEFRONT_PARM_C/
     &     ICEFRONTlengthFile,
     &     ICEFRONTdepthFile

#ifdef ALLOW_EXF
C     the following variables are used in conjunction
C     with pkg/exf to specify sub-glacial runoff
      INTEGER SGRunOffstartdate1
      INTEGER SGRunOffstartdate2
      _RL     SGRunOffStartTime
      _RL     SGRunOffperiod
      _RL     SGRunOffconst
      _RL     SGRunOff_inscal
      _RL     SGRunOff_remov_intercept
      _RL     SGRunOff_remov_slope
      CHARACTER*(MAX_LEN_FNAM) SGRunOffFile
      CHARACTER*1 SGRunOffmask

      COMMON /ICEFRONT_EXF_PAR_I/
     &       SGRunOffstartdate1, SGRunOffstartdate2
      COMMON /ICEFRONT_EXF_PAR_R/
     &       SGRunOffStartTime,  SGRunOffperiod,
     &       SGRunOffconst,      SGRunOff_inscal,
     &       SGRunOff_remov_intercept, SGRunOff_remov_slope
      COMMON /ICEFRONT_EXF_PAR_C/
     &       SGRunOffFile,  SGRunOffmask
#endif /* ALLOW_EXF */

#endif /* ALLOW_ICEFRONT */
