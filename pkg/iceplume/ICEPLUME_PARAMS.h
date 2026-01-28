CBOP
C     !ROUTINE: ICEPLUME_PARAMS.h
C     !INTERFACE:
C     include "ICEPLUME_PARAMS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | ICEPLUME_PARAMS.h
C     | o Hold parameters used in pkg/iceplume
C     *==========================================================*
C     \ev
CEOP
#ifdef ALLOW_ICEPLUME

C Parameters to be read in from namelists in data.iceplume
C     usePlumeDiagnostics :: switch diagnostics, need pkg/diagnostics
C     conserveMass        :: prevent net addition of entrained mass if .TRUE.
C     use[Sheet,Done,Trunc,Buoy]Plume :: switch [sheet,cone,truncated,buoyancy-driven]
C     applyIcePlumeBGTend[T,S] :: switch to apply plume tendency g[T,S] to ocean T,S

      COMMON /ICEPLUME_PARM01_L/
     &      usePlumeDiagnostics, conserveMass,
     &      useSheetPlume, useConePlume, 
     &      useTruncPlume, useBuoyPlume,
     &      applyIcePlumeBGTendT, applyIcePlumeBGTendS
      LOGICAL usePlumeDiagnostics, conserveMass,
     &        useSheetPlume, useConePlume, 
     &        useTruncPlume, useBuoyPlume,
     &        applyIcePlumeBGTendT, applyIcePlumeBGTendS

C     runoffQsgfile   :: file containing subglacial runoff field in unit kg/s
C     plumeMaskFile   :: xy mask of iceplume types, integer ranging [-7,7]
C     plumeLengthFile :: file of dimension length of plume in unit m

      COMMON /ICEPLUME_PARM01_C/
     &      runoffQsgfile, plumeMaskFile, plumeLengthFile
      CHARACTER*(MAX_LEN_FNAM)
     &      runoffQsgfile, plumeMaskFile, plumeLengthFile

C     T_sg_0        :: init pot temp of sg discharge [degC]
C     S_sg_0        :: init sal of sg discharge [g/kg]
C     Angle_sg_0    :: initial angle wrt horizon, deg -- make nearly horz
C     wVel_sg_0     :: init vert vel at pt src, use to convert Q_sg to r_sg,w_sg
C     RTOL          :: Relative tolerance parameter for ODE solver
C     ATOL          :: Absolute tolerance parameter for ODE solver
C     E_0           :: Entrainment parameter in plume model (unitless)
C     iceTemp       :: temperature of ice in contact with ocean (degC)
C     c_i           :: Heat capacity of ice (J/kg/degC)
C     lambda1       :: freezing point slope (degC/[g/kg])
C     lambda2       :: freezing point offset (degC)
C     lambda3       :: freezing point depth (degC/m)
C     GamT          :: Thermal turbulent transfer coeff (unitless)
C     GamS          :: Haline turbulent transfer coeff (unitless)
C     Cd            :: iceplume drag coeff (unitless)
C     slopeTmod     :: slope of line fit of ptemp b/t submarine melt plume and fjord 
C     interceptTmod :: intercept of line fit of above
C     maxDepth      :: max depth of domain (m)
C     backgroundVelThresh     :: Unresolved velocity at ice-ocean interface (m/s)
C     ICEPLUMElatentHeat      :: latent heat of fusion (def: 334000 J/kg)
C     ICEPLUMEheatCapacity_Cp :: Heat Capacity of shelf ice (def: 2000. J/kg)
C Parameters for when def ICEPLUME_ALLOW_SCHULZ22
C     Gam[T,S]const :: constant value (independent of background vel) of Gam[T,S]
C     facGamSGamT   :: factor relating Gam[T,S]const
C     Lp            :: along-front length of the truncated line plume

      COMMON /ICEPLUME_PARM01_RL/
     &    T_sg_0, S_sg_0, Angle_sg_0, wVel_sg_0, RTOL, ATOL, E_0,
     &    iceTemp, c_i, lambda1, lambda2, lambda3, GamT, GamS, Cd,
     &    slopeTmod, interceptTmod, maxDepth, backgroundVelThresh,
     &    ICEPLUMElatentHeat, ICEPLUMEheatCapacity_Cp
#ifdef ICEPLUME_ALLOW_SCHULZ22
     &   ,GamTconst, GamSconst, facGamSGamT, Lp
#endif
      _RL T_sg_0, S_sg_0, Angle_sg_0, wVel_sg_0, RTOL, ATOL, E_0,
     &    iceTemp, c_i, lambda1, lambda2, lambda3, GamS, GamT, Cd,
     &    slopeTmod, interceptTmod, maxDepth, backgroundVelThresh,
     &    ICEPLUMElatentHeat, ICEPLUMEheatCapacity_Cp
#ifdef ICEPLUME_ALLOW_SCHULZ22
      _RL GamTconst, GamSconst, facGamSGamT, Lp
#endif

C General variables shared between subroutines:
C     ICEPLUMEisOn   :: set to True if useICEPLUME = .TRUE.
C     useDetachPlume :: can be .true. only if def iceplume_allow_detached_plume
C     iceDepthK      :: index of depth of grounding line [unitless]
C     iceDepth       :: depth of grounding line [m]
C     dLnormal       :: model grid d[x,y]G normal to glacier wall
C     dLtangential   :: model grid d[x,y]G parallel to (i.e., along) glacier wall
C     [Q,r,w]_sg     :: time-varying sg discharge [volflux,radius,vert vel] at pt src

      COMMON /ICEPLUME_PARM_L/ ICEPLUMEisOn, useDetachPlume
      LOGICAL ICEPLUMEisOn, useDetachPlume

      COMMON /ICEPLUME_PARM_I/ iceDepthK
      INTEGER iceDepthK

      COMMON /ICEPLUME_PARM_RL/
     &    Q_sg, r_sg, w_sg, L_sg, dLnormal, dLtangential, iceDepth
      _RL Q_sg, r_sg, w_sg, L_sg, dLnormal, dLtangential, iceDepth

# endif /* ALLOW_ICEPLUME */
