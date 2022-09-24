
#include "ICEPLUME_OPTIONS.h"

#ifdef ALLOW_EXF
#include "EXF_OPTIONS.h"
#endif

#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_ICEPLUME

C Header file pkg/ICEPLUME

      COMMON /ICEPLUME_INTEGERS/
     &       iceDepthK,
     &       runoffLocation, plumeInputLayer

      INTEGER iceDepthK
      INTEGER runoffLocation  (100,2)
      INTEGER plumeInputLayer (100)

      COMMON /ICEPLUME_LOGICALS/
     &      usePlumeDiagnostics,
     &      dispersePlumeOutput,
     &      useSheetPlume,
     &      useConePlume,
     &      useDetachPlume,
     &      conserveMass,
     &      useInputPtracers
      LOGICAL usePlumeDiagnostics
      LOGICAL dispersePlumeOutput
      LOGICAL useSheetPlume
      LOGICAL useConePlume
      LOGICAL useDetachPlume
      LOGICAL conserveMass
      LOGICAL useInputPtracers

      COMMON /ICEPLUME_FIELDS/
     &     runoffQsg,runoffQsg0,runoffQsg1,
     &     plumeMask, 
     &     temp_addMass3Dplume, salt_addMass3Dplume,
     &     addMass3Dplume,
     &     Qin
catn     &     runoffVel, runoffVel0, runoffVel1,
catn     &     runoffRad, runoffRad0, runoffRad1,
      _RL runoffQsg  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffQsg0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffQsg1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffVel  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffVel0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffVel1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffRad  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffRad0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffRad1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL plumeMask  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL temp_addMass3Dplume  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL salt_addMass3Dplume  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL addMass3Dplume  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS Qin (100)

      COMMON /ICEPLUME_FILES/
     &	    runoffQsgfile,
     &      plumeMaskFile
#ifdef ALLOW_EXF
     &     ,runoffQsgmask
#endif /* ALLOW_EXF */
      CHARACTER*(MAX_LEN_FNAM)
     &	    runoffQsgfile,
     &      plumeMaskFile
#ifdef ALLOW_EXF
      CHARACTER*1 runoffQsgmask
c runoffQsgmask will be 'c'
#endif /* ALLOW_EXF */

#ifdef ALLOW_EXF
C     the following variables are used in conjunction
C     with pkg/exf to specify sub-glacial runoff
      INTEGER runoffQsgstartdate1
      INTEGER runoffQsgstartdate2
      _RL     runoffQsgStartTime
      _RL     runoffQsgperiod
      _RL     runoffQsgRepCycle
      _RL     runoffQsgconst
      _RL     runoffQsg_inscal
      _RL     runoffQsg_remov_intercept
      _RL     runoffQsg_remov_slope
#ifdef USE_EXF_INTERPOLATION
      _RL     runoffQsg_lon0
      _RL     runoffQsg_lon_inc
      INTEGER runoffQsg_nlon
      _RL     runoffQsg_lat0
c MAX_LAT_INC=1279 in exf_interp_size
      _RL     runoffQsg_lat_inc(1279)
      INTEGER runoffQsg_nlat
      INTEGER runoffQsg_interpMethod
#endif /* USE_EXF_INTERPOLATION */

      COMMON /ICEPLUME_EXF_PAR_I/
     &       runoffQsgstartdate1, runoffQsgstartdate2
#ifdef USE_EXF_INTERPOLATION
     &      ,runoffQsg_nlon,runoffQsg_nlat
     &      ,runoffQsg_interpMethod
#endif /* USE_EXF_INTERPOLATION */
      COMMON /ICEPLUME_EXF_PAR_R/
     &       runoffQsgStartTime,runoffQsgperiod,
     &       runoffQsgRepCycle,runoffQsgconst,runoffQsg_inscal,
     &       runoffQsg_remov_intercept, runoffQsg_remov_slope
#ifdef USE_EXF_INTERPOLATION
     &      ,runoffQsg_lon0,runoffQsg_lon_inc
     &      ,runoffQsg_lat0,runoffQsg_lat_inc
#endif /* USE_EXF_INTERPOLATION */
#endif /* ALLOW_EXF */


      COMMON /ICEPLUME_FIELDS_PROFILES/
     &     sProf, tProf, ptProf, prProf, uProf, vProf,
     &     rProfPlume, wProfPlume, tProfPlume, sProfPlume,
     &     uProfPlume, mProfPlume, mProfAv, mProf, zProf, zProfAbs,
     &     volFLux, volFluxDiff,
     &     FwFlux, HeatFlux,
     &     delta_z, aProfPlume, mIntProfPlume
      _RL sProf  (Nr)
      _RL tProf  (Nr)
      _RL ptProf (Nr)
      _RL prProf (Nr)
      _RL uProf  (Nr)
      _RL vProf  (Nr)
      _RL rProfPlume (Nr+1)
      _RL wProfPlume (Nr+1)
      _RL tProfPlume (Nr+1)
      _RL sProfPlume (Nr+1)
      _RL aProfPlume (Nr+1)   ! integrated area of plume from bottom
      _RL mIntProfPlume (Nr+1)   ! integrated melt of plume from bottom
      _RL uProfPlume (Nr)     
      _RL mProfPlume (Nr)
      _RL mProfAv    (Nr)
      _RL mProf      (Nr)
      _RL zProf      (Nr+1)
      _RL zProfAbs   (Nr+1)
      _RL volFlux     (Nr+1)
      _RL volFluxDiff (Nr)
      _RL HeatFlux(Nr)
      _RL FwFlux(Nr)
      _RL delta_z(Nr)
      
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME      
      COMMON /ICEPLUME_FIELDS_DETACHED/
     & thetaProfPlume, distanceProfPlume
      _RL thetaProfPlume (Nr+1)
      _RL distanceProfPlume (Nr+1)
#endif      

C     dLnormal     :: the model grid d[x,y]G that is normal to the glacier wall
C     dLtangential :: the model grid d[x,y]G that is parallel to (i.e., along) the glacier wall
C     Angle_sg_0   :: initial angle with respect to horizon, deg -- make nearly horizontal
C     T_sg_0       :: initial temperature of subglacial discharge, default to 1.d-3 degC
C     S_sg_0       :: initial salinity of subglacial discharge, default to 1.d-3 g/kg
C     Q_sg         :: time-dependent subglacial discharge volume flux rate at point source, default to 1.d-3 m3/s
C     r_sg         :: time-dependent subglacial discharge radius at point source, default to 1.d-2 m
C     w_sg         :: time-dependent subglacial discharge vertical vel at point source, default to 1.d0 m/s
C     wVel_sg_0    :: initial vertical vel at point source, default to 1. m/s, use to convert Q_sg to r_sg,w_sg

      COMMON /ICEPLUME_PARM_R/
     &     E_0,
     &     Q_sg, T_sg_0, S_sg_0, r_sg, w_sg,
     &     Angle_sg_0, wVel_sg_0,
     &     dLnormal, dLtangential,
     &     RTOL, ATOL,
     &     iceTemp,
     &     outputThickness,
     &     kap, c_i,
     &     gam, lambda1, lambda2, lambda3,
     &     GamT, GamS, Cd,
     &     sOutPlume, tOutPlume,
     &     iceDepth,
     &     backgroundVel,
     &     ptracerfluxSum,
     &     maxDepth
      _RS E_0
      _RL Angle_sg_0
      _RL wVel_sg_0
      _RL T_sg_0
      _RL S_sg_0
      _RL Q_sg
      _RL w_sg
      _RL r_sg
      _RL dLnormal
      _RL dLtangential
      _RL RTOL
      _RL ATOL
      _RL iceTemp
      _RL outputThickness
      _RL kap
      _RL c_i
      _RL gam
      _RL lambda1
      _RL lambda2
      _RL lambda3
      _RL sOutPlume
      _RL tOutPlume
      _RL gamS
      _RL gamT
      _RL cd
      _RL iceDepth
      _RL backgroundVel
      _RL ptracerFluxSum
      _RL maxDepth

C-----------------------------------------
C Parameters relating to the icefront package
C     ICEPLUMElatentHeat       :: latent heat of fusion (def: 334000 J/kg)
C     ICEPLUMEheatCapacity_Cp  :: Heat Capacity of shelfice (def: 2000. J/kg)

      COMMON /ICEPLUME_PARMS_R/
     &     ICEPLUMElatentHeat, 
     &     ICEPLUMEheatCapacity_Cp
      _RL ICEPLUMElatentHeat
      _RL ICEPLUMEheatCapacity_Cp

      COMMON /ICEPLUME_FIELDS_RL/
     &     iceplumeBG_TendT,
     &     iceplumeBG_TendS
      _RL iceplumeBG_TendT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL iceplumeBG_TendS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

catn change icefront to iceplumeBG or iceplume to avoid potential conflict
      LOGICAL ICEPLUMEisOn
      LOGICAL applyIcePlumeBGTendT
      LOGICAL applyIcePlumeBGTendS
      COMMON /ICEPLUME_PARMS_L/
     &     ICEPLUMEisOn,
     &     applyIcePlumeBGTendT,
     &     applyIcePlumeBGTendS

C---------------------------------------------

#ifdef ALLOW_PTRACERS
C Parameters relating to PTRACERS

      COMMON /ICEPLUME_PTRACERS_RL/
     &     ptr_addMass3D,
     &     ptracerMask
      _RL ptr_addMass3d
     &     (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL ptracerMask
     &     (1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num,nSx,nSy)
   
      COMMON /ICEPLUME_PTRACERS_FILES/
     &     ptracerMaskFile
      CHARACTER*(512) 
     &     ptracerMaskFile

#endif /* ALLOW_PTRACERS */

# endif /* ALLOW_ICEPLUME */ 
