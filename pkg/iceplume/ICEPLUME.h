
#include "ICEPLUME_OPTIONS.h"

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
     &     runoffVel, runoffVel0, runoffVel1,
     &     runoffRad, runoffRad0, runoffRad1,
     &     plumeMask, 
     &     HeatFlux3D, FwFlux3D,
     &     rProfPlume3D, wProfPlume3D,
     &     tProfPlume3D, sProfPlume3D,
     &     mProfPlume3D, mProfAv3D,
     &     rProfPlume3DLocal, wProfPlume3DLocal,
     &     tProfPlume3DLocal, sProfPlume3DLocal,
     &     mProfPlume3DLocal, mProfAv3DLocal,
     &     temp_addMass3D, salt_addMass3D,
     &     volFluxDiff3D,
     &     Qin
      _RL runoffVel  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffVel0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffVel1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffRad  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffRad0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffRad1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL plumeMask  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RS HeatFlux3D  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS FwFlux3D    (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS rProfPlume3D    (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS wProfPlume3D    (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS tProfPlume3D    (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS sProfPlume3D    (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS mProfPlume3D    (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS mProfAv3D       (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL temp_addMass3D  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL salt_addMass3D  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL volFluxDiff3D   (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS rProfPlume3DLocal    (sNx,sNy,Nr)
      _RS wProfPlume3DLocal    (sNx,sNy,Nr)
      _RS tProfPlume3DLocal    (sNx,sNy,Nr)
      _RS sProfPlume3DLocal    (sNx,sNy,Nr)
      _RS mProfPlume3DLocal    (sNx,sNy,Nr)
      _RS mProfAv3DLocal       (sNx,sNy,Nr)
      _RS Qin (100)

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
     & thetaProfPlume, distanceProfPlume,
     & thetaProfPlume3D, distanceProfPlume3D,
     & thetaProfPlume3DLocal, distanceProfPlume3DLocal
      _RL thetaProfPlume (Nr+1)
      _RL distanceProfPlume (Nr+1)
      _RL thetaProfPlume3D
     &  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL distanceProfPlume3D
     &  (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)      
      _RL thetaProfPlume3DLocal (sNx,sNy,Nr)
      _RL distanceProfPlume3DLocal (sNx,sNy,Nr)
#endif      

      COMMON /ICEPLUME_PARM_R/
     &     E_0,
     &     Q_sg, T_sg, S_sg, r_sg, w_sg,
     &     theta_sg,
     &     delta_x, delta_y,
     &     RTOL, ATOL,
     &     iceTemp,
     &     outputThickness,
     &     kap, rho_ref, g, c_w, c_i, L,
     &     gam, lambda1, lambda2, lambda3,
     &     GamT, GamS, Cd,
     &     sOutPlume, tOutPlume,
     &     iceDepth,
     &     backgroundVel,
     &     ptracerfluxSum,
     &     maxDepth
      _RS E_0
      _RS Q_sg
      _RL T_sg
      _RL S_sg
      _RL w_sg
      _RL r_sg
      _RL theta_sg
      _RL delta_x
      _RL delta_y
      _RL RTOL
      _RL ATOL
      _RL iceTemp
      _RL outputThickness
      _RL kap
      _RL rho_ref
      _RL g
      _RL c_w
      _RL c_i
      _RL L
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

      COMMON /ICEPLUME_FILES/
     &	    runoffVelfile,
     &	    runoffRadfile,
     &      plumeMaskFile
C Again, doesn't like MAX_LEN_FNAM
      CHARACTER*(512)
     &	    runoffVelfile,
     &	    runoffRadfile,
     &      plumeMaskFile

C-----------------------------------------
C Parameters relating to the icefront package

      COMMON /ICEPLUME_ICEFRONT_PARMS_R/
     &     ICEFRONTlatentHeat, 
     &     ICEFRONTheatCapacity_Cp
      _RL ICEFRONTlatentHeat
      _RL ICEFRONTheatCapacity_Cp

      COMMON /ICEPLUME_ICEFRONT_FIELDS_RL/
     &     icefront_TendT,
     &     icefront_TendS
      _RL icefront_TendT (1:sNx,1:sNy,Nr,nSx,nSy)
      _RL icefront_TendS (1:sNx,1:sNy,Nr,nSx,nSy)

      COMMON /ICEPLUME_ICEFRONT_FIELDS_RS/
     &     R_icefront,
     &     icefrontlength
      _RS R_icefront     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS icefrontlength (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C      LOGICAL ICEFRONTisOn
      LOGICAL applyIcefrontTendT
      LOGICAL applyIcefrontTendS
      COMMON /ICEPLUME_ICEFRONT_PARMS_L/
C     &     ICEFRONTisOn,
     &     applyIcefrontTendT,
     &     applyIcefrontTendS

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
