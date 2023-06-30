CBOP
C     !ROUTINE: ICEPLUME_FIELDS.h
C     !INTERFACE:
C     include "ICEPLUME_FIELDS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | ICEPLUME_FIELDS.h
C     | o Hold multi-dimensional fields used in pkg/iceplume
C     | Terminology: 
C     | subglacial runoff discharge: from point source, abbr SG or sg
C     | submarine melt: background melt of glacier due to rising SG
C     |                 plume, abbr: BG or bg
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_ICEPLUME

C Obsolete, to be deleted
CC     num_runoff :: number of grid pts for pt-src subglacial discharge 
C      INTEGER num_runoff_sg
C      PARAMETER( num_runoff_sg = 100 )
CC runoffLocation  ::
CC plumeInputLayer ::
CC Qin             ::
C      COMMON /ICEPLUME_FIELDS_I/
C     &       runoffLocation, plumeInputLayer
C      INTEGER runoffLocation  (num_runoff_sg,2)
C      INTEGER plumeInputLayer (num_runoff_sg)

C      COMMON /ICEPLUME_FIELDS_RS/ Qin
C      _RS Qin (num_runoff_sg)

C      runoffQsg[,0,1] :: SG discharge flux [kg/s]
C      plumeMask       :: mask specifying type of plume at glacier/ocean interface
C      plumeLength     :: length of plume along glacier/ocean interface [m]
C      addMass3Dplume  :: src+entrained mass in SG plume to be added to ocean [kg/s]
C      [temp,salt]_addMass3Dplume :: [ptemp,salinity] of addMass3Dplume [degC,g/kg]
C      HeatFlux3Dpl    :: Heat flux associated with addMass3Dplume [J/s]
C      SaltFlux3Dpl    :: Salt flux associated with addMass3Dplume [g/s]
C      addMass3Dbg     :: src+entrained mass in submarine melt to be added to ocean [kg/s]
C      HeatFlux3Dbg    :: Heat flux associated with background submarine melt [J/s]
C      SaltFlux3Dbg    :: Salt flux associated with background submarine melt [g/s]
C      iceplumeBG_Tend[T,S]:: contrib of tend g[T,S] of BG melt

      COMMON /ICEPLUME_FIELDS_RL/
     &     runoffQsg,runoffQsg0,runoffQsg1,
     &     plumeMask, plumeLength,
     &     addMass3Dplume,
     &     iceplumeBG_TendT,iceplumeBG_TendS,
     &     HeatFlux3Dpl, SaltFlux3Dpl,
     &     addMass3Dbg, HeatFlux3Dbg, SaltFlux3Dbg
c    &     temp_addMass3Dplume, salt_addMass3Dplume,

      _RL runoffQsg  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffQsg0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL runoffQsg1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL plumeMask(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL plumeLength(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
c      _RL temp_addMass3Dplume(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
c      _RL salt_addMass3Dplume(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL addMass3Dplume(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL HeatFlux3Dpl(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL SaltFlux3Dpl(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL addMass3Dbg(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL HeatFlux3Dbg(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL SaltFlux3Dbg(1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL iceplumeBG_TendT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL iceplumeBG_TendS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

CC runoffVel[,0,1] ::
CC runoffRad[,0,1] ::
catn     &     runoffVel, runoffVel0, runoffVel1,
catn     &     runoffRad, runoffRad0, runoffRad1,
catn      _RL runoffVel  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffVel0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffVel1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffRad  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffRad0 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
catn      _RL runoffRad1 (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)


C aProfPlume    :: integrated contact area of SG plume from bottom [m2]
C rProfPlume    :: depth vert prof. of SG plume [m]
C [w,u]ProfPlume   :: [vertical,hoz along-contact] vel vert prof. of SG plume [m]
C [t,s,u]ProfPlume :: [insitu-temp,salinity] vert prof of SG plume [degC,g/kg]
C mProfPlume    :: integrated melt rate of SG plume [m/day] 
C mIntProfPlume :: integrated melt of SG plume from bottom
C mProfAv       :: avg melt rate of combined SG plus BG [m/day]
C mProf         :: BG melt rate [m/day]
C [s,t,pt]Prof  :: [salt,temp,ptemp] vert prof of BG melt
C [u,v]Prof     :: absolute value of fjord [uVel,vVel] vert prof at BG melt contact
C VolFlux       :: Vert. vol flux of SG melt from below into cell [m3/s]
C VolFluxDiff   :: Net vertical convergence of SG melt (bot minus top) [m3/s] 
cC FwFlux1dbg    :: net BG mass flux, positive = into ocean, [kg/m2/s]
cC HeatFlux1dbg  :: net BG heatflux flux, negative = into ocean [W/m2]
C [z,pr]Prof    :: model [depth, pressure] vert prof [m,dbar]
C zProfAbs      :: absolute values of zProf [m]
cC delta_z       :: model drF

      COMMON /ICEPLUME_FIELDS_PROFILES/
     &     sProf, tProf, ptProf, prProf, uProf, vProf,
     &     rProfPlume, wProfPlume, tProfPlume, sProfPlume,
     &     uProfPlume, aProfPlume, mProfPlume, mIntProfPlume,
     &     mProfAv, mProf, zProf, zProfAbs,
     &     volFLux, volFluxDiff
catn the three fields below are only local to iceplume_calc
c     &     FwFlux1dbg, HeatFlux1dbg
c     &     delta_z
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
      _RL aProfPlume (Nr+1)
      _RL mIntProfPlume (Nr+1)
      _RL uProfPlume (Nr)
      _RL mProfPlume (Nr)
      _RL mProfAv    (Nr)
      _RL mProf      (Nr)
      _RL zProf      (Nr+1)
      _RL zProfAbs   (Nr+1)
      _RL volFlux     (Nr+1)
      _RL volFluxDiff (Nr)
c      _RL HeatFlux1dbg(Nr)
c      _RL FwFlux1dbg(Nr)
c      _RL delta_z(Nr)

#ifdef ICEPLUME_ALLOW_DETACHED_PLUME
C thetaProfPlume    ::
C distanceProfPlume ::

      COMMON /ICEPLUME_FIELDS_DETACHED/
     & thetaProfPlume, distanceProfPlume
      _RL thetaProfPlume (Nr+1)
      _RL distanceProfPlume (Nr+1)
#endif

# endif /* ALLOW_ICEPLUME */
