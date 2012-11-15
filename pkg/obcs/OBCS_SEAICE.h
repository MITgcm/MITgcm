C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_SEAICE.h,v 1.4 2012/11/15 15:55:42 dimitri Exp $
C $Name:  $

#ifdef ALLOW_OBCS

CBOP
C     !ROUTINE: OBCS_SEAICE.h
C     !INTERFACE:
C     #include "OBCS_SEAICE.h"

C     !DESCRIPTION:
C     *==========================================================*
C     | OBCS_SEAICE.h
C     | o Header file containing
C     |   OBCS seaice parameters and OB values of seaice fields.
C     *==========================================================*
CEOP

C seaiceSpongeThickness  :: number grid points that make up the sponge layer (def=0)
      COMMON /OBC_SEAICE_PARM_I/
     & seaiceSpongeThickness
      INTEGER seaiceSpongeThickness

C useSeaiceSponge :: turns on seaice sponge layer along boundary (def=false)
      COMMON /OBC_SEAICE_PARM_L/
     & useSeaiceSponge
      LOGICAL useSeaiceSponge

C [A,H,SL,SN]relaxobcs[inner,bound] :: relaxation time scale (in seconds) on the
C                            boundary (bound) and at the innermost grid point of the
C                            sponge layer (inner); relaxation time scales in-between
C                            are linearly interpolated from these values
      COMMON /OBC_SEAICE_PARM_R/
     &     Arelaxobcsinner,  Arelaxobcsbound,
     &     Hrelaxobcsinner,  Hrelaxobcsbound,
     &    SLrelaxobcsinner, SLrelaxobcsbound,
     &    SNrelaxobcsinner, SNrelaxobcsbound
      _RS  Arelaxobcsinner,  Arelaxobcsbound
      _RS  Hrelaxobcsinner,  Hrelaxobcsbound
      _RS SLrelaxobcsinner, SLrelaxobcsbound
      _RS SNrelaxobcsinner, SNrelaxobcsbound

C OB[N,S,E,W][a,h,sn,sl,uice,vice]File :: Files with boundary conditions,
C                                         the letter combinations mean:
C                     N/S/E/W   :: northern/southern/eastern/western boundary
C                     a/h       :: sea ice concentration/effective thickness
C                     sn/sl     :: effective snow thickness/sea ice salinity
C                     uice/vice :: sea ice u/v drift velocities

      COMMON /OBC_SEAICE_FILES/
     &      OBNaFile,   OBSaFile,   OBEaFile,   OBWaFile,
     &      OBNhFile,   OBShFile,   OBEhFile,   OBWhFile,
     &      OBNslFile,  OBSslFile,  OBEslFile,  OBWslFile,
     &      OBNsnFile,  OBSsnFile,  OBEsnFile,  OBWsnFile,
     &      OBNuiceFile,OBSuiceFile,OBEuiceFile,OBWuiceFile,
     &      OBNviceFile,OBSviceFile,OBEviceFile,OBWviceFile
      CHARACTER*(MAX_LEN_FNAM)
     &      OBNaFile,   OBSaFile,   OBEaFile,   OBWaFile,
     &      OBNhFile,   OBShFile,   OBEhFile,   OBWhFile,
     &      OBNslFile,  OBSslFile,  OBEslFile,  OBWslFile,
     &      OBNsnFile,  OBSsnFile,  OBEsnFile,  OBWsnFile,
     &      OBNuiceFile,OBSuiceFile,OBEuiceFile,OBWuiceFile,
     &      OBNviceFile,OBSviceFile,OBEviceFile,OBWviceFile

#ifdef ALLOW_SEAICE
C--   COMMON /OBC_SEAICE_NSEW/ Open boundary values of seaice fields
C     OBNa is the ice AREA value imposed at the Northern OB
C     OBNh is the ice HEFF value imposed at the Northern OB
C     OBNsl is the ice HSALT value imposed at the Northern OB
C     OBNsn is the ice HSNOW value imposed at the Northern OB
C     OBNuice is the uice value imposed at the Northern OB
C     OBNvice is the vice value imposed at the Northern OB
C     etc

#ifdef ALLOW_OBCS_NORTH
      COMMON /OBC_SEAICE_N/
     &    OBNa , OBNh , OBNsl , OBNsn , OBNuice , OBNvice ,
     &    OBNa0, OBNh0, OBNsl0, OBNsn0, OBNuice0, OBNvice0,
     &    OBNa1, OBNh1, OBNsl1, OBNsn1, OBNuice1, OBNvice1
      _RL OBNa    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNh    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsl   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsn   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNuice (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNvice (1-Olx:sNx+Olx,nSx,nSy)
c#ifdef ALLOW_OBCS_PRESCRIBE
      _RL OBNa0   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNh0   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsl0  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsn0  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNuice0(1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNvice0(1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNa1   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNh1   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsl1  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsn1  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNuice1(1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNvice1(1-Olx:sNx+Olx,nSx,nSy)
c#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      COMMON /OBC_SEAICE_S/
     &    OBSa , OBSh , OBSsl , OBSsn , OBSuice , OBSvice ,
     &    OBSa0, OBSh0, OBSsl0, OBSsn0, OBSuice0, OBSvice0,
     &    OBSa1, OBSh1, OBSsl1, OBSsn1, OBSuice1, OBSvice1
      _RL OBSa    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSh    (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsl   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsn   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSuice (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSvice (1-Olx:sNx+Olx,nSx,nSy)
c#ifdef ALLOW_OBCS_PRESCRIBE
      _RL OBSa0   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSh0   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsl0  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsn0  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSuice0(1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSvice0(1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSa1   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSh1   (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsl1  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsn1  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSuice1(1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSvice1(1-Olx:sNx+Olx,nSx,nSy)
c#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_OBCS_EAST
      COMMON /OBC_SEAICE_E/
     &    OBEa , OBEh , OBEsl , OBEsn , OBEuice , OBEvice ,
     &    OBEa0, OBEh0, OBEsl0, OBEsn0, OBEuice0, OBEvice0,
     &    OBEa1, OBEh1, OBEsl1, OBEsn1, OBEuice1, OBEvice1
      _RL OBEa    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEh    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsl   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsn   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEuice (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEvice (1-Oly:sNy+Oly,nSx,nSy)
c#ifdef ALLOW_OBCS_PRESCRIBE
      _RL OBEa0   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEh0   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsl0  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsn0  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEuice0(1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEvice0(1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEa1   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEh1   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsl1  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsn1  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEuice1(1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEvice1(1-Oly:sNy+Oly,nSx,nSy)
c#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      COMMON /OBC_SEAICE_W/
     &    OBWa , OBWh , OBWsl , OBWsn , OBWuice , OBWvice ,
     &    OBWa0, OBWh0, OBWsl0, OBWsn0, OBWuice0, OBWvice0,
     &    OBWa1, OBWh1, OBWsl1, OBWsn1, OBWuice1, OBWvice1
      _RL OBWa    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWh    (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsl   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsn   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWuice (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWvice (1-Oly:sNy+Oly,nSx,nSy)
c#ifdef ALLOW_OBCS_PRESCRIBE
      _RL OBWa0   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWh0   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsl0  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsn0  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWuice0(1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWvice0(1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWa1   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWh1   (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsl1  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsn1  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWuice1(1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWvice1(1-Oly:sNy+Oly,nSx,nSy)
c#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS */
