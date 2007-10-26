C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/OBCS.h,v 1.16 2007/10/26 14:51:26 dimitri Exp $
C $Name:  $

#ifdef ALLOW_OBCS

C--   Private logical flag to record active status of package
      LOGICAL OBCSisON
      COMMON /OBCS_PACKAGE/ OBCSisON

C These are input arrays (of integers) that contain the *absolute*
C computational index of an open-boundary (OB) point.
C A zero (0) element means there is no corresponding OB in that column/row.
C The computational coordinate refers to "tracer" cells.
C For a northern/southern OB, the OB V point is to the south/north.
C For an eastern/western OB, the OB U point is to the west/east.
C eg.
C     OB_Jnorth(3)=34  means that:
C          T( 3 ,34) is a an OB point
C          U(3:4,34) is a an OB point
C          V( 4 ,34) is a an OB point
C while
C     OB_Jsouth(3)=1  means that:
C          T( 3 ,1) is a an OB point
C          U(3:4,1) is a an OB point
C          V( 4 ,2) is a an OB point
C
C For convenience, negative values for Jnorth/Ieast refer to
C points relative to the Northern/Eastern edges of the model
C eg. OB_Jnorth(3)=-1  means that the point (3,Ny) is a northern O-B.
C
      COMMON /PARM_IL_OB/
     & OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     & spongeThickness,
     & useOrlanskiNorth,useOrlanskiSouth,
     & useOrlanskiEast,useOrlanskiWest,
     & useOBCSsponge, useOBCSbalance, useOBCSprescribe,
     & OBCSprintDiags
      INTEGER OB_Jnorth(Nx)
      INTEGER OB_Jsouth(Nx)
      INTEGER OB_Ieast(Ny)
      INTEGER OB_Iwest(Ny)
      INTEGER spongeThickness
      LOGICAL useOrlanskiNorth
      LOGICAL useOrlanskiSouth
      LOGICAL useOrlanskiEast
      LOGICAL useOrlanskiWest
      LOGICAL useOBCSsponge
      LOGICAL useOBCSbalance
      LOGICAL useOBCSprescribe
      LOGICAL OBCSprintDiags

      COMMON /PARM_R_OB/
     & Urelaxobcsinner,Urelaxobcsbound,
     & Vrelaxobcsinner,Vrelaxobcsbound
      _RS Urelaxobcsinner
      _RS Urelaxobcsbound
      _RS Vrelaxobcsinner
      _RS Vrelaxobcsbound

C--   COMMON /GRID_OB/ Open boudary related stuff
C     OBNu is the U value imposed at the Northern OB
C     OBNv is the V value imposed at the Northern OB
C     OBNt is the T value imposed at the Northern OB
C     OBNu is the S value imposed at the Northern OB
C     OBNa is the ice AREA value imposed at the Northern OB
C     OBNh is the ice HEFF value imposed at the Northern OB
C     OBNsl is the ice HSALT value imposed at the Northern OB
C     OBNsn is the ice HSNOW value imposed at the Northern OB
C     OBNuice is the uice value imposed at the Northern OB
C     OBNvice is the vice value imposed at the Northern OB
C     etc
C
      COMMON /GRID_IND_OB/
     & OB_Jn,OB_Js,OB_Ie,OB_Iw
      INTEGER OB_Jn(1-Olx:sNx+Olx,nSx,nSy)
      INTEGER OB_Js(1-Olx:sNx+Olx,nSx,nSy)
      INTEGER OB_Ie(1-Oly:sNy+Oly,nSx,nSy)
      INTEGER OB_Iw(1-Oly:sNy+Oly,nSx,nSy)

#ifdef ALLOW_OBCS_NORTH
      COMMON /GRID_N_OB/ 
     &      OBNu,OBNv,OBNt,OBNs
      _RL OBNu (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNv (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNt (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNs (1-Olx:sNx+Olx,Nr,nSx,nSy)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_N_OB_AUX/ 
     &      OBNu0,OBNv0,OBNt0,OBNs0,
     &      OBNu1,OBNv1,OBNt1,OBNs1
      _RL OBNu0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNv0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNt0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNs0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNu1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNv1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNt1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBNs1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
#endif /* ALLOW_OBCS_PRESCRIBE */
#ifdef ALLOW_SEAICE
      COMMON /SEAICE_N_OB/ OBNa,OBNh,OBNa0,OBNh0,OBNa1,OBNh1,
     &     OBNsl,OBNsn,OBNsl0,OBNsn0,OBNsl1,OBNsn1,
     &     OBNuice,OBNvice,OBNuice0,OBNvice0,OBNuice1,OBNvice1
      _RL OBNa  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNh  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNa0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNh0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNa1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNh1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsl  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsn  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsl0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsn0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsl1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNsn1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNuice  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNvice  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNuice0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNvice0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNuice1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBNvice1 (1-Olx:sNx+Olx,nSx,nSy)
#endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      COMMON /GRID_S_OB/ 
     &      OBSu,OBSv,OBSt,OBSs
      _RL OBSu (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSv (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSt (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSs (1-Olx:sNx+Olx,Nr,nSx,nSy)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_S_OB_AUX/ 
     &      OBSu0,OBSv0,OBSt0,OBSs0,
     &      OBSu1,OBSv1,OBSt1,OBSs1
      _RL OBSu0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSv0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSt0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSs0 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSu1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSv1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSt1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSs1 (1-Olx:sNx+Olx,Nr,nSx,nSy)
#endif /* ALLOW_OBCS_PRESCRIBE */
#ifdef ALLOW_SEAICE
      COMMON /SEAICE_S_OB/ OBSa,OBSh,OBSa0,OBSh0,OBSa1,OBSh1,
     &     OBSsl,OBSsn,OBSsl0,OBSsn0,OBSsl1,OBSsn1,
     &     OBSuice,OBSvice,OBSuice0,OBSvice0,OBSuice1,OBSvice1
      _RL OBSa  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSh  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSa0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSh0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSa1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSh1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsl  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsn  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsl0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsn0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsl1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSsn1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSuice  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSvice  (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSuice0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSvice0 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSuice1 (1-Olx:sNx+Olx,nSx,nSy)
      _RL OBSvice1 (1-Olx:sNx+Olx,nSx,nSy)
#endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_OBCS_EAST
      COMMON /GRID_E_OB/ 
     &      OBEu,OBEv,OBEt,OBEs
      _RL OBEu (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEv (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEt (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEs (1-Oly:sNy+Oly,Nr,nSx,nSy)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_E_OB_AUX/ 
     &      OBEu0,OBEv0,OBEt0,OBEs0,
     &      OBEu1,OBEv1,OBEt1,OBEs1
      _RL OBEu0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEv0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEt0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEs0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEu1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEv1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEt1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBEs1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
#endif /* ALLOW_OBCS_PRESCRIBE */
#ifdef ALLOW_SEAICE
      COMMON /SEAICE_E_OB/ OBEa,OBEh,OBEa0,OBEh0,OBEa1,OBEh1,
     &     OBEsl,OBEsn,OBEsl0,OBEsn0,OBEsl1,OBEsn1,
     &     OBEuice,OBEvice,OBEuice0,OBEvice0,OBEuice1,OBEvice1
      _RL OBEa  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEh  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEa0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEh0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEa1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEh1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsl  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsn  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsl0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsn0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsl1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEsn1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEuice  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEvice  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEuice0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEvice0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEuice1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBEvice1 (1-Oly:sNy+Oly,nSx,nSy)
#endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      COMMON /GRID_W_OB/ 
     &      OBWu,OBWv,OBWt,OBWs
      _RL OBWu (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWv (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWt (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWs (1-Oly:sNy+Oly,Nr,nSx,nSy)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_W_OB_AUX/ 
     &      OBWu0,OBWv0,OBWt0,OBWs0,
     &      OBWu1,OBWv1,OBWt1,OBWs1
      _RL OBWu0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWv0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWt0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWs0 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWu1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWv1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWt1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWs1 (1-Oly:sNy+Oly,Nr,nSx,nSy)
#endif /* ALLOW_OBCS_PRESCRIBE */
#ifdef ALLOW_SEAICE
      COMMON /SEAICE_W_OB/ OBWa,OBWh,OBWa0,OBWh0,OBWa1,OBWh1,
     &     OBWsl,OBWsn,OBWsl0,OBWsn0,OBWsl1,OBWsn1,
     &     OBWuice,OBWvice,OBWuice0,OBWvice0,OBWuice1,OBWvice1
      _RL OBWa  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWh  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWa0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWh0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWa1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWh1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsl  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsn  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsl0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsn0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsl1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWsn1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWuice  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWvice  (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWuice0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWvice0 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWuice1 (1-Oly:sNy+Oly,nSx,nSy)
      _RL OBWvice1 (1-Oly:sNy+Oly,nSx,nSy)
#endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS_WEST */

      COMMON /OB_FILES/
     &      OBNuFile,OBNvFile,OBNtFile,OBNsFile,OBNaFile,OBNhFile,
     &      OBSuFile,OBSvFile,OBStFile,OBSsFile,OBSaFile,OBShFile,
     &      OBEuFile,OBEvFile,OBEtFile,OBEsFile,OBEaFile,OBEhFile,
     &      OBWuFile,OBWvFile,OBWtFile,OBWsFile,OBWaFile,OBWhFile,
     &      OBNslFile,OBSslFile,OBEslFile,OBWslFile,
     &      OBNsnFile,OBSsnFile,OBEsnFile,OBWsnFile,
     &      OBNuiceFile,OBSuiceFile,OBEuiceFile,OBWuiceFile,
     &      OBNviceFile,OBSviceFile,OBEviceFile,OBWviceFile
      CHARACTER*(MAX_LEN_FNAM)
     &      OBNuFile,OBNvFile,OBNtFile,OBNsFile,OBNaFile,OBNhFile,
     &      OBSuFile,OBSvFile,OBStFile,OBSsFile,OBSaFile,OBShFile,
     &      OBEuFile,OBEvFile,OBEtFile,OBEsFile,OBEaFile,OBEhFile,
     &      OBWuFile,OBWvFile,OBWtFile,OBWsFile,OBWaFile,OBWhFile,
     &      OBNslFile,OBSslFile,OBEslFile,OBWslFile,
     &      OBNsnFile,OBSsnFile,OBEsnFile,OBWsnFile,
     &      OBNuiceFile,OBSuiceFile,OBEuiceFile,OBWuiceFile,
     &      OBNviceFile,OBSviceFile,OBEviceFile,OBWviceFile

#ifdef ALLOW_NONHYDROSTATIC
      COMMON /GRID_OBNH/ 
     &  OBNw,OBSw,OBEw,OBWw
      _RL OBNw (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBSw (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RL OBEw (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL OBWw (1-Oly:sNy+Oly,Nr,nSx,nSy)
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef NONLIN_FRSURF
      COMMON /GRID_OB_NLFS/
     &  OBNhfac0,OBShfac0,OBEhfac0,OBWhfac0,
     &  OBNeta,  OBSeta,  OBEeta,  OBWeta
      _RS OBNhfac0(1-Olx:sNx+Olx,nSx,nSy)
      _RS OBShfac0(1-Olx:sNx+Olx,nSx,nSy)
      _RS OBEhfac0(1-Oly:sNy+Oly,nSx,nSy)
      _RS OBWhfac0(1-Oly:sNy+Oly,nSx,nSy)
      _RS OBNeta (1-Olx:sNx+Olx,nSx,nSy)
      _RS OBSeta (1-Olx:sNx+Olx,nSx,nSy)
      _RS OBEeta (1-Oly:sNy+Oly,nSx,nSy)
      _RS OBWeta (1-Oly:sNy+Oly,nSx,nSy)
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_OBCS
       common /gg_volflux/ shiftvel
       _RL shiftvel(2)
#endif

#endif /* ALLOW_OBCS */
