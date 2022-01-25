C---  Fields need in autodiff_store.F and autodiff_restore.F

#ifdef AUTODIFF_USE_STORE_RESTORE
      INTEGER NDV3D
# ifdef ALLOW_ADAMSBASHFORTH_3
      PARAMETER (NDV3D  = 14)
# else
      PARAMETER (NDV3D  = 10)
# endif
      _RL StoreDynVars3D
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,NDV3D)
      COMMON /AUTODIFF_STORE_DYN3D/ StoreDynVars3D

      INTEGER NDV2D
      PARAMETER (NDV2D  = 22)
      _RL StoreDynVars2D
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NDV2D)
      COMMON /AUTODIFF_STORE_DYN2D/ StoreDynVars2D

# ifdef ALLOW_EXF
      INTEGER NEXF1, NEXF2
      PARAMETER (NEXF1  = 23)
      PARAMETER (NEXF2  = 24)
      _RL StoreEXF1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NEXF1)
      COMMON /AUTODIFF_STORE_EXF_FLUX/
     &       StoreEXF1
      _RL StoreEXF2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NEXF2)
      COMMON /AUTODIFF_STORE_EXF_ATMOS/
     &       StoreEXF2

      INTEGER NCTRL1
      PARAMETER (NCTRL1 = 20)
      _RL StoreCTRLS1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NCTRL1)
      COMMON /AUTODIFF_STORE_CTRL/
     &       StoreCTRLS1
# endif

# ifdef ALLOW_SEAICE
      INTEGER NSI
      PARAMETER (NSI = 16+nITD)
      _RL StoreSEAICE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NSI)
      COMMON /AUTODIFF_STORE_SEAICE/
     &       StoreSEAICE
# endif
#endif /* AUTODIFF_USE_STORE_RESTORE */

#if ( defined ALLOW_OBCS && defined AUTODIFF_USE_STORE_RESTORE_OBCS )
      INTEGER NOB
      PARAMETER (NOB = 20)
      _RL StoreOBCSN(1-OLx:sNx+OLx,Nr,nSx,nSy,NOB)
      COMMON /AUTODIFF_STORE_OBCSN/
     &       StoreOBCSN
      _RL StoreOBCSS(1-OLx:sNx+OLx,Nr,nSx,nSy,NOB)
      COMMON /AUTODIFF_STORE_OBCSS/
     &       StoreOBCSS
      _RL StoreOBCSE(1-OLy:sNy+OLy,Nr,nSx,nSy,NOB)
      COMMON /AUTODIFF_STORE_OBCSE/
     &       StoreOBCSE
      _RL StoreOBCSW(1-OLy:sNy+OLy,Nr,nSx,nSy,NOB)
      COMMON /AUTODIFF_STORE_OBCSW/
     &       StoreOBCSW
#endif
