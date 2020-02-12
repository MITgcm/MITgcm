#ifdef ALLOW_AUTODIFF_WHTAPEIO
      COMMON /AUTODIFF_WHTAPEIO_I/
     &  tapeFileCounter, tapeMaxCounter, tapeFileUnit, tapeFileUnitS
      INTEGER tapeFileCounter, tapeMaxCounter
      INTEGER tapeFileUnit, tapeFileUnitS(4)
      COMMON /AUTODIFF_WHTAPEIO_L/
     &  tapeConcatIO, tapeSingleCpuIO, tapeBufferIO
      logical tapeConcatIO, tapeSingleCpuIO, tapeBufferIO
#endif

c ad dump record number (used only if dumpAdByRec is true)
      INTEGER dumpAdRecMn
      INTEGER dumpAdRecDy
      INTEGER dumpAdRecSi
      INTEGER dumpAdRecEt
      COMMON /AUTODIFF_DUMP_AD_REC/
     &       dumpAdRecMn, dumpAdRecDy, dumpAdRecSi,
     &       dumpAdRecEt

      INTEGER ilev_1
      INTEGER ilev_2
      INTEGER ilev_3
      INTEGER ilev_4
      INTEGER max_lev2
      INTEGER max_lev3
      INTEGER max_lev4
      INTEGER NDV3D, NDV2D, NEXF1, NEXF2, NCTRL1, NOB, NSI
#ifdef ALLOW_ADAMSBASHFORTH_3
      PARAMETER (NDV3D  = 14)
#else
      PARAMETER (NDV3D  = 10)
#endif
      PARAMETER (NDV2D  = 22)
      PARAMETER (NEXF1  = 23)
      PARAMETER (NEXF2  = 24)
      PARAMETER (NCTRL1 = 20)
      PARAMETER (NOB = 20)
      PARAMETER (NSI = 19)
      _RL StoreDynVars3D
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,NDV3D)
      _RL StoreDynVars2D
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NDV2D)
      _RL StoreEXF1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NEXF1)
      _RL StoreEXF2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NEXF2)
      _RL StoreCTRLS1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NCTRL1)
      _RL StoreOBCSN(1-OLx:sNx+OLx,Nr,nSx,nSy,NOB)
      _RL StoreOBCSS(1-OLx:sNx+OLx,Nr,nSx,nSy,NOB)
      _RL StoreOBCSE(1-OLy:sNy+OLy,Nr,nSx,nSy,NOB)
      _RL StoreOBCSW(1-OLy:sNy+OLy,Nr,nSx,nSy,NOB)
      _RL StoreSEAICE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,NSI)

      COMMON /AUTODIFF_STORE_DYN/
     &       StoreDynVars3D,
     &       StoreDynVars2D
      COMMON /AUTODIFF_STORE_EXF_FLUX/
     &       StoreEXF1
      COMMON /AUTODIFF_STORE_EXF_ATMOS/
     &       StoreEXF2
      COMMON /AUTODIFF_STORE_CTRL/
     &       StoreCTRLS1
      COMMON /AUTODIFF_STORE_OBCSN/
     &       StoreOBCSN
      COMMON /AUTODIFF_STORE_OBCSS/
     &       StoreOBCSS
      COMMON /AUTODIFF_STORE_OBCSE/
     &       StoreOBCSE
      COMMON /AUTODIFF_STORE_OBCSW/
     &       StoreOBCSW
      COMMON /AUTODIFF_STORE_SEAICE/
     &       StoreSEAICE

