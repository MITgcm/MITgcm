#ifdef ALLOW_AUTODIFF_WHTAPEIO
      COMMON /AUTODIFF_WHTAPEIO_I/
     &  tapeFileCounter, tapeMaxCounter, tapeFileUnit, tapeFileUnitS
      INTEGER tapeFileCounter, tapeMaxCounter
      INTEGER tapeFileUnit, tapeFileUnitS(4)
      COMMON /AUTODIFF_WHTAPEIO_L/
     &  tapeConcatIO, tapeSingleCpuIO, tapeBufferIO
      LOGICAL tapeConcatIO, tapeSingleCpuIO, tapeBufferIO
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
