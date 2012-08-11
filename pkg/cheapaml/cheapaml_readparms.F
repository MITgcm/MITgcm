C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_readparms.F,v 1.8 2012/08/11 19:50:47 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

      SUBROUTINE CHEAPAML_READPARMS( myThid )
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_READPARMS                            |
C     | o Routine to initialize CHEAPAML variables and consts.   |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CHEAPAML.h"

C     === Routine arguments ===
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   Bulk Formula parameter
      NAMELIST /CHEAPAML_CONST/
     &  cheapaml_ntim, cheapaml_mask_width,
     &  cheapaml_h, cheapaml_kdiff,
     &  cheapaml_taurelax,
     &  cdrag_1,cdrag_2,cdrag_3,
     &  rhoa, cpair, stefan, gasR,xkar,
     &   dsolms,dsolmn,zu,zt,zq,
     &   xphaseinit,gamma_blk,humid_fac,p0,
     &   cheap_pr1,cheap_pr2
      NAMELIST /CHEAPAML_PARM01/
     &         AirTempFile,
     &         SolarFile,
     &         UWindFile,
     &         VWindFile,
     &         TrFile,
     &         QrFile,
     &         AirQFile,
     &         UStressFile, VStressFile,
     &         WaveHFile,WavePFile,
     &         TracerFile,TracerRfile,
     &         CheapMaskFile,Cheap_hFile,
     &         cheap_clfile,
     &         periodicExternalForcing_cheap,
     &         externForcingPeriod_cheap,
     &         externForcingCycle_cheap,
     &         Cheap_prFile
      NAMELIST /CHEAPAML_PARM02/
     &         useFreshwaterFlux,
     &         useFluxLimit,
     &         FluxFormula,WaveModel,
     &         useStressOption,
     &         useCheapTracer ,
     &         usetimevarblh,
     &         useclouds


      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' BULKF_READPARMS: opening data.cheapaml'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)



      CALL OPEN_COPY_DATA_FILE(
     I                          'data.cheapaml', 'CHEAPAML_READPARMS',
     O                          iUnit,
     I                          myThid )


C--   Default values
C-    Physical constant :
      cheapaml_ntim = 5
      cheapaml_h=1000. _d 0
      cheapaml_kdiff=1.d4
      cheapaml_taurelax = .1 _d 0
      cheapaml_mask_width = 0
      rhoa   = 1.3 _d 0
      cpair  = 1004. _d 0
      stefan = 5.67 _d -8
      dsolms = 50. _d 0
      dsolmn = 25. _d 0
c heights of variables
      zu=10. _d 0
      zt=zu
      zq=zu
      xphaseinit = 0. _d 0
      cdrag_1=0.0027 _d 0
      cdrag_2=0.000142 _d 0
      cdrag_3=0.0000764 _d 0
c gas constant
      gasR=287.04 _d 0
c atmospheric adiabatic lapse rate
        gamma_blk=0.0098 _d 0
c humidity factor for computing virtual potential temperature
        humid_fac=0.606 _d 0
c surface pressure in mb
        p0=1000. _d 0
c latent heat (J/kg)
        lath=2.5d6
c Celsius to Kelvin
        Celsius2K=273.16 _d 0
c von Karman constant
        xkar=0.4 _d 0
c periodic forcing
        periodicExternalForcing_cheap = periodicExternalForcing
        externForcingPeriod_cheap = externForcingPeriod
        externForcingCycle_cheap = externForcingCycle
C precipitation time constant
        cheap_pr1 = 12*3600.0 _d 0
        cheap_pr2 = 120*3600.0 _d 0

C-    Input data files names :
      AirTempFile=' '
      AirQFile=' '
      SolarFile=' '
      UWindFile=' '
      VWindFile=' '
      TrFile=' '
      QrFile=' '
      WaveHFile=' '
      WavePFile=' '
      TracerFile=' '
      TracerRFile=' '
      CheapMaskFile=' '
      Cheap_hFile =' '
      Cheap_prFile =' '
      Cheap_clFile =' '
      FluxFormula='LANL'
      WaveModel='Smith'

C-    e-p option turned off by default
        useFreshwaterFlux=.FALSE.

C-    turn off flux limiting advection by default
        useFluxLimit=.FALSE.

C-    turn off stress option by default
        useStressOption=.FALSE.

C-    turn off passive tracer option by default
        useCheapTracer=.FALSE.


C-    turn off time varying BL height option by default
        usetimevarblh=.FALSE.

C-    turn off clouds option by default
        useclouds=.FALSE.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=CHEAPAML_CONST)
      WRITE(msgBuf,'(A)') ' CHEAPAML_READPARMS: read CHEAPAML_CONST'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=CHEAPAML_PARM01)
      WRITE(msgBuf,'(A)') ' CHEAPAML_READPARMS: read CHEAPAML_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=CHEAPAML_PARM02)
      WRITE(msgBuf,'(A)') ' CHEAPAML_READPARMS: read CHEAPAML_PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)



C--   Close the open data file
      CLOSE(iUnit)
c compute derivative values
c energy flux conversion factor
        xef=1. _d 0 /rhoa/cpair
c inverse of layer thickness
        hm=1. _d 0/cheapaml_h

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      iUnit = standardMessageUnit
      WRITE(iUnit,*) 'Caml: ntim     =',cheapaml_ntim
      WRITE(iUnit,*) 'Caml: mask_w   =',cheapaml_mask_width
      WRITE(iUnit,*) 'Caml: h        =',cheapaml_h
      WRITE(iUnit,*) 'Caml: kdiff    =',cheapaml_kdiff
      WRITE(iUnit,*) 'Caml: tau2     =',cheapaml_taurelax
      WRITE(iUnit,*) 'Caml: rhoa     =',rhoa
      WRITE(iUnit,*) 'Caml: cpair    =',cpair
      WRITE(iUnit,*) 'Caml: stefan   =',stefan
      Write(iUnit,*) 'Caml: useFreshwaterFlux  =',useFreshwaterFlux
      Write(iUnit,*) 'Caml: useFluxLimit       =',useFluxLimit
      Write(iUnit,*) 'Caml: useStressOption    =',useStressOption
      Write(iUnit,*) 'Caml: useCheapTracer     =',useCheapTracer
      Write(iUnit,*) 'Caml: usetimevarblh      =',usetimevarblh
      Write(iUnit,*) 'Caml: useclouds          =',usetimevarblh
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
