C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_summary.F,v 1.10 2006/11/01 09:49:36 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_SUMMARY
      SUBROUTINE SEAICE_SUMMARY( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_SUMMARY
C     | o Summarize pkg/seaice parameters.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#ifdef ALLOW_THSICE
#include "SIZE.h"
#include "PARAMS.h"
#endif /* ALLOW_THSICE */
#include "SEAICE_PARAMS.h"

C     !INPUT PARAMETERS:
C     == routine arguments ==
C     myThid  - thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == local variables ==
      INTEGER  il
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == external ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNk

C     == end of interface ==

      _BARRIER
      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)')
     &' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')
     &'// Seaice configuration (SEAICE_PARM01) >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')
     &' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      CALL WRITE_0D_L ( SEAICEwriteState,  INDEX_NONE,
     & 'SEAICEwriteState  =', ' /* write sea ice state to file */')
      CALL WRITE_0D_L ( SEAICEuseDYNAMICS, INDEX_NONE,
     & 'SEAICEuseDYNAMICS =', ' /* use dynamics */')
#ifdef SEAICE_CGRID
#ifdef SEAICE_ALLOW_EVP
      CALL WRITE_0D_L ( SEAICEuseEVP, INDEX_NONE,
     & 'SEAICEuseEVP      =', ' /* use EVP solver */')
#endif /* SEAICE_ALLOW_EVP */
      CALL WRITE_0D_L ( SEAICE_maskRHS, INDEX_NONE,
     & 'SEAICE_maskRHS    =', ' /* mask RHS of solver */')
      CALL WRITE_0D_L ( SEAICE_clipVelocities, INDEX_NONE,
     & 'SEAICE_clipVeloctities =', ' /* impose max. vels. */')
      CALL WRITE_0D_L ( useHB87stressCoupling, INDEX_NONE,
     & 'useHB87stressCoupling  =', ' /* altern. ice-ocean stress */')
#endif /* SEAICE_CGRID */
      CALL WRITE_0D_L ( SEAICEadvSnow, INDEX_NONE,
     & 'SEAICEadvSnow =', ' /* advect snow layer together with ice */')
      CALL WRITE_0D_L ( SEAICEuseFlooding, INDEX_NONE,
     & 'SEAICEuseFlooding =', ' /* turn submerged snow into ice */')
      CALL WRITE_0D_I ( LAD,               INDEX_NONE,
     & 'LAD               =', ' /* time stepping scheme */')
      CALL WRITE_0D_I ( IMAX_TICE,         INDEX_NONE,
     & 'IMAX_TICE         =', ' /* iterations for ice heat budget */')
      CALL WRITE_0D_I ( SEAICEadvScheme,   INDEX_NONE,
     & 'SEAICEadvScheme   =', ' /* advection scheme for thickness */')
      IF ( SEAICEadvScheme .EQ. 2 )
     & CALL WRITE_0D_L ( SEAICEuseFluxForm,   INDEX_NONE,
     & 'SEAICEuseFluxForm =', ' /* advection in FV flux form */')
#ifdef ALLOW_THSICE
      IF ( useThSice ) THEN
       CALL WRITE_0D_I ( SEAICEadvSchArea,   INDEX_NONE,
     & 'SEAICEadvSchArea   =', ' /* advection scheme for area */')
       CALL WRITE_0D_I ( SEAICEadvSchHeff,   INDEX_NONE,
     & 'SEAICEadvSchHeff   =', ' /* advection scheme for thickness */')
       CALL WRITE_0D_I ( SEAICEadvSchEnth,   INDEX_NONE,
     & 'SEAICEadvSchEnth   =', ' /* advection scheme for enthalphy */')
       CALL WRITE_0D_I ( SEAICEadvSchSnow,   INDEX_NONE,
     & 'SEAICEadvSchSnow   =', ' /* advection scheme for snow */')
      ENDIF
#endif
      CALL WRITE_0D_R8( SEAICE_deltaTtherm,INDEX_NONE,
     & 'SEAICE_deltaTtherm=', ' /* thermodynamic timestep */')
      CALL WRITE_0D_R8( SEAICE_deltaTdyn  ,INDEX_NONE,
     & 'SEAICE_deltaTdyn  =', ' /* dynamic timestep */')
#ifdef SEAICE_ALLOW_EVP
      CALL WRITE_0D_R8( SEAICE_deltaTevp  ,INDEX_NONE,
     & 'SEAICE_deltaTevp  =', ' /* EVP timestep */')
#endif
      CALL WRITE_0D_R8( SEAICE_monFreq,  INDEX_NONE,
     & 'SEAICE_monFreq  =',' /* monitor frequency */')
      CALL WRITE_0D_R8( SEAICE_dumpFreq   ,INDEX_NONE,
     & 'SEAICE_dumpFreq   =', ' /* dump frequency */')
      CALL WRITE_0D_R8( SEAICE_taveFreq   ,INDEX_NONE,
     & 'SEAICE_taveFreq   =', ' /* time-averaging frequency */')
      CALL WRITE_0D_L ( SEAICE_mon_stdio,  INDEX_NONE,
     & 'SEAICE_mon_stdio  =',' /* write monitor to std-outp */')
      CALL WRITE_0D_L ( SEAICE_dump_mdsio,  INDEX_NONE,
     & 'SEAICE_dump_mdsio =',' /* write snap-shot   using MDSIO */')
      CALL WRITE_0D_L ( SEAICE_tave_mdsio,  INDEX_NONE,
     & 'SEAICE_tave_mdsio =',' /* write TimeAverage using MDSIO */')
      CALL WRITE_0D_L ( SEAICE_mon_mnc,  INDEX_NONE,
     & 'SEAICE_mon_mnc    =',' /* write monitor to netcdf file */')
      CALL WRITE_0D_L ( SEAICE_dump_mnc,  INDEX_NONE,
     & 'SEAICE_dump_mnc   =',' /* write snap-shot   using MNC */')
      CALL WRITE_0D_L ( SEAICE_tave_mnc,  INDEX_NONE,
     & 'SEAICE_tave_mnc   =',' /* write TimeAverage using MNC */')
      CALL WRITE_0D_R8( SEAICE_initialHEFF,INDEX_NONE,
     & 'SEAICE_initialHEFF=', ' /* initial sea-ice thickness */')
      CALL WRITE_0D_R8( SEAICE_drag       ,INDEX_NONE,
     & 'SEAICE_drag       =', ' /* air-ice drag coefficient */')
      CALL WRITE_0D_R8( OCEAN_drag        ,INDEX_NONE,
     & 'OCEAN_drag        =', ' /* air-ocean drag coefficient */')
      CALL WRITE_0D_R8( SEAICE_waterDrag  ,INDEX_NONE,
     & 'SEAICE_waterDrag  =', ' /* water-ice drag * density */')
      CALL WRITE_0D_R8( SEAICE_dryIceAlb  ,INDEX_NONE,
     & 'SEAICE_dryIceAlb  =', ' /* winter albedo */')
      CALL WRITE_0D_R8( SEAICE_wetIceAlb  ,INDEX_NONE,
     & 'SEAICE_wetIceAlb  =', ' /* summer albedo */')
      CALL WRITE_0D_R8( SEAICE_drySnowAlb ,INDEX_NONE,
     & 'SEAICE_drySnowAlb =', ' /* dry snow albedo */')
      CALL WRITE_0D_R8( SEAICE_wetSnowAlb ,INDEX_NONE,
     & 'SEAICE_wetSnowAlb =', ' /* wet snow albedo */')
      CALL WRITE_0D_R8( SEAICE_waterAlbedo,INDEX_NONE,
     & 'SEAICE_waterAlbedo=', ' /* water albedo */')
      CALL WRITE_0D_R8( SEAICE_strength   ,INDEX_NONE,
     & 'SEAICE_strength   =', ' /* sea-ice strength Pstar */')
      CALL WRITE_0D_R8( SEAICE_sensHeat   ,INDEX_NONE,
     & 'SEAICE_sensHeat   =', ' /* sensible heat transfer */')
      CALL WRITE_0D_R8( SEAICE_latentWater,INDEX_NONE,
     & 'SEAICE_latentWater=', ' /* latent heat transfer for water */')
      CALL WRITE_0D_R8( SEAICE_latentIce  ,INDEX_NONE,
     & 'SEAICE_latentIce  =', ' /* latent heat transfer for ice */')
      CALL WRITE_0D_R8( SEAICE_iceConduct ,INDEX_NONE,
     & 'SEAICE_iceConduct =', ' /* sea-ice conductivity */')
      CALL WRITE_0D_R8( SEAICE_snowConduct,INDEX_NONE,
     & 'SEAICE_snowConduct=', ' /* snow conductivity */')
      CALL WRITE_0D_R8( SEAICE_emissivity ,INDEX_NONE,
     & 'SEAICE_emissivity =', ' /* Stefan-Boltzman * emissivity */')
      CALL WRITE_0D_R8( SEAICE_snowThick  ,INDEX_NONE,
     & 'SEAICE_snowThick  =', ' /* cutoff snow thickness */')
      CALL WRITE_0D_R8( SEAICE_shortwave  ,INDEX_NONE,
     & 'SEAICE_shortwave  =', ' /* penetration shortwave radiation */')
      CALL WRITE_0D_R8( SEAICE_freeze     ,INDEX_NONE,
     & 'SEAICE_freeze     =', ' /* freezing temp. of sea water */')
      WRITE(msgBuf,'(a)')
     &'Initial sea-ice thickness is read from file:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      il = ilnblnk(HeffFile)
      WRITE(msgBuf,'(A,A,A)')
     &'   >>  ',HeffFile(1:il),'  <<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      CALL WRITE_0D_R8( LSR_ERROR         ,INDEX_NONE,
     & 'LSR_ERROR         =', ' /* sets accuracy of LSR solver */')
      CALL WRITE_0D_R8( DIFF1             ,INDEX_NONE,
     & 'DIFF1             =', ' /* parameter used in advect.F */')
      CALL WRITE_0D_R8( A22               ,INDEX_NONE,
     & 'A22               =', ' /* parameter used in growth.F */')
      CALL WRITE_0D_R8( HO                ,INDEX_NONE,
     & 'HO                =', ' /* demarcation ice thickness */')
      CALL WRITE_0D_R8( MAX_HEFF          ,INDEX_NONE,
     & 'MAX_HEFF          =', ' /* maximum ice thickness */')
      CALL WRITE_0D_R8( MIN_ATEMP         ,INDEX_NONE,
     & 'MIN_ATEMP         =', ' /* minimum air temperature */')
      CALL WRITE_0D_R8( MIN_LWDOWN        ,INDEX_NONE,
     & 'MIN_LWDOWN        =', ' /* minimum downward longwave */')
      CALL WRITE_0D_R8( MAX_TICE          ,INDEX_NONE,
     & 'MAX_TICE          =', ' /* maximum ice temperature */')
      CALL WRITE_0D_R8( MIN_TICE          ,INDEX_NONE,
     & 'MIN_TICE          =', ' /* minimum ice temperature */')
      CALL WRITE_0D_R8( SEAICE_EPS        ,INDEX_NONE,
     & 'SEAICE_EPS        =', ' /* reduce derivative singularities */')
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')
     &'// Seaice configuration (SEAICE_PARM01) >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(A)')

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
