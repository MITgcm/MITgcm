C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_summary.F,v 1.2 2005/05/22 02:06:07 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_SUMMARY( mythid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_SUMMARY                                |
C     | o Summarize pkg/seaice parameters.                       |
C     \==========================================================/
      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SEAICE_PARAMS.h"

c     == routine arguments ==

c     mythid  - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

      integer  il
      character*(max_len_mbuf) msgbuf

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      _BARRIER
      _BEGIN_MASTER(myThid)

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// Seaice configuration (SEAICE_PARM01) >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      CALL WRITE_0D_L ( SEAICEwriteState,  INDEX_NONE,
     & 'SEAICEwriteState  =', ' /* write sea ice state to file */')
      CALL WRITE_0D_L ( SEAICEuseDYNAMICS, INDEX_NONE,
     & 'SEAICEuseDYNAMICS =', ' /* use dynamics */')
      CALL WRITE_0D_I ( LAD,               INDEX_NONE,
     & 'LAD               =', ' /* time stepping scheme */')
      CALL WRITE_0D_I ( IMAX_TICE,         INDEX_NONE,
     & 'IMAX_TICE         =', ' /* iterations for ice heat budget */')
      CALL WRITE_0D_R8( SEAICE_deltaTtherm,INDEX_NONE,
     & 'SEAICE_deltaTtherm=', ' /* thermodynamic timestep */')
      CALL WRITE_0D_R8( SEAICE_deltaTdyn  ,INDEX_NONE,
     & 'SEAICE_deltaTdyn  =', ' /* dynamic timestep */')
      CALL WRITE_0D_R8( SEAICE_dumpFreq   ,INDEX_NONE,
     & 'SEAICE_dumpFreq   =', ' /* dump frequency */')
      CALL WRITE_0D_R8( SEAICE_taveFreq   ,INDEX_NONE,
     & 'SEAICE_taveFreq   =', ' /* time-averaging frequency */')
      CALL WRITE_0D_L ( SEAICE_dump_mdsio,  INDEX_NONE,
     & 'SEAICE_dump_mdsio =',' /* write snap-shot   using MDSIO */')
      CALL WRITE_0D_L ( SEAICE_tave_mdsio,  INDEX_NONE,
     & 'SEAICE_tave_mdsio =',' /* write TimeAverage using MDSIO */')
c     CALL WRITE_0D_L ( SEAICE_mon_stdio,  INDEX_NONE,
c    & 'SEAICE_mon_stdio  =',' /* write monitor to std-outp */')
      CALL WRITE_0D_L ( SEAICE_dump_mnc,  INDEX_NONE,
     & 'SEAICE_dump_mnc   =',' /* write snap-shot   using MNC */')
      CALL WRITE_0D_L ( SEAICE_tave_mnc,  INDEX_NONE,
     & 'SEAICE_tave_mnc   =',' /* write TimeAverage using MNC */')
c     CALL WRITE_0D_L ( SEAICE_mon_mnc,  INDEX_NONE,
c    & 'SEAICE_mon_mnc    =',' /* write monitor to netcdf file */')
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
      write(msgbuf,'(a)')
     &'Initial sea-ice thickness is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(HeffFile)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',HeffFile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      WRITE(msgBuf,'(A)')   '    ;     '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
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
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// Seaice configuration (SEAICE_PARM01) >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')

      _END_MASTER(myThid)
      _BARRIER

      return
      end
