C $Header: /u/gcmpack/MITgcm/pkg/ptracers/PTRACERS_PARAMS.h,v 1.3 2007/12/17 22:03:15 jmc Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: PTRACERS_PARAMS.h
C    !INTERFACE:
C #include PTRACERS_PARAMS.h

C    !DESCRIPTION:
C Contains passive tracer parameters.

CEOP

C     COMMON /PTRACERS_PARAMS/  PTRACERS parameters:
C     PTRACERS_taveFreq :: Frequency with which time-averaged PTRACERS
C                          are written to post-processing files.
C     PTRACERS_ref      :: vertical profile for passive tracers, in
C                          analogy to tRef and sRef, hence the name
C     PTRACERS_EvPrRn   :: tracer concentration in Rain, Evap & RunOff
C       notes: a) used if both NonLin_FrSurf & useRealFreshWater are set.
C              b) use pTracer surface (local) value if = UNSET_RL (default)
C     PTRACERS_Iter0    :: timestep number when tracers are initialized

      _RL PTRACERS_dumpFreq
      _RL PTRACERS_taveFreq
      _RL PTRACERS_monitorFreq
      _RL PTRACERS_diffKh(PTRACERS_num)
      _RL PTRACERS_diffK4(PTRACERS_num)
      _RL PTRACERS_diffKrNr(Nr,PTRACERS_num)
      _RL PTRACERS_ref(Nr,PTRACERS_num)
      _RL PTRACERS_EvPrRn(PTRACERS_num)
      COMMON /PTRACERS_PARAMS_R/
     &     PTRACERS_dumpFreq,
     &     PTRACERS_taveFreq,
     &     PTRACERS_monitorFreq,
     &     PTRACERS_diffKh,
     &     PTRACERS_diffK4,
     &     PTRACERS_diffKrNr,
     &     PTRACERS_ref,
     &     PTRACERS_EvPrRn

      INTEGER PTRACERS_Iter0
      INTEGER PTRACERS_numInUse
      INTEGER PTRACERS_advScheme(PTRACERS_num)
      COMMON /PTRACERS_PARAMS_I/
     &     PTRACERS_Iter0,
     &     PTRACERS_numInUse,
     &     PTRACERS_advScheme

C     PTRACERS_MultiDimAdv  :: internal flag (depend on the advection scheme),
C                           true if this tracer uses Multi-Dim advection
C     PTRACERS_AdamsBashGtr :: internal flag (depend on the advection scheme),
C                           true if applies Adams-Bashforth on tracer tendency
      LOGICAL PTRACERS_ImplVertAdv(PTRACERS_num)
      LOGICAL PTRACERS_MultiDimAdv(PTRACERS_num)
      LOGICAL PTRACERS_AdamsBashGtr(PTRACERS_num)
      LOGICAL PTRACERS_useGMRedi(PTRACERS_num)
      LOGICAL PTRACERS_useKPP(PTRACERS_num)
      LOGICAL PTRACERS_useRecords
      LOGICAL
     &     PTRACERS_monitor_mnc, PTRACERS_monitor_stdio,
     &     PTRACERS_timeave_mdsio, PTRACERS_snapshot_mdsio,
     &     PTRACERS_pickup_write_mdsio, PTRACERS_pickup_read_mdsio,
     &     PTRACERS_timeave_mnc, PTRACERS_snapshot_mnc,
     &     PTRACERS_pickup_write_mnc, PTRACERS_pickup_read_mnc
      COMMON /PTRACERS_PARAMS_L/
     &     PTRACERS_ImplVertAdv,
     &     PTRACERS_MultiDimAdv,
     &     PTRACERS_AdamsBashGtr,
     &     PTRACERS_useGMRedi,
     &     PTRACERS_useKPP,
     &     PTRACERS_useRecords,
     &     PTRACERS_timeave_mdsio, PTRACERS_snapshot_mdsio,
     &     PTRACERS_pickup_write_mdsio, PTRACERS_pickup_read_mdsio,
     &     PTRACERS_monitor_stdio, PTRACERS_monitor_mnc,
     &     PTRACERS_timeave_mnc, PTRACERS_snapshot_mnc,
     &     PTRACERS_pickup_write_mnc, PTRACERS_pickup_read_mnc

      CHARACTER*(MAX_LEN_FNAM) PTRACERS_initialFile(PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) PTRACERS_names(PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) PTRACERS_long_names(PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) PTRACERS_units(PTRACERS_num)
      COMMON /PTRACERS_PARAMS_C/
     &     PTRACERS_initialFile,
     &     PTRACERS_names,
     &     PTRACERS_long_names,
     &     PTRACERS_units

C     COMMON /PTRACERS_LABELS/ holds pTracers labels
C     PTRACERS_ioLabel  :: pTracer I/O & diagnostics label (2 charecters long)
      COMMON /PTRACERS_LABELS/
     &     PTRACERS_ioLabel
      CHARACTER*2              PTRACERS_ioLabel(PTRACERS_num)

#endif /* ALLOW_PTRACERS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
