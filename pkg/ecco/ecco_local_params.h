C     ==================================================================
C     HEADER ECCO_legacy
C     ==================================================================

      INTEGER NSSHV4COST
      PARAMETER ( NSSHV4COST=5 )

      character*(MAX_LEN_FNAM) tbarfile
      character*(MAX_LEN_FNAM) sbarfile
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(MAX_LEN_FNAM) sigmaRbarfile
#endif
      character*(MAX_LEN_FNAM) sstbarfile
      character*(MAX_LEN_FNAM) psbarfile
      character*(MAX_LEN_FNAM) bpbarfile
      character*(MAX_LEN_FNAM) iestaubarfile
      character*(MAX_LEN_FNAM) ubarfile
      character*(MAX_LEN_FNAM) vbarfile
      character*(MAX_LEN_FNAM) wbarfile
      character*(MAX_LEN_FNAM) tauxbarfile
      character*(MAX_LEN_FNAM) tauybarfile
      character*(MAX_LEN_FNAM) hfluxmeanbarfile
      character*(MAX_LEN_FNAM) sfluxmeanbarfile
      character*(MAX_LEN_FNAM) costTranspDataFile

#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
      _RL transpbar(maxNumDays,nSx,nSy)
      _RL transpobs(maxNumDays)
      _RL wtransp(maxNumDays)
#endif

      _RL  mult_hflux
      _RL  mult_sflux
      _RL  mult_hfluxmm
      _RL  mult_sfluxmm
      _RL  mult_tauu
      _RL  mult_tauv
      _RL  mult_hmean
      _RL  mult_h
      _RL  mult_tp
      _RL  mult_ers
      _RL  mult_gfo
      _RL  mult_sshv4cost(NSSHV4COST)
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL  mult_sigmaR
#endif
      _RL  mult_temp
      _RL  mult_salt
      _RL  mult_temp0
      _RL  mult_salt0
      _RL  mult_etan0
      _RL  mult_uvel0
      _RL  mult_vvel0
      _RL  mult_sst
      _RL  mult_tmi
      _RL  mult_sss
      _RL  mult_bp
      _RL  mult_ies
      _RL  mult_ctdt
      _RL  mult_ctds
      _RL  mult_ctdtclim
      _RL  mult_ctdsclim
      _RL  mult_xbt
      _RL  mult_argot
      _RL  mult_argos
      _RL  mult_usercost(NUSERCOST)
      _RL  mult_drift
      _RL  mult_tdrift
      _RL  mult_sdrift
      _RL  mult_wdrift
      _RL  mult_scatx
      _RL  mult_scaty
      _RL  mult_atemp
      _RL  mult_aqh
      _RL  mult_precip
      _RL  mult_swflux
      _RL  mult_swdown
      _RL  mult_snowprecip
      _RL  mult_lwflux
      _RL  mult_lwdown
      _RL  mult_evap
      _RL  mult_apressure
c     _RL  mult_runoff
      _RL  mult_uwind
      _RL  mult_vwind
      _RL  mult_curmtr
      _RL  mult_kapgm
      _RL  mult_kapredi
      _RL  mult_diffkr
c     _RL  mult_ini_fin
      _RL  mult_edtau
      _RL  mult_bottomdrag
      _RL  mult_smooth_ic
      _RL  mult_smooth_bc
      _RL  mult_transp

      character*(MAX_LEN_FNAM) hflux_errfile
      character*(MAX_LEN_FNAM) sflux_errfile
      character*(MAX_LEN_FNAM) tauu_errfile
      character*(MAX_LEN_FNAM) tauv_errfile
c     character*(MAX_LEN_FNAM) hfluxm_errfile
c     character*(MAX_LEN_FNAM) sfluxm_errfile
c     character*(MAX_LEN_FNAM) tauum_errfile
c     character*(MAX_LEN_FNAM) tauvm_errfile
      character*(MAX_LEN_FNAM) scatx_errfile
      character*(MAX_LEN_FNAM) scaty_errfile
C     moved to OBCS_CTRL.h because this file is used for obcs-ctrl parameters
c     character*(MAX_LEN_FNAM) data_errfile
      character*(MAX_LEN_FNAM) geoid_errfile
      character*(MAX_LEN_FNAM) geoid_covariancefile
      character*(MAX_LEN_FNAM) ssh_errfile
      character*(MAX_LEN_FNAM) tp_errfile
      character*(MAX_LEN_FNAM) ers_errfile
      character*(MAX_LEN_FNAM) gfo_errfile
      character*(MAX_LEN_FNAM) sshv4cost_scalefile(NSSHV4COST)
      character*(MAX_LEN_FNAM) sshv4cost_errfile(NSSHV4COST)
c     character*(MAX_LEN_FNAM) ctdt_errfile
c     character*(MAX_LEN_FNAM) ctds_errfile
c     character*(MAX_LEN_FNAM) drift_errfile
      character*(MAX_LEN_FNAM) udrifterrfile
      character*(MAX_LEN_FNAM) vdrifterrfile
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(MAX_LEN_FNAM) sigmaRerrfile
#endif
      character*(MAX_LEN_FNAM) salterrfile
      character*(MAX_LEN_FNAM) temperrfile
      character*(MAX_LEN_FNAM) velerrfile
      character*(MAX_LEN_FNAM) salt0errfile
      character*(MAX_LEN_FNAM) temp0errfile
      character*(MAX_LEN_FNAM) etan0errfile
      character*(MAX_LEN_FNAM) uvel0errfile
      character*(MAX_LEN_FNAM) vvel0errfile
      character*(MAX_LEN_FNAM) vel0errfile
      character*(MAX_LEN_FNAM) ssterrfile
      character*(MAX_LEN_FNAM) ssserrfile
      character*(MAX_LEN_FNAM) bperrfile
      character*(MAX_LEN_FNAM) ieserrfile
      character*(MAX_LEN_FNAM) atemp_errfile
      character*(MAX_LEN_FNAM) aqh_errfile
      character*(MAX_LEN_FNAM) precip_errfile
      character*(MAX_LEN_FNAM) swflux_errfile
      character*(MAX_LEN_FNAM) swdown_errfile
      character*(MAX_LEN_FNAM) snowprecip_errfile
      character*(MAX_LEN_FNAM) lwflux_errfile
      character*(MAX_LEN_FNAM) lwdown_errfile
      character*(MAX_LEN_FNAM) evap_errfile
      character*(MAX_LEN_FNAM) apressure_errfile
c     character*(MAX_LEN_FNAM) runoff_errfile
      character*(MAX_LEN_FNAM) edtau_errfile
      character*(MAX_LEN_FNAM) kapgm_errfile
      character*(MAX_LEN_FNAM) kapredi_errfile
      character*(MAX_LEN_FNAM) diffkr_errfile
      character*(MAX_LEN_FNAM) bottomdrag_errfile
      character*(MAX_LEN_FNAM) usercost_errfile(NUSERCOST)
      character*(MAX_LEN_FNAM) uwind_errfile
      character*(MAX_LEN_FNAM) vwind_errfile

      _RL whflux0
      _RL wsflux0
      _RL wtau0
      _RL watemp0
      _RL waqh0
      _RL wprecip0
      _RL wswflux0
      _RL wswdown0
      _RL wsnowprecip0
      _RL wlwflux0
      _RL wlwdown0
      _RL wevap0
      _RL wapressure0
c     _RL wrunoff0
      _RL wbottomdrag0
      _RL wwind0
      _RL wdiffkr0
      _RL wkapgm0
      _RL wkapredi0
      _RL wedtau0

      _RL wmean_hflux
      _RL wmean_sflux
      _RL wmean_tau
      _RL wmean_atemp
      _RL wmean_aqh
      _RL wmean_precip
      _RL wmean_swflux
      _RL wmean_swdown
      _RL wmean_snowprecip
      _RL wmean_lwflux
      _RL wmean_lwdown
      _RL wmean_evap
      _RL wmean_apressure
c     _RL wmean_runoff
      _RL wmean_wind

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(MAX_LEN_FNAM) sigmaRdatfile
#endif
      character*(MAX_LEN_FNAM) tdatfile
      character*(MAX_LEN_FNAM) sdatfile
      character*(MAX_LEN_FNAM) scatxdatfile
      character*(MAX_LEN_FNAM) scatydatfile
      character*(MAX_LEN_FNAM) sstdatfile
      character*(MAX_LEN_FNAM) tmidatfile
      character*(MAX_LEN_FNAM) sssdatfile
      character*(MAX_LEN_FNAM) bpdatfile
      character*(MAX_LEN_FNAM) iesdatfile
      character*(MAX_LEN_FNAM) mdtdatfile
      character*(MAX_LEN_FNAM) topexfile
      character*(MAX_LEN_FNAM) ersfile
      character*(MAX_LEN_FNAM) gfofile
      character*(MAX_LEN_FNAM) ctdtfile
      character*(MAX_LEN_FNAM) ctdsfile
      character*(MAX_LEN_FNAM) ctdtclimfile
      character*(MAX_LEN_FNAM) ctdsclimfile
      character*(MAX_LEN_FNAM) xbtfile
      character*(MAX_LEN_FNAM) argotfile
      character*(MAX_LEN_FNAM) argosfile
      character*(MAX_LEN_FNAM) usercost_datafile(NUSERCOST)
      character*(MAX_LEN_FNAM) udriftfile
      character*(MAX_LEN_FNAM) vdriftfile
      character*(MAX_LEN_FNAM) curmtrufile
      character*(MAX_LEN_FNAM) curmtrvfile

c     integer scatxstartdate(4)
c     integer scatystartdate(4)
c     integer sststartdate(4)
c     integer argotstartdate(4)
c     integer argosstartdate(4)
c     integer tmistartdate(4)
c     integer sssstartdate(4)
c     integer bpstartdate(4)
c     integer iesstartdate(4)
c     integer topexstartdate(4)
c     integer ersstartdate(4)
c     integer gfostartdate(4)
c     integer mdtstartdate(4)
c     integer mdtenddate(4)

      integer tmistartdate1
      integer tmistartdate2
      integer sststartdate1
      integer sststartdate2
      integer sssstartdate1
      integer sssstartdate2
      integer bpstartdate1
      integer bpstartdate2
      integer iesstartdate1
      integer iesstartdate2
      integer argotstartdate1
      integer argotstartdate2
      integer argosstartdate1
      integer argosstartdate2
      integer topexstartdate1
      integer topexstartdate2
      integer ersstartdate1
      integer ersstartdate2
      integer gfostartdate1
      integer gfostartdate2
      integer scatstartdate1
      integer scatstartdate2
      integer mdtstartdate1
      integer mdtstartdate2
      integer mdtenddate1
      integer mdtenddate2

      _RL topexperiod
      _RL ersperiod
      _RL gfoperiod
      _RL scatperiod

      _RL topexintercept
      _RL ersintercept
      _RL gfointercept
      _RL topexslope
      _RL ersslope
      _RL gfoslope

      _RL  sshv4cost_errfactor(NSSHV4COST)

      character*(MAX_LEN_FNAM) tpTimeMaskFile
      character*(MAX_LEN_FNAM) ersTimeMaskFile
      character*(MAX_LEN_FNAM) gfoTimeMaskFile

C     ==================================================================
C     END OF HEADER ECCO_legacy
C     ==================================================================
