c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_param.h,v 1.1 2001/05/14 22:08:42 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exfa_sflx_time
c     ==================================================================
c
c     o Header file for the surface flux anomaly data.
c       Used by the external forcing package.
c
c     started: Ralf.Giering@FastOpt.de 24-Mai-2000
c
c     ==================================================================
c     HEADER exfa_sflx_time
c     ==================================================================

c     Calendar data.
      integer hfluxastartdate1
      integer hfluxastartdate2
      integer hfluxastartdate(4)
      _RL     hfluxaperiod
      character*1 hfluxamask
      parameter(  hfluxamask = 'c' )

      integer sfluxastartdate1
      integer sfluxastartdate2
      integer sfluxastartdate(4)
      _RL     sfluxaperiod
      character*1 sfluxamask
      parameter(  sfluxamask = 'c' )

      integer ustressastartdate1
      integer ustressastartdate2
      integer ustressastartdate(4)
      _RL     ustressaperiod
      character*1 ustressamask
      parameter(  ustressamask = 'c' )

      integer vstressastartdate1
      integer vstressastartdate2
      integer vstressastartdate(4)
      _RL     vstressaperiod
      character*1 vstressamask
      parameter(  vstressamask = 'c' )

      common /exfa_sfluxes_i/
     &                          hfluxastartdate1,  hfluxastartdate2,
     &                          sfluxastartdate1,  sfluxastartdate2,
     &                          ustressastartdate1,ustressastartdate2,
     &                          vstressastartdate1,vstressastartdate2,
     &                          hfluxastartdate,
     &                          sfluxastartdate,
     &                          ustressastartdate,
     &                          vstressastartdate

      common /exfa_sfluxes_r/
     &                          hfluxaperiod,
     &                          sfluxaperiod,
     &                          ustressaperiod,
     &                          vstressaperiod

c     File names.
      character*(128) hfluxafile
      character*(128) sfluxafile
      character*(128) ustressafile
      character*(128) vstressafile

      common /exfa_sfluxes_c/
     &                          hfluxafile,
     &                          sfluxafile,
     &                          ustressafile,
     &                          vstressafile

c     file precision and field type

      integer       exf_iprec
      character*(2) exf_yftype

      common /exfa_prec_type/ 
     &                        exf_iprec,
     &                        exf_yftype
