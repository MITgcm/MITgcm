c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_param.h,v 1.1 2001/05/14 22:08:40 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exf_clim_param
c     ==================================================================
c
c     o Header for the climatology part of the external forcing package.
c
c     started: Ralf Giering 15-Jan-2001
c
c     ==================================================================
c     HEADER exf_clim_param
c     ==================================================================

c     Calendar data.
      integer climtempstartdate1
      integer climtempstartdate2
      integer climtempstartdate(4)
      _RL     climtempperiod
      character*1 climtempmask
      parameter(  climtempmask = 's' )

      integer climsaltstartdate1
      integer climsaltstartdate2
      integer climsaltstartdate(4)
      _RL     climsaltperiod
      character*1 climsaltmask
      parameter(  climsaltmask = 's' )

      integer climsststartdate1
      integer climsststartdate2
      integer climsststartdate(4)
      _RL     climsstperiod
      character*1 climsstmask
      parameter(  climsstmask = 's' )

      integer climsssstartdate1
      integer climsssstartdate2
      integer climsssstartdate(4)
      _RL     climsssperiod
      character*1 climsssmask
      parameter(  climsssmask = 's' )

      common /exf_clim_i/
     &                        climtempstartdate1, climtempstartdate2,
     &                        climsaltstartdate1, climsaltstartdate2,
     &                        climsststartdate1,  climsststartdate2,
     &                        climsssstartdate1,  climsssstartdate2,
     &                        climtempstartdate,
     &                        climsaltstartdate,
     &                        climsststartdate,
     &                        climsssstartdate

      common /exf_clim_c/
     &                        climtempfile,
     &                        climsaltfile,
     &                        climsstfile,
     &                        climsssfile

      common /exf_clim_r/
     &                        climtempperiod,
     &                        climsaltperiod,
     &                        climsstperiod,
     &                        climsssperiod

c     File names.
      character*(128) climtempfile
      character*(128) climsaltfile
      character*(128) climsstfile
      character*(128) climsssfile

c     file precision and field type

      common /exf_clim_prec/ exf_clim_iprec
     &                     , exf_clim_yftype

      integer       exf_clim_iprec
      character*(2) exf_clim_yftype
