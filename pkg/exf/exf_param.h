c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_param.h,v 1.2 2002/02/07 20:00:09 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exf_param
c     ==================================================================
c
c     o Header file for the surface flux data. Used by the external
c       forcing package.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c              - Restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed some variables names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c              - included exf_iprec, exf_yftype to enable easy
c                switch between 32bit/64 bit data format
c
c              Patrick Heimbach, heimbach@mit.edu  01-May-2001
c              - added obcs parameters
c
c     ==================================================================
c     HEADER exf_param
c     ==================================================================

c     Calendar data.
      integer hfluxstartdate1
      integer hfluxstartdate2
      integer hfluxstartdate(4)
      _RL     hfluxperiod
      character*1 hfluxmask
      parameter(  hfluxmask = 's' )

      integer atempstartdate1
      integer atempstartdate2
      integer atempstartdate(4)
      _RL     atempperiod
      character*1 atempmask
      parameter(  atempmask = 's' )

      integer aqhstartdate1
      integer aqhstartdate2
      integer aqhstartdate(4)
      _RL     aqhperiod
      character*1 aqhmask
      parameter(  aqhmask = 's' )

      integer sfluxstartdate1
      integer sfluxstartdate2
      integer sfluxstartdate(4)
      _RL     sfluxperiod
      character*1 sfluxmask
      parameter(  sfluxmask = 's' )

      integer precipstartdate1
      integer precipstartdate2
      integer precipstartdate(4)
      _RL     precipperiod
      character*1 precipmask
      parameter(  precipmask = 's' )

      integer ustressstartdate1
      integer ustressstartdate2
      integer ustressstartdate(4)
      _RL     ustressperiod
      character*1 ustressmask
      parameter(  ustressmask = 'u' )

      integer vstressstartdate1
      integer vstressstartdate2
      integer vstressstartdate(4)
      _RL     vstressperiod
      character*1 vstressmask
      parameter(  vstressmask = 'v' )

      integer uwindstartdate1
      integer uwindstartdate2
      integer uwindstartdate(4)
      _RL     uwindperiod
      character*1 uwindmask
      parameter(  uwindmask = 'u' )

      integer vwindstartdate1
      integer vwindstartdate2
      integer vwindstartdate(4)
      _RL     vwindperiod
      character*1 vwindmask
      parameter(  vwindmask = 'v' )

      integer swfluxstartdate1
      integer swfluxstartdate2
      integer swfluxstartdate(4)
      _RL     swfluxperiod
      character*1 swfluxmask
      parameter(  swfluxmask = 's' )

      integer lwfluxstartdate1
      integer lwfluxstartdate2
      integer lwfluxstartdate(4)
      _RL     lwfluxperiod
      character*1 lwfluxmask
      parameter(  lwfluxmask = 's' )

      integer obcsNstartdate1
      integer obcsNstartdate2
      integer obcsNstartdate(4)
      _RL     obcsNperiod

      integer obcsSstartdate1
      integer obcsSstartdate2
      integer obcsSstartdate(4)
      _RL     obcsSperiod

      integer obcsEstartdate1
      integer obcsEstartdate2
      integer obcsEstartdate(4)
      _RL     obcsEperiod

      integer obcsWstartdate1
      integer obcsWstartdate2
      integer obcsWstartdate(4)
      _RL     obcsWperiod

c     File names.
      character*(128) hfluxfile
      character*(128) atempfile
      character*(128) aqhfile
      character*(128) precipfile
      character*(128) sfluxfile
      character*(128) ustressfile
      character*(128) vstressfile
      character*(128) uwindfile
      character*(128) vwindfile
      character*(128) swfluxfile
      character*(128) lwfluxfile

      common /exf_param_i/
     &                          hfluxstartdate1,   hfluxstartdate2,
     &                          atempstartdate1,   atempstartdate2,
     &                          aqhstartdate1,     aqhstartdate2,
     &                          sfluxstartdate1,   sfluxstartdate2,
     &                          precipstartdate1,  precipstartdate2,
     &                          ustressstartdate1, ustressstartdate2,
     &                          vstressstartdate1, vstressstartdate2,
     &                          uwindstartdate1,   uwindstartdate2,
     &                          vwindstartdate1,   vwindstartdate2,
     &                          swfluxstartdate1,  swfluxstartdate2,
     &                          lwfluxstartdate1,  lwfluxstartdate2,
     &                          obcsNstartdate1,   obcsNstartdate2,
     &                          obcsSstartdate1,   obcsSstartdate2,
     &                          obcsEstartdate1,   obcsEstartdate2,
     &                          obcsWstartdate1,   obcsWstartdate2,
     &                          hfluxstartdate,
     &                          atempstartdate,
     &                          aqhstartdate,
     &                          sfluxstartdate,
     &                          precipstartdate,
     &                          ustressstartdate,
     &                          vstressstartdate,
     &                          uwindstartdate,
     &                          vwindstartdate,
     &                          swfluxstartdate,
     &                          lwfluxstartdate,
     &                          obcsNstartdate,
     &                          obcsSstartdate,
     &                          obcsEstartdate,
     &                          obcsWstartdate

      common /exf_param_r/
     &                          hfluxperiod,
     &                          atempperiod,
     &                          aqhperiod,
     &                          sfluxperiod,
     &                          precipperiod,
     &                          ustressperiod,
     &                          vstressperiod,
     &                          uwindperiod,
     &                          vwindperiod,
     &                          swfluxperiod,
     &                          lwfluxperiod,
     &                          obcsNperiod,
     &                          obcsSperiod,
     &                          obcsEperiod,
     &                          obcsWperiod

      common /exf_param_c/
     &                          hfluxfile,
     &                          atempfile,
     &                          aqhfile,
     &                          sfluxfile,
     &                          precipfile,
     &                          ustressfile,
     &                          vstressfile,
     &                          uwindfile,
     &                          vwindfile,
     &                          swfluxfile,
     &                          lwfluxfile

c     file precision and field type

      common /exf_param_type/ 
     &                        exf_iprec,
     &                        exf_yftype

      integer exf_iprec
      character*(2) exf_yftype
