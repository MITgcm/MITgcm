C $Header: /u/gcmpack/MITgcm/pkg/monitor/MONITOR.h,v 1.2 2001/11/08 16:47:34 jmc Exp $
C $Name:  $
C
C     /==========================================================\
C     | MONITOR.h                                                |
C     |==========================================================|
C     | Parameters for "monitor" setup.                          |
C     | Monitor routines ( prefixed MON_ ) provide a simple      |
C     | set of utilities for outputting useful runtime diagnostic|
C     | information. They use a standard format so that the      |
C     | monitor output can be parsed offline to help in trouble  |
C     | shooting.                                                |
C     | Monitor setup params should be set through appropriate   |
C     | MON_ routines to ensure that changes are made in a       |
C     | thread-safe fashion.                                     |
C     \==========================================================/

C--   Monitor head and tail strings
      CHARACTER*(*) mon_head
      PARAMETER ( mon_head = '%MON'       )
      CHARACTER*(*) mon_foot_min
      PARAMETER ( mon_foot_min = '_min'   )
      CHARACTER*(*) mon_foot_max
      PARAMETER ( mon_foot_max = '_max'   )
      CHARACTER*(*) mon_foot_sd 
      PARAMETER ( mon_foot_sd = '_sd'     )
      CHARACTER*(*) mon_foot_mean
      PARAMETER ( mon_foot_mean = '_mean' )
      CHARACTER*(*) mon_foot_volint
      PARAMETER ( mon_foot_volint = '_volint' )
      CHARACTER*(*) mon_foot_volmean
      PARAMETER ( mon_foot_volmean = '_volmean' )
      CHARACTER*(*) mon_foot_del2
      PARAMETER ( mon_foot_del2 = '_del2' )
      CHARACTER*(*) mon_string_none
      PARAMETER ( mon_string_none = 'NONE')

C--   COMMON /MON_I/ Monitor integer variables
C     mon_ioUnit - Used to specify the output unit for monitor IO.
C     mon_prefL  - Prefix length of current mon_ prefix
      COMMON /MON_I/ mon_ioUnit, mon_prefL
      INTEGER mon_ioUnit 
      INTEGER mon_prefL

C--   COMMON /MON_C/ Monitor character variables
C     mon_pref   - Prefix used for monitor output
      COMMON /MON_C/ mon_pref
      CHARACTER*(MAX_LEN_MBUF) mon_pref
