C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_alarms.F,v 1.13 2009/04/01 19:55:07 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_alarms (nymdb,nhmsb,deltaT)

C***********************************************************************
C  Purpose
C  -------
C     Driver to Set Internal Model Alarms
C
C  Argument Description
C  --------------------
C     nymdb ..... Begining Date to Start Alarm Clock
C     nhmsb ..... Begining Time to Start Alarm Clock
C     ndt ....... Model Timestep
C
C     Note: For ndt = 0, all Intermal Model Alarms are set to 1 second
C
C***********************************************************************
      implicit none

c Input Parameters
c ----------------
      integer  nymdb,nhmsb
      _RL deltaT

      integer  myid

c Physics Frequencies
c -------------------
      integer ndt
      integer  nhmsf, m,n, nd
      integer  ndlw, ndsw, ndturb, ndmoist

      myid = 1

      ndt = deltaT

      if( ndt.ne.0 ) then

C Longwave  Radiation Frequency
      ndlw = 003000
C Shortwave Radiation Frequency
      ndsw = 003000

c Turbulence
c ----------
      ndturb = 2*int( max(1.0,(300./iabs(ndt)+0.5)/2) ) * iabs(ndt)

      n  =  1
      m  = -1
      nd = ndturb
      do while ( mod(3*3600,nd).ne.0 )
      nd = ndturb + 2*n*m*ndt
      m  = -m
      n  =  n+1
      if( nd.lt.ndt ) then
          if( myid.eq.1 ) then
          print *
          print *, 'Turbulence cannot occur with Model Timestep = ',ndt
          print *
          endif
      call my_finalize
      call my_exit (101)
      endif
      enddo
      ndturb  = nhmsf( nd )

c Moist Processes
c ---------------
      ndmoist = int( 600./iabs(ndt) + 0.5 ) * iabs(ndt)

      n  =  1
      m  = -1
      nd = ndmoist
      do while ( mod(3*3600,nd).ne.0 )
      nd = ndmoist + n*m*ndt
      m  = -m
      n  =  n+1
      if( nd.lt.ndt ) then
          if( myid.eq.1 ) then
          print *
          print *, 'Convection cannot occur with Model Timestep = ',ndt
          print *
          endif
      call my_finalize
      call my_exit (101)
      endif
      enddo
      ndmoist = nhmsf( nd )

      else

c For NDT = 0, all Alarms set to 1 second
c ---------------------------------------
C Longwave  Radiation Frequency
      ndlw    = 000001
C Shortwave Radiation Frequency
      ndsw    = 000001
C Turbulence          Frequency
      ndturb  = 000001
C Moist Processes     Frequency
      ndmoist = 000001

      endif

c Set Alarm Clocks
c ----------------

      call set_alarm ( 'radsw',nymdb,nhmsb,ndsw    )
      call set_alarm ( 'radlw',nymdb,nhmsb,ndlw    )
      call set_alarm ( 'turb' ,nymdb,nhmsb,ndturb  )
      call set_alarm ( 'moist',nymdb,nhmsb,ndmoist )
      call set_alarm ( 'pnt',nymdb,nhmsb,0 )

      return
      end
