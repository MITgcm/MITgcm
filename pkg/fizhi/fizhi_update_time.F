C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_update_time.F,v 1.3 2004/07/29 15:19:44 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_update_time (myThid,deltat)
C***********************************************************************
C  Purpose
C  -------
C    Update Model Date and Time
C
C  Arguments  Description
C  ----------------------
C     myThid ... Process(or) number
C     deltat ... Time step to tick the clock
C***********************************************************************
      implicit none

#include "chronos.h"

      integer myThid
      _RL deltat

      integer ndt

C **********************************************************************
C ****           Call Routine to Tick the Clock                     ****
C **********************************************************************
      ndt = deltat
      call tick(nymd,nhms,ndt)

      if(mythid.eq.1) then
      print *, 'Current Date ',nymd,' Current Time ',nhms
      endif

      return
      end
