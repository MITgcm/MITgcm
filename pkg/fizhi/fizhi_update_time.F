C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_update_time.F,v 1.1 2004/06/08 16:12:53 molod Exp $
C $Name:  $

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

      integer myThid, deltat

C **********************************************************************
C ****           Call Routine to Tick the Clock                     ****
C **********************************************************************

      call tick(nymd,nhms,deltat)

      if(mythid.eq.1) then
      print *, 'Current Date ',nymd,' Current Time ',nhms
      endif

      return
      end
