C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_diagalarms.F,v 1.5 2005/10/14 16:58:33 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_diagalarms (myThid)

C***********************************************************************        
C  Purpose                                                                      
C  -------                                                                      
C     Routine to Set Alarms for diagnostic output
C
C  Argument Description                                                         
C  --------------------                                                          
C     myThid .... Process ID
C
C NOTE: This routine ASSUMES that fizhi_init_fixed has been called
C       IT WILL NOT WORK OTHERWISE
C***********************************************************************        
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#include "chronos.h"

c Input Parameters
c ----------------
      integer  myThid

c Local variables
c -------------------
      integer  nhmsf, n, nincr 
      integer mmdd,hhmmss,nsecf2
      character *9 tagname
      integer  nymdb,nhmsb
      _RL absfreq

c Set Alarm Clocks
c ----------------
c To Set Begin Time for Alarm Clocks:
c  Increment  < 1 month - counting starts at model start time
c  Increment >= 1 month - counting starts at beginning of start month
c ----------------------------------------------------------------------

C-    2D/3D field diagnostics:
      do n = 1,nlists
       absfreq = abs(freq(n))
       if(absfreq.lt.100.) then
        nymdb = nymd
        nhmsb = nhms
       else
        nymdb = int(nymd/100) * 100 + 1
        nhmsb = 0
       endif
       write(tagname,'(A,I2.2)')'diagtag',n
       mmdd = int(absfreq)
       hhmmss = int((absfreq - int(absfreq))*1.e6)
       nincr = mmdd * 1.e6 + hhmmss
       call set_alarm(tagname,nymdb,nhmsb,nincr)
      enddo

C-    Global/Regional statistics diagnostics:
      do n = 1,diagSt_nbLists
       absfreq = abs(diagSt_freq(n))
       if(absfreq.lt.100.) then
        nymdb = nymd
        nhmsb = nhms
       else
        nymdb = int(nymd/100) * 100 + 1
        nhmsb = 0
       endif
       write(tagname,'(A,I2.2)')'diagStg',n
       mmdd = int(absfreq)
       hhmmss = int((absfreq - int(absfreq))*1.e6)
       nincr = mmdd * 1.e6 + hhmmss
       call set_alarm(tagname,nymdb,nhmsb,nincr)
      enddo

      return
      end
