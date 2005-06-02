C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_diagalarms.F,v 1.3 2005/06/02 22:56:52 molod Exp $
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

c Set Alarm Clocks
c ----------------
c To Set Begin Time for Alarm Clocks:
c  Increment  < 1 month - counting starts at model start time
c  Increment >= 1 month - counting starts at beginning of start month
c ----------------------------------------------------------------------

C-    2D/3D field diagnostics:
      do n = 1,nlists
       if(freq(n).lt.100.) then
        nymdb = nymd
        nhmsb = nhms
       else
        nymdb = int(nymd/100) * 100 + 1
        nhmsb = 0
       endif
       write(tagname,'(A,I2.2)')'diagtag',n
       mmdd = int(freq(n))
       hhmmss = int((freq(n) - int(freq(n)))*1.e6)
       nincr = mmdd * 1.e6 + hhmmss
       call set_alarm(tagname,nymdb,nhmsb,nincr)
      enddo

C-    Global/Regional statistics diagnostics:
      do n = 1,diagSt_nbLists
       if(freq(n).lt.100.) then
        nymdb = nymd
        nhmsb = nhms
       else
        nymdb = int(nymd/100) * 100 + 1
        nhmsb = 0
       endif
       write(tagname,'(A,I2.2)')'diagStg',n
       mmdd = int(diagSt_freq(n))
       hhmmss = int((diagSt_freq(n) - int(diagSt_freq(n)))*1.e6)
       nincr = mmdd * 1.e6 + hhmmss
       call set_alarm(tagname,nymdb,nhmsb,nincr)
      enddo

      return
      end
