C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_diagalarms.F,v 1.1 2005/05/16 23:41:18 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_diagalarms (nymdb,nhmsb,myThid)

C***********************************************************************        
C  Purpose                                                                      
C  -------                                                                      
C     Routine to Set Alarms for diagnostic output
C
C  Argument Description                                                         
C  --------------------                                                          
C     nymdb ..... Begining Date to Start Alarm Clock 
C     nhmsb ..... Begining Time to Start Alarm Clock 
C     myThid .... Process ID
C
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
      integer  nymdb,nhmsb,myThid

c Physics Frequencies
c -------------------
      integer  nhmsf, n, nincr 
      integer mmdd,hhmmss,nsecf2
      character *9 tagname
                                                                                
c Set Alarm Clocks
c ----------------

      do n = 1,nlists
       write(tagname,'(A,I2.2)')'diagtag',n
       mmdd = int(freq(n))
       hhmmss = int((freq(n) - int(freq(n)))*1.e6)
       nincr = mmdd * 1.e4 + hhmmss
       call set_alarm(tagname,nymdb,nhmsb,nincr)
      enddo

      return
      end
