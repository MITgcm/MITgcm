C $Header: /u/gcmpack/MITgcm/pkg/cost/Attic/cost_summary.F,v 1.1 2001/03/25 22:33:54 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_Summary(
     I                         mythid
     &                       )

c     ==================================================================
c     SUBROUTINE cost_Summary
c     ==================================================================
c
c     o Summarize the cost function related parts of the ECCO release.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_Summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"

c     == routine arguments ==

      integer mythid 

c     == local variables ==

      integer il

      character*(max_len_mbuf) msgbuf

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ECCO cost function configuration  >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a)')
     &'  Multipliers for the indivdual cost function contributions:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Net heat flux:                ',mult_hq
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Salt flux:                    ',mult_hs
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Zonal wind stress:            ',mult_tauu
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Meridional wind stress:       ',mult_tauv
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Mean sea surface height:      ',mult_hmean
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Sea surface height anomalies: ',mult_h
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Temperature (3d):             ',mult_temp
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Salinity (3d):                ',mult_salt
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Sea surface temperature:      ',mult_sst
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  CTD temperature:              ',mult_ctdt
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  CTD salinity:                 ',mult_ctds
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
     

       write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      il = ilnblnk(tdatfile)
      write(msgbuf,'(a,a)')
     &'  Temperature data are read from: ',tdatfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(sdatfile)
      write(msgbuf,'(a,a)')
     &'  Salinity data are read from:    ',sdatfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      if (using_topex) then
        il = ilnblnk(topexfile)
        write(msgbuf,'(a,a)')
     &  '  T/P data are read from:         ',topexfile(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        il = ilnblnk(topexmeanfile)
        write(msgbuf,'(a,a)')
     &  '  T/P mean is read from:          ',topexmeanfile(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i10,i8)')
     &  '  T/P start date is:              ',
     &                        topexstartdate(1),
     &                        topexstartdate(2)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,e18.11)')
     &  '  T/P sampling period is:         ',
     &                        topexperiod
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      else
        write(msgbuf,'(a)')
     &  '  T/P data not used.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      endif

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      if (using_ers) then
        il = ilnblnk(ersfile)
        write(msgbuf,'(a,a)')
     &  '  ERS data are read from:         ',ersfile(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i10,i8)')
     &  '  ERS start date is:              ',
     &                        ersstartdate(1),
     &                        ersstartdate(2)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,e18.11)')
     &  '  ERS sampling period is:         ',
     &                        ersperiod
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      else
        write(msgbuf,'(a)')
     &  '  ERS data not used.'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      endif

      il = ilnblnk(sstdatfile)
      write(msgbuf,'(a,a)')
     &'  SST data are read from:         ',sstdatfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i10,i8)')
     &'  SST start date is:              ',
     &                      sststartdate(1),
     &                      sststartdate(2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
     
      write(msgbuf,'(a,a)')
     &'  CTD temperature data are read from:',ctdtfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
     
      write(msgbuf,'(a,a)')
     &'  CTD salinity data are read from: ',ctdsfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
     
     
     

      il = ilnblnk(data_errfile)
      write(msgbuf,'(a,a)')
     &'  Error "data" file:              ',data_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(hflux_errfile)
      write(msgbuf,'(a,a)')
     &'  Heat flux error file:           ',hflux_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(sflux_errfile)
      write(msgbuf,'(a,a)')
     &'  Salt flux error file:           ',sflux_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(tauu_errfile)
      write(msgbuf,'(a,a)')
     &'  Zonal wind stress error file:   ',tauu_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(tauv_errfile)
      write(msgbuf,'(a,a)')
     &'  Merid. wind stress error file:  ',tauv_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(ssh_errfile)
      write(msgbuf,'(a,a)')
     &'  SSH error file:                 ',ssh_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(geoid_errfile)
      write(msgbuf,'(a,a)')
     &'  Geoid error file:               ',geoid_errfile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(geoid_covariancefile)
      write(msgbuf,'(a,a)')
     &'  Geoid error covariance file:    ',geoid_covariancefile(1:il)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
c      il = ilnblnk(ctdt_errfile)
c      write(msgbuf,'(a,a)')
c     &'  CTD temperature error file:    ',ctdt_errfile(1:il)
c      call print_message( msgbuf, standardmessageunit,
c     &                    SQUEEZE_RIGHT , mythid)
c      il = ilnblnk(ctdt_errfile)
c      write(msgbuf,'(a,a)')
c     &'  CTD salinity error file:       ',ctds_errfile(1:il)
c      call print_message( msgbuf, standardmessageunit,
c     &                    SQUEEZE_RIGHT , mythid)
      
      
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// Optimization configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      return
      end

