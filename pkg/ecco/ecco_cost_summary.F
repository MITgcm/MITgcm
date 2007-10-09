C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_cost_summary.F,v 1.3 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine ecco_cost_summary( mythid )

c     ==================================================================
c     SUBROUTINE ecco_cost_summary
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
c     SUBROUTINE ecco_cost_summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "ecco_cost.h"

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
     &'  Net heat flux:                ',mult_hflux
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Salt flux:                    ',mult_sflux
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
     &'  Temperature Lev.:             ',mult_temp
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Salinity Lev.:                ',mult_salt
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Temperature ini.:             ',mult_temp0
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Salinity ini.:                ',mult_salt0
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  TMI Sea surface temperature:  ',mult_tmi
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Sea surface temperature:      ',mult_sst
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Sea surface salinity:        ',mult_sss
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
      write(msgbuf,'(a,e10.3)')
     &'  CTD clim temperature:         ',mult_ctdtclim
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  CTD clim salinity:            ',mult_ctdsclim
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  XBT Temperature:              ',mult_xbt
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  ARGO Temperature:              ',mult_argot
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  ARGO Salt:                     ',mult_argos
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  drifter velocities:           ',mult_drift
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  drift between last and 1st year:',mult_tdrift
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  drift between last and 1st year:',mult_sdrift
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  Ageostrophic bdy flow:        ',mult_ageos
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  OB North:                     ',mult_obcsn
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  OB South:                     ',mult_obcss
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  OB West:                      ',mult_obcsw
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,e10.3)')
     &'  OB East:                      ',mult_obcse
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)


      write(msgbuf,'(a)')
     &' '
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

#ifdef ALLOW_SSH_MEAN_COST_CONTRIBUTION
      if (using_topex) then
        il = ilnblnk(topexmeanfile)
        write(msgbuf,'(a,a)')
     &  '  T/P mean is read from:          ',topexmeanfile(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      endif
#endif /* ALLOW_SSH_MEAN_COST_CONTRIBUTION */

#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
      if (using_topex) then
        il = ilnblnk(topexfile)
        write(msgbuf,'(a,a)')
     &  '  T/P data are read from:         ',topexfile(1:il)
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
      endif
#endif /* ALLOW_SSH_TPANOM_COST_CONTRIBUTION */

#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
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
      endif
#endif /* ALLOW_SSH_ERSANOM_COST_CONTRIBUTION */

#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
      if (using_gfo) then
        il = ilnblnk(gfofile)
        write(msgbuf,'(a,a)')
     &  '  GFO data are read from:         ',gfofile(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i10,i8)')
     &  '  GFO start date is:              ',
     &                        gfostartdate(1),
     &                        gfostartdate(2)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,e18.11)')
     &  '  GFO sampling period is:         ',
     &                        gfoperiod
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      endif
#endif /* ALLOW_SSH_GFOANOM_COST_CONTRIBUTION */

      return
      end

