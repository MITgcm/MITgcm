C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init_rec.F,v 1.1 2013/01/26 14:45:56 heimbach Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine ctrl_init_rec(
     I     fldname,
     I     fldstartdate1, fldstartdate2, fldperiod, nfac,
     O     fldstartdate, diffrec, startrec, endrec,
     I     mythid )

c     ==================================================================
c     SUBROUTINE ctrl_init_rec
c     ==================================================================
c
c     helper routine to compute the first and last record of a
c     time dependent control variable
c
c     Martin.Losch@awi.de, 2011-Mar-15
c
c     ==================================================================
c     SUBROUTINE ctrl_init_rec
c     ==================================================================

      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif

c     == input variables ==
c     fldstartdate1/2 : start time (date/time) of fld
c     fldperod        : sampling interval of fld
c     nfac            : factor for the case that fld is an obcs variable
c                       in this case nfac = 4, otherwise nfac = 1
c     mythid          : thread ID of this instance
      character*(*) fldname
      integer fldstartdate1
      integer fldstartdate2
      _RL     fldperiod
      integer nfac
      integer mythid

c     == output variables ==
c     fldstartdate : full date from fldstartdate1 and 2
c     startrec     : first record of ctrl variable
c     startrec     : last record of ctrl variable
c     diffrec      : difference between first and last record of ctrl variable
      integer fldstartdate(4)
      integer startrec
      integer endrec
      integer diffrec

c     == local variables ==
      integer i
#ifdef ALLOW_CAL
      integer difftime(4)
      _RL     diffsecs
#endif /* ALLOW_CAL */
      character*(max_len_mbuf) msgbuf
      integer il

c     == functions ==
      integer  ilnblnk
      external ilnblnk

      if ( debugLevel .GE. debLevB ) then
       il=ilnblnk(fldname)
       WRITE( msgBuf,'(A,A)')
     &     'CTRL_INIT_REC: Getting record indices for ',fldname(1:il)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid )
      endif

c     initialise some output
      do i = 1,4
       fldstartdate(i) = 0
      end do
      startrec = 0
      endrec   = 0
      diffrec  = 0
      if ( fldperiod .EQ. -12. ) then
       startrec = 1
       endrec   = 12*nfac
      elseif ( fldperiod .EQ. 0. ) then
       startrec = 1
       endrec   = 1*nfac
      else
# ifdef ALLOW_CAL
       call cal_FullDate( fldstartdate1, fldstartdate2,
     &                        fldstartdate , mythid )
       call cal_TimePassed( fldstartdate, modelstartdate,
     &                           difftime, mythid )
       call cal_ToSeconds ( difftime, diffsecs, mythid )
       startrec = int((modelstart + startTime - diffsecs)/
     &                fldperiod) + 1
       endrec   = int((modelend + startTime - diffsecs + modelstep/2)/
     &                fldperiod) + 2
       if ( nfac .ne. 1 ) then
c     This is the case of obcs.
        startrec = (startrec - 1)*nfac + 1
        endrec   = endrec*nfac
       endif
# else /* ndef ALLOW_CAL */
       startrec = 1
       endrec   = (int((endTime - startTime)/fldperiod) + 1)*nfac
#endif /* ALLOW_CAL */
      endif
      diffrec  = endrec - startrec + 1

      if ( debugLevel .GE. debLevB ) then
       WRITE( msgBuf,'(A,A,A)')
     &      'CTRL_INIT_REC: Record indices for ',fldname(1:il),':'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid )
       WRITE( msgBuf,'(A,I10,A,I10)')
     &      'CTRL_INIT_REC: startrec = ',startrec,', endrec = ',endrec
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid )
      endif

      return
      end

