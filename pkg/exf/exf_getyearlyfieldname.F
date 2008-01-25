C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getyearlyfieldname.F,v 1.1 2008/01/25 16:02:56 mlosch Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
      subroutine exf_GetYearlyFieldName(
     I     useYearlyFields, twoDigitYear, genperiod, year, genfile,
     O     genfileout,
     I     mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_GetYearlyFieldName
c     ==================================================================
c
c     o Determine actual name of forcing file including year extension
c
c     started: Martin Losch  24-Jan-2008
c
c     ==================================================================

      implicit none

c     == global variables ==

c     == routine arguments ==

c     input
      _RL     genperiod
      logical useYearlyFields
      logical twoDigitYear
      character*(128) genfile
      integer year
      _RL     mytime
      integer myiter
      integer mythid
c     output
      character*(128) genfileout

#ifdef ALLOW_EXF
c     == local variables ==

      integer yearLoc
      integer il

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if (useYearlyFields.and.genperiod.gt.0) then
c     Complete filename with YR or _YEAR extension
       il = ilnblnk( genfile )
       if (twoDigitYear) then
        yearLoc = year-1900
        if (year.ge.2000) yearLoc = year-2000
        write(genfileout(1:128),'( a,i2.2)') genfile(1:il),yearLoc
       else
        write(genfileout(1:128),'(2a,i4.4)') genfile(1:il),'_',year
       endif
      else
       genfileout = genfile
      endif
#endif /* ALLOW_EXF */      
      return
      end
