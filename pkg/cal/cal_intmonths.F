C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_intmonths.F,v 1.2 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      integer function cal_IntMonths(
     I                                mythid
     &                              )

c     ==================================================================
c     FUNCTION cal_IntMonths
c     ==================================================================
c
c     o Return the number of calendar months that are affected by the 
c       current model integration.
c
c              
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     FUNCTION cal_IntMonths
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer endmonth
      integer startmonth
      integer endyear
      integer startyear

c     == end of interface ==

      startmonth = mod(modelstartdate(1)/100,100)
      endmonth   = mod(  modelenddate(1)/100,100)
      startyear  = modelstartdate(1)/10000
      endyear    = modelenddate(1)/10000

ce    --> allow for previous iterations: niter0 !
ce        in this case one would need some more information about the 
ce        integration of the model, e.g. the global start and end dates.

      if ( startyear .ne. endyear ) then
        cal_IntMonths = (nmonthyear - startmonth + 1) +
     &               nmonthyear*(endyear - startyear - 1) +
     &               endmonth
      else
        cal_IntMonths = endmonth - startmonth + 1
      endif

      if ( ( modelenddate(2) .eq. 0) .and.
     &     ( mod(modelenddate(1),100) .eq. 1) ) then
        cal_IntMonths = cal_IntMonths - 1
      endif

      return
      end

