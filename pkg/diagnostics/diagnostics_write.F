      subroutine diagnostics_write ( nymd,nhms,nymd0,nhms0,xlabel,
     .                     vars,grid,earth,ocean,land,physics,coupling,
     .                     diag,plist,nplist,dlist,ndlist,nrtype )

C***********************************************************************
C  Purpose
C  -------
C     Output sequence for the (multiple) diagnostics output files
C
C  Arguments  Description
C  ----------------------
C     nymd ..... Current   YYMMDD
C     nhms ..... Current   HHMMSS
C     nymd0 .... Beginning YYMMDD
C     nhms0 .... Beginning HHMMSS
C     xlabel ... Character*80 Description of Experiment
C     jobid .... Character*8  Job Identifier
C     vars ..... Dynamics State Data Structure
C     grid ..... Dynamics Grid  Data Structure
C     physics .. Physics  State Data Structure
C     land ..... Land State     Data Structure
C     earth .... Earth Model    Data Structure
C     ocean .... Ocean Model    Data Structure
C     coupling . Coupling       Data Structure
C     diag ..... Diagnostics    Data Structure
C      plist ... Prognostic History List Data Structure
C     nplist ... Number of distinct Prognostic History Lists
C      dlist ... Diagnostic History List Data Structure
C     ndlist ... Number of distinct Diagnostic History Lists
C     nrtype ... Output Record Type:  0   After  Analysis or Straight Forecast
C                                    -1   Before Analysis
C                                     1   Assimilation (w/IAU)
C
C***********************************************************************
c Diagnostics Common
c -----------------
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer   nrtype
      integer   ndlist
      integer   nplist
      integer   ndindx (ndiagt)

      character*80 xlabel
      character* 8 jobid
      integer      nymd,nhms,nymd0,nhms0

c Local variables
c ===============
      integer   nd
      integer   ndsum
      integer   n,rc
      integer   nymdt,nhmst


c HHMMSS event timestep counter
c =============================
      integer  alarm, nalarm,  mhms
      alarm( mhms ) = nalarm ( mhms, nymd, nhms, nymd0, 0     )

C***********************************************************************
C*                      Check for Prognostic Output                    *
C***********************************************************************
      do n=1,nplist
      if( alarm(plist(n)%frequency).eq.0 ) then
       call progsig (nymd,nhms,nymd0,nhms0,xlabel,jobid,nrtype,plist(n),
     .                   vars,grid,earth,ocean,land,physics,coupling )
      endif
      enddo
C***********************************************************************
C*                      Check for Diagnostic Output                    *
C***********************************************************************
      do n=1,ndlist
      call chkdiag(dlist(n),nymd,nhms,nymd0,nhms0,ndindx,ndsum,nymdt,
     .                                  nhmst)
      if (ndsum.ne.0) then
      call diagout( nymdt,nhmst,nymd0,nhms0,xlabel,ndindx,nrtype,
     .     dlist(n),qdiag)
      endif
      enddo

c Clear Outputed Diagnostics
c --------------------------
      do n=1,ndlist
      call chkdiag(n,nymd,nhms,nymd0,nhms0,ndindx,ndsum,nymdt,nhmst)
      if (ndsum.ne.0) then
      call clrindx ( diag,ndindx )
      endif
      enddo

      return
      end
      subroutine chkdiag(nl,nymd,nhms,nymd0,nhms0,ndindx,ndsum,nymdt,nhmst)
      implicit none

#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer nymd,nhms,nymd0,nhms0,nymdt,nhmst,ndsum
      integer ndindx(ndiagt)

      integer nymdd,nhmsd,ntick,nafreq,n,m,ndum
      integer nsecf,nalarm

      nymdt = nymd
      nhmst = nhms
      nymdd = nymd0
      nhmsd = nhms0

c Initialize Diagnostic Index
c ---------------------------
      do n=1,ndiagt
      ndindx(n) = 0
      enddo

c Fill Current Diagnostic Index
c -----------------------------
      ndsum  = 0
      nafreq = frequency(nl)
      if( nalarm( nafreq,nymd,nhms,nymdd,nhmsd ).eq.0 ) then

      do n=1,nfield(nl)
         do m=1,ndiagt
         if( fields(nl,n).eq.cdiag_(m) .and. idiag(m).ne.0 ) then
         ndsum = ndsum  + 1
         ndindx (ndsum) = m
         endif
         enddo
      enddo

      endif
      return
      end

      function chklist (name,list)
      implicit none
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"
      logical  chklist
      character*8 name
      integer n
      chklist = .false.
      do n=1,nfield(nl)
      if( fields(nl,n).eq.name ) chklist = .true.
      enddo
      return
      end
