C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagnostics_initialise.F,v 1.9 2004/07/07 03:10:15 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INITIALISE

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INITIALISE(myThid)

C     !DESCRIPTION:
C     Set Pointer Locations for Diagnostics in diag_list.
      
C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer ndiagcount
      integer m,mm,n
      logical found
      integer i,j,bi,bj

C     Calculate pointers for diagnostics set to non-zero frequency
      call diagnostics_init_vals (myThid)
      
      ndiagcount = 0
      do n=1,nlists
        do m=1,nfields(n)
          
          found = .false.
C         Search all possible model diagnostics
          do mm=1,ndiagt
            if( flds(m,n).eq.cdiag(mm) ) then
              call setdiag (myThid,mm,ndiagcount)
              found = .true.
            endif
          enddo
          if( .not.found .and. myThid.eq.1 ) then
            print *, 'Note:  ',flds(m,n),' is not a Diagnostic.'
          endif
        enddo
      enddo
      
C     Zero out the qdiag array which accumulates during integration
      do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
          do n = 1,numdiags
            do j = 1,sNy
              do i = 1,sNx
                qdiag(i,j,n,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      return
      end
