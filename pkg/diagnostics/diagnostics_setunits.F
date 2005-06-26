C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagnostics_setunits.F,v 1.7 2005/06/26 16:51:49 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_SETUNITS

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SETUNITS (
     I     myThid,
     I     chardiag, 
     I     diagunits )

C     !DESCRIPTION:
C     Wrapper routine to set the units of an active diagnostic
      
C     !USES:
       implicit none
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      integer myThid
      character*8 chardiag
      character*16 diagunits
CEOP

C     Local variables
      integer n, ndiagnum, ipointer

C     Run through list of active diagnostics to make sure
C     we are trying to fill a valid diagnostic
      
      ndiagnum = 0
      ipointer = 0
      do n = 1,ndiagt
        if(chardiag.eq.cdiag(n)) then
          ndiagnum = n
c         ipointer = idiag(n)
        endif
      enddo
      if( ndiagnum.ne.0 .and. ipointer.ne.0 ) then
        udiag(ndiagnum)=diagunits
      else
        write(6,1000)chardiag
      endif
      
 1000 format(' ',' Warning: Trying to set units of diagnostic ',a8,
     &     ' But it is not a valid (or active) name ')

      return
      end
