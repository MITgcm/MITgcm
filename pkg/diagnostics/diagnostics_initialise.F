      subroutine diagnostics_initialise (myThid)
C***********************************************************************
C
C  PURPOSE
C     Set Pointer Locations for Diagnostics in diag_list
C
C  INPUT:
C   nlist ..... Number of  Diagnostic Lists
C      nd ..... Total Number of Diagnostics
C
C***********************************************************************
      implicit none

#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid
      integer ndiagcount

C **********************************************************************
C ****        Check for Diagnostic Consistency among Lists          ****
C **********************************************************************

      do n=1,nlist
      do m=1,nfield(n)

      do nn=n+1,nlist
      do mm=1,nfield(nn)
      if( fields(n,m) .eq. fields(nn,mm) ) then
      if( freq(n,m) .ne. freq(nn,mm) then
          if( myThid.eq.0 ) then
          print *
          print *, 'The Diagnostic: ',fields(n,m),' is requested ',
     .             'at two distinct frequencies: ',freq(n),
     .             ' and ',freq(nn),' !'
          print *
          endif
          STOP
      endif
      endif
      enddo
      enddo

      enddo
      enddo

C **********************************************************************
C **** Calculate pointers for diagnostics set to non-zero frequency ****
C **********************************************************************

      call diagnostics_init_vals (myThid,nr,nrphys)

      ndiagcount = 0
      do n=1,nlist
      do m=1,nfield(n)

         found = .false.
         do mm=1,ndiagt   ! Search all possible model diagnostics
            if( fields(n,m).eq.cdiag(mm) ) then
            call setdiag (myThid,mm,ndiagcount,numdiags)
            found = .true.
            endif
         enddo
         if( .not.found .and. myThid.eq.0 ) then
         print *, 'Note:  ',fields(n,m),' is not a Diagnostic.'
         endif

      enddo
      enddo

      return
      end
      subroutine setdiag (myThid,num,ndiagmx,numdiags)
C***********************************************************************
C
C  PURPOSE
C     SET POINTER LOCATIONS, NAMES, LEVELS and TITLES FOR DIAGNOSTIC NUM
C
C***********************************************************************

      implicit none
#include "diagnostics.h"

      integer num,myThid,numdiags,ndiagmx
      integer ipointer

      DATA IPOINTER / 1 /

      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer     num2,mate

      equivalence (     parms1 , parse1(1) )
      equivalence ( mate_index , parse1(6) )

C **********************************************************************
C ****                SET POINTERS FOR DIAGNOSTIC NUM               ****
C **********************************************************************

      parms1 = gdiag(num)

      IF( IDIAG(NUM).EQ.0 ) THEN
        if(ndiagmx+kdiag(num).gt.numdiags) then
          write(6,4000)num,cdiag(num)
        else
          IDIAG(NUM) = IPOINTER
          IPOINTER   = IPOINTER + KDIAG(NUM)
          ndiagmx    = ndiagmx  + KDIAG(NUM)
          if(myid.eq.0) WRITE(6,2000) KDIAG(NUM),NUM,CDIAG(NUM),ndiagmx
        endif
      ELSE
          if(myid.eq.0) WRITE(6,3000) NUM, CDIAG(NUM)
      ENDIF

c Check for Counter Diagnostic
c ----------------------------
      if( parse1(5).eq.'C') then
      read (mate_index,100) mate

      IF( IDIAG(mate).EQ.0 ) THEN
        if(ndiagmx+kdiag(num).gt.numdiags) then
          write(6,5000)num,cdiag(num)
        else
          IDIAG(mate) = IPOINTER
          IPOINTER    = IPOINTER + KDIAG(mate)
          ndiagmx     = ndiagmx  + KDIAG(mate)
          if(myid.eq.0)WRITE(6,2000)KDIAG(mate),mate,CDIAG(mate),ndiagmx
        endif
      ELSE
          if(myid.eq.0) WRITE(6,3000) mate, CDIAG(mate)
      ENDIF
      endif

      RETURN

  100 format(i3)
 2000 FORMAT(1X,'Allocating ',I2,' Level(s) for Diagnostic # ',I3,
     .          ' (',A8,'),  Total Number of Diagnostics: ',I5)
 3000 FORMAT(1X,'Diagnostic # ',I3,' (',A8,') has already been set')
 4000 FORMAT(1X,'Unable to allocate space for Diagnostic # ',I3,
     .                      ' (',A8,')')
 4000 FORMAT(1X,'Unable to allocate space for Counter Diagnostic # ',
     .    I3,' (',A8,')',' WARNING - Diag will not accumulate properly')
      END
