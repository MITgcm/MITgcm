      subroutine getdiag (lev,ipoint,bi,bj,undef,qtmp)
C***********************************************************************        
C  PURPOSE                                                                      
C     Retrieve averaged model diagnostic
C  INPUT:                                                                       
C     lev ..... Diagnostic LEVEL
C  ipoint ..... DIAGNOSTIC NUMBER FROM MENU                                     
C   undef ..... UNDEFINED VALUE                                                 
C      bi ..... X-direction process(or) number
C      bj ..... Y-direction process(or) number
C                                                                               
C  OUTPUT:                                                                      
C    qtmp ..... AVERAGED DIAGNOSTIC QUANTITY
C                                                                               
C***********************************************************************        
      implicit none

#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer bi,bj
      integer lev,ipoint
      integer i,j,ipnt,klev
      _RL undef, factor
      _RL qtmp(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nsx,Nsy)

      do j = 1,sNy
      do i = 1,sNx
       qtmp(i,j,bi,bj) = undef
      enddo
      enddo

      IF (IPOINT.LT.1) GO TO 999

      KLEV = KDIAG(IPOINT)
      IF(KLEV.GE.LEV) THEN
      IPNT = IDIAG(IPOINT) + LEV - 1
                                FACTOR = 1.0
      IF( NDIAG(IPOINT).NE.0 )  FACTOR = 1.0   / NDIAG(IPOINT)
      do j = 1,sNy
      do i = 1,sNx
      if( qdiag(i,j,ipnt,bi,bj).ne.undef ) 
     .     qtmp(i,j,bi,bj) = qdiag(i,j,ipnt,bi,bj)*factor
      enddo
      enddo
      ENDIF

 999  RETURN
      END

      subroutine getdiag2 (lev,ipoint,bi,bj,undef,qtmp)
C***********************************************************************        
C                                                                               
C  PURPOSE                                                                      
C     Retrieve model diagnostic (No Averaging)
C  INPUT:                                                                       
C     lev ..... Model LEVEL                                                     
C  ipoint ..... DIAGNOSTIC NUMBER FROM MENU                                     
C   undef ..... UNDEFINED VALUE                                                 
C      im ..... X-DIMENSION
C      jm ..... Y-DIMENSION
C      nd ..... Number of 2-D Diagnostics
C                                                                               
C  OUTPUT:                                                                      
C    qtmp ..... DIAGNOSTIC QUANTITY                                             
C                                                                               
C***********************************************************************        
      implicit none

#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer bi,bj

      integer lev,ipoint
      integer i,j,ipnt,klev
      _RL undef
      _RL qtmp(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nsx,Nsy)

      do j = 1,sNy
      do i = 1,sNx
       qtmp(i,j,bi,bj) = undef
      enddo
      enddo

      IF (IPOINT.LT.1) GO TO 999

      KLEV = KDIAG(IPOINT)
      IF(KLEV.GE.LEV) THEN
      IPNT = IDIAG(IPOINT) + LEV - 1
      do j = 1,sNy
      do i = 1,sNx
       qtmp(i,j,bi,bj) = qdiag(i,j,ipnt,bi,bj)
      enddo
      enddo
      ENDIF

 999  RETURN
      END
                                                                                
      subroutine clrindx (myThid,listnum)
C***********************************************************************
C
C  PURPOSE
C     DRIVER TO CLEAR DIAGNOSTICS SPECIFIED IN DIAGNOSTIC INDEX LIST
C
C  ARGUMENT DESCRIPTION
C     listnum ....  diagnostics list number
C
C***********************************************************************

      implicit none
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid, listnum

      integer m, n
      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer mate

      equivalence (     parms1 , parse1(1) )
      equivalence ( mate_index , parse1(6) )

      do n=1,nfields(listnum)
       do m=1,ndiagt
        if( flds(n,listnum).eq.cdiag(m) .and. idiag(m).ne.0 ) then
         call clrdiag (myThid,m)

c Check for Counter Diagnostic
c ----------------------------
         parms1 =  gdiag(m)
         if( parse1(5).eq.'C' ) then
          read (mate_index,100) mate
          call clrdiag (myThid,mate)
         endif
        endif
       enddo
      enddo
                   
  100 format(i3)
      RETURN       
      END          


      subroutine clrdiag (myThid,index)
C***********************************************************************        
C  PURPOSE                                                                      
C     ZERO OUT MODEL DIAGNOSTIC ARRAY ELEMENTS
C***********************************************************************        
                                                                                
      implicit none
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid, index

      integer bi,bj
      integer i,j,k

C **********************************************************************        
C ****              SET DIAGNOSTIC AND COUNTER TO ZERO              ****        
C **********************************************************************        
                                                                                
      do bj=myByLo(myThid), myByHi(myThid)
      do bi=myBxLo(myThid), myBxHi(myThid)
       do k = 1,kdiag(index)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,idiag(index)+k-1,bi,bj) = 0.0
        enddo
        enddo
       enddo
      enddo
      enddo

      ndiag(index) = 0

      return
      end

      subroutine setdiag (myThid,num,ndiagmx)
C***********************************************************************
C
C  PURPOSE
C     SET POINTER LOCATIONS, NAMES, LEVELS and TITLES FOR DIAGNOSTIC NUM
C
C***********************************************************************

      implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer num,myThid,ndiagmx
      integer ipointer

      DATA IPOINTER / 1 /

      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer     mate

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
          if(myThid.eq.0) WRITE(6,2000)KDIAG(NUM),NUM,CDIAG(NUM),ndiagmx
        endif
      ELSE
          if(myThid.eq.0) WRITE(6,3000) NUM, CDIAG(NUM)
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
        if(myThid.eq.0)WRITE(6,2000)KDIAG(mate),mate,CDIAG(mate),ndiagmx
       endif
      ELSE
          if(myThid.eq.0) WRITE(6,3000) mate, CDIAG(mate)
      ENDIF
      endif

      RETURN

  100 format(i3)
 2000 FORMAT(1X,'Allocating ',I2,' Level(s) for Diagnostic # ',I3,
     .          ' (',A8,'),  Total Number of Diagnostics: ',I5)
 3000 FORMAT(1X,'Diagnostic # ',I3,' (',A8,') has already been set')
 4000 FORMAT(1X,'Unable to allocate space for Diagnostic # ',I3,
     .                      ' (',A8,')')
 5000 FORMAT(1X,'Unable to allocate space for Counter Diagnostic # ',
     .    I3,' (',A8,')',' WARNING - Diag will not accumulate properly')
      END
