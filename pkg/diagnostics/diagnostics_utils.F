      subroutine getdiag (qdiag,lev,ipoint,qtmp,im,jm,nd,undef)
C***********************************************************************        
C                                                                               
C  PURPOSE                                                                      
C     Retrieve averaged model diagnostic
C  INPUT:                                                                       
C     lev ..... Model LEVEL                                                     
C  ipoint ..... DIAGNOSTIC NUMBER FROM MENU                                     
C   undef ..... UNDEFINED VALUE                                                 
C      im ..... X-DIMENSION
C      jm ..... Y-DIMENSION
C      nd ..... Number of 2-D Diagnostics
C                                                                               
C  OUTPUT:                                                                      
C    qtmp ..... AVERAGED DIAGNOSTIC QUANTITY                                             
C                                                                               
C***********************************************************************        
      implicit none

#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer    im,jm,nd
      real qdiag(im,jm,nd)

      integer lev,ipoint
      integer i,j,ipnt,klev
      real    undef, factor
      real    qtmp(im,jm)

      do j = 1,jm
      do i = 1,im
      qtmp(i,j) = undef
      enddo
      enddo

      IF (IPOINT.LT.1) GO TO 999

      KLEV = KDIAG(IPOINT)
      IF(KLEV.GE.LEV) THEN
      IPNT = IDIAG(IPOINT) + LEV - 1
                                FACTOR = 1.0
      IF( NDIAG(IPOINT).NE.0 )  FACTOR = 1.0   / NDIAG(IPOINT)
      do j = 1,jm
      do i = 1,im
      if( qdiag(i,j,ipnt).ne.undef ) qtmp(i,j) = qdiag(i,j,ipnt)*factor
      enddo
      enddo
      ENDIF

 999  RETURN
      END

      subroutine getdiag2 (qdiag,lev,ipoint,qtmp,im,jm,nd,undef)
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

#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer    im,jm,nd
      real qdiag(im,jm,nd)

      integer lev,ipoint
      integer i,j,ipnt,klev
      real    undef, factor
      real    qtmp(im,jm)

      do j = 1,jm
      do i = 1,im
      qtmp(i,j) = undef
      enddo
      enddo

      IF (IPOINT.LT.1) GO TO 999

      KLEV = KDIAG(IPOINT)
      IF(KLEV.GE.LEV) THEN
      IPNT = IDIAG(IPOINT) + LEV - 1
      do j = 1,jm
      do i = 1,im
      qtmp(i,j) = qdiag(i,j,ipnt)
      enddo
      enddo
      ENDIF

 999  RETURN
      END
      subroutine  clrindx ( diag,indxlist )
C***********************************************************************
C
C  PURPOSE
C     DRIVER TO CLEAR DIAGNOSTICS SPECIFIED IN DIAGNOSTIC INDEX LIST
C
C  ARGUMENT DESCRIPTION
C     INDXLIST.. INTEGER DIAGNOSTIC INDEX LIST
C
C***********************************************************************

      implicit none

#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer indxlist (ndiagt)
      integer index, n
 
      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer     mate

      equivalence (     parms1 , parse1(1) )
      equivalence ( mate_index , parse1(6) )

      DO  INDEX=1,NDIAGT
      N = INDXLIST (index)

      IF( N.NE.0 .AND. IDIAG(N).NE.0 ) THEN
      call clrdiag (diag,n)

c Check for Counter Diagnostic
c ----------------------------
      parms1 =  gdiag(n)
      if( parse1(5).eq.'C' ) then
       read (mate_index,100) mate
       call clrdiag (diag,mate)
      endif

      ENDIF        
      ENDDO
                   
  100 format(i3)
      RETURN       
      END          


      subroutine clrdiag (diag,n)
C***********************************************************************        
C                                                                               
C  PURPOSE                                                                      
C     INITIALIZE MODEL DIAGNOSTIC QUANTITIES                                    
C                                                                               
C***********************************************************************        
                                                                                
      implicit none
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer n
      integer i,j,k

C **********************************************************************        
C ****              SET DIAGNOSTIC AND COUNTER TO ZERO              ****        
C **********************************************************************        
                                                                                
      IF( IDIAG(N).NE.0 ) THEN                                                  

        do k=1,kdiag(n)
        do j=1,sNx
        do i=1,sNy
        qdiag(i,j,idiag(n)+k-1) = 0.0
        enddo
        enddo
        enddo

      NDIAG(N) = 0                                                           
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
