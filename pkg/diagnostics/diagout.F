      subroutine  diagout (myThid,listnum)
C***********************************************************************
C
C  Purpose
C     Routine to write Output for Diagnostic Fields
C
C  Argument Description
C     myThid ... Process(or) number
C     listnum .. Diagnostics list number being written
C
C***********************************************************************        

      implicit none
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "SIZE.h"

#ifdef(ALLOW_FIZHI)then
#include "fizhi_SIZE.h"
#else
       integer Nrphys
       parameter (Nrphys=1)
#endif

#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer m, n
      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer mate
      _RL qtmp1(sNx,sNy,Nrphys)
      _RL qtmp2(sNx,sNy,Nrphys)
                                                                                
      equivalence (     parms1 , parse1(1) )
      equivalence ( mate_index , parse1(6) )


      do n=1,nfields(listnum)
       do m=1,ndiagt
        if( flds(n,listnum).eq.cdiag(m) .and. idiag(m).ne.0 ) then

         parms1 =  gdiag(m)
         if(ndiag(m).ne.0.and.idiag(m).ne.0.and.parse1(5).ne.'D')then
          if( myThid.eq.0 ) write(6,2000) n,cdiag(n),ndiag(n),gdiag(n)
           if(parse1(5).ne.'C') then
            do k=1,kdiag(n)
             call getdiag ( k,n,bi,bj,undef,qtmp1(1,1,k))
            enddo

c Check for Vector Diagnostic and Mate
c ------------------------------------
      if( parse1(1).eq.'U' .or. parse1(1).eq.'V' ) then
      read (mate_index,100) mate
  100 format(i3)
                                                                                   
        if( idiag(mate).ne.0 ) then
          if(first) then
          if( myThid.eq.0 ) write(6,2001) cdiag(n),mate,cdiag(mate)
 2001     format(1x,'Computing Vector  Mate for  ',a8,5x,
     .                              'Diagnostic # ',i3,2x,a8)
          endif
          do k=1,kdiag(mate)
          CALL GETDIAG ( k,mate,bi,bj,undef,qtmp1(1,1,k))
          enddo
          parms2 =  gdiag(mate)
        else
          if(first) then
          if( myThid.eq.0 ) write(6,2002) cdiag(n),mate,cdiag(mate)
 2002     format(1x,'        Vector  Mate for  ',a8,5x,
     .                   'Diagnostic # ',i3,2x,a8,' not enabled')
          endif
        endif
                                                                                   
      endif

      ELSE
                                                                                   
      read (mate_index,100) mate
      do k=1,kdiag(n)
       call getdiag2 ( k,n,bi,bj,undef,qtmp1(1,1,k))
       call getdiag2 ( k,mate,bi,bj,undef,qtmp1(1,1,k))
       do j = 1,jm
        do i = 1,im
         if(qtmp2(i,j,k).ne.0.) then
          qtmp1(i,j,k) = qtmp1(i,j,k) / qtmp2(i,j,k)
         else
          qtmp1(i,j,k) = undef
         endif
        enddo
       enddo
      enddo
                                                                                   
      ENDIF


       call mdswritefield(

      enddo

 2000 format(1x,'Computing Diagnostic # ',i3,2x,a8,5x,'Counter: ',
     .                                         i3,6x,'Parms: ',a16)
      RETURN                                                                    
      END                                                                       
