      subroutine  diagout (myThid,myIter,listnum)
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

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
       integer Nrphys
       parameter (Nrphys=1)
#endif

#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid, myIter, listnum

      integer i, j, k, m, n, bi, bj
      character*8 parms1
      character*1 parse1(8)
      character*3 mate_index
      integer mate
      _RL qtmp1(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr+Nrphys,Nsx,Nsy)
      _RL qtmp2(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr+Nrphys,Nsx,Nsy)
      _RL undef, getcon

      equivalence (     parms1 , parse1(1) )
      equivalence ( mate_index , parse1(6) )

      CHARACTER*(MAX_LEN_FNAM) pref
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(80) fn
      logical glf

      undef = getcon('UNDEF')
      glf = .FALSE.
      WRITE(suff,'(I10.10)') myIter
      pref = fnames(listnum)
      WRITE( fn, '(A,A)' ) pref(1:8),'.',suff(1:10)

      do n=1,nfields(listnum)
       do m=1,ndiagt
        if( flds(n,listnum).eq.cdiag(m) .and. idiag(m).ne.0 ) then
         parms1 =  gdiag(m)
         if(ndiag(m).ne.0.and.parse1(5).ne.'D')then
          if( myThid.eq.1 ) write(6,2000) m,cdiag(m),ndiag(m),gdiag(m)
          if(parse1(5).ne.'C') then
           do k=1,kdiag(m)
            call getdiag ( myThid,k,m,undef,qtmp1)
           enddo

c Check for Mate of a Vector Diagnostic
c -------------------------------------
           if( parse1(1).eq.'U' .or. parse1(1).eq.'V' ) then
            read (mate_index,100) mate
            if( idiag(mate).ne.0 ) then
             if( myThid.eq.1 ) write(6,2001) cdiag(m),mate,cdiag(mate)
            else
             if( myThid.eq.1 ) write(6,2002) cdiag(m),mate,cdiag(mate)
            endif
           endif

          else

c Check for Mate of a Counter Diagnostic
c --------------------------------------
           read (mate_index,100) mate
           do k=1,kdiag(m)
            call getdiag2 ( myThid,k,m,undef,qtmp1)
            call getdiag2 ( myThid,k,mate,undef,qtmp2)
            do bj=myByLo(myThid), myByHi(myThid)
            do bi=myBxLo(myThid), myBxHi(myThid)
            do j = 1,sNy
             do i = 1,sNx
              if(qtmp2(i,j,k,bi,bj).ne.0.) then
               qtmp1(i,j,k,bi,bj) = 
     .                       qtmp1(i,j,k,bi,bj) / qtmp2(i,j,k,bi,bj)
              else
               qtmp1(i,j,k,bi,bj) = undef
              endif
             enddo
            enddo
            enddo
            enddo
           enddo

          endif
         endif
        endif
       call mdswritefield(fn,64,glf,'RL',kdiag(m),qtmp1,n,myIter,myThid)
       enddo
      enddo

  100 format(i3)
 2000 format(1x,'Computing Diagnostic # ',i3,2x,a8,5x,'Counter: ',
     .                                         i3,6x,'Parms: ',a16)
 2001     format(1x,'          Vector  Mate for  ',a8,5x,
     .                              'Diagnostic # ',i3,2x,a8,' exists ')
 2002     format(1x,'        Vector  Mate for  ',a8,5x,
     .                   'Diagnostic # ',i3,2x,a8,' not enabled')

      return                                                                    
      end                                                                       
