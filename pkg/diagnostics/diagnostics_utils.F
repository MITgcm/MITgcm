C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_utils.F,v 1.16 2004/12/20 01:53:54 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: GETDIAG

C     !INTERFACE:
      SUBROUTINE GETDIAG (levreal,ipoint,undef,qtmp,myThid)

C     !DESCRIPTION:
C     Retrieve averaged model diagnostic

C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
CEOP

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
      integer Nrphys
      parameter (Nrphys=0)
#endif

C     INPUT:
C     levreal .... Diagnostic LEVEL
C     ipoint ..... DIAGNOSTIC NUMBER FROM MENU
C     undef  ..... UNDEFINED VALUE
C     bi     ..... X-direction process(or) number
C     bj     ..... Y-direction process(or) number
      _RL levreal
      integer myThid,ipoint
      _RL undef

C     OUTPUT:
C     qtmp    ..... AVERAGED DIAGNOSTIC QUANTITY
      _RL qtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)

      _RL factor
      integer i,j,ipnt,klev
      integer bi,bj
      integer lev

      if (ipoint.ge.1) then
       lev = NINT(levreal)

       klev = kdiag(ipoint)
       if (klev.ge.lev) then
        ipnt = idiag(ipoint) + lev - 1
        factor = 1.0
        if (ndiag(ipoint).ne.0) factor = 1.0/ndiag(ipoint)

        do bj=myByLo(myThid), myByHi(myThid)
          do bi=myBxLo(myThid), myBxHi(myThid)

            do j = 1,sNy
              do i = 1,sNx
                if ( qdiag(i,j,ipnt,bi,bj) .le. undef ) then
                  qtmp(i,j,lev,bi,bj) = qdiag(i,j,ipnt,bi,bj)*factor
                else
                  qtmp(i,j,lev,bi,bj) = undef
                endif
              enddo
            enddo

          enddo
        enddo

       endif
      endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: GETDIAG2

C     !INTERFACE:
      SUBROUTINE GETDIAG2 (levreal,ipoint,undef,qtmp,myThid)

C     !DESCRIPTION:
C***********************************************************************
C  PURPOSE
C     Retrieve averaged model diagnostic
C  INPUT:
C  levreal .... Diagnostic LEVEL
C  ipoint ..... DIAGNOSTIC NUMBER FROM MENU
C   undef ..... UNDEFINED VALUE
C
C  OUTPUT:
C    qtmp ..... AVERAGED DIAGNOSTIC QUANTITY
C
C***********************************************************************

C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
CEOP

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
       integer Nrphys
       parameter (Nrphys=0)
#endif

      _RL levreal
      integer myThid,ipoint
      _RL undef
      _RL qtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)

      integer i,j,ipnt,klev
      integer bi,bj
      integer lev

      if (ipoint.ge.1) then
       lev = NINT(levreal)

       klev = kdiag(ipoint)
       if (klev.ge.lev) then
        ipnt = idiag(ipoint) + lev - 1

        do bj=myByLo(myThid), myByHi(myThid)
          do bi=myBxLo(myThid), myBxHi(myThid)

            do j = 1,sNy
              do i = 1,sNx
                if ( qdiag(i,j,ipnt,bi,bj) .le. undef ) then
                  qtmp(i,j,lev,bi,bj) = qdiag(i,j,ipnt,bi,bj)
                else
                  qtmp(i,j,lev,bi,bj) = undef
                endif
              enddo
            enddo

          enddo
        enddo

       endif
      endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine clrindx (listnum, myThid)
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
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

      integer myThid, listnum

      integer m, n
      character*8 parms1
      character*3 mate_index
      integer mate

      do n=1,nfields(listnum)
       do m=1,ndiagt
        if( flds(n,listnum).eq.cdiag(m) .and. idiag(m).ne.0 ) then
         call clrdiag (m, myThid)

c Check for Counter Diagnostic
c ----------------------------
         parms1 =  gdiag(m)(1:8)
         if ( parms1(5:5).eq.'C' ) then
          mate_index = parms1(6:8)
          read (mate_index,'(I3)') mate
          call clrdiag (mate, myThid)
         endif
        endif
       enddo
      enddo

      RETURN
      END


      subroutine clrdiag (index, myThid)
C***********************************************************************
C  PURPOSE
C     ZERO OUT MODEL DIAGNOSTIC ARRAY ELEMENTS
C***********************************************************************

      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

      integer myThid, index

      integer bi,bj
      integer i,j,k

C **********************************************************************
C ****              SET DIAGNOSTIC AND COUNTER TO ZERO              ****
C **********************************************************************

      do bj=myByLo(myThid), myByHi(myThid)
       do bi=myBxLo(myThid), myBxHi(myThid)
        do k = 1,kdiag(index)
         do j = 1-OLy,sNy+OLy
          do i = 1-OLx,sNx+OLx
           qdiag(i,j,idiag(index)+k-1,bi,bj) = 0.0
          enddo
         enddo
        enddo
       enddo
      enddo

      ndiag(index) = 0

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_IS_ON

C     !INTERFACE:
      LOGICAL FUNCTION DIAGNOSTICS_IS_ON( diagName, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION DIAGNOSTIC_IS_ON
C     | o Return TRUE if diagnostics "diagName" is Active
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     diagName   ::  diagnostic identificator name (8 characters long)
C     myThid     ::  my thread Id number
      CHARACTER*8  diagName
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER j,n,m

      DIAGNOSTICS_IS_ON = .FALSE.
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( diagName.EQ.flds(m,n) ) THEN
          j = jdiag(m,n)     
          IF ( idiag(j).NE.0 .AND. ndiag(j).GE.0 ) 
     &         DIAGNOSTICS_IS_ON = .TRUE.
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
