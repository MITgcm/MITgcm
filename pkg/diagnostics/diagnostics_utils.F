C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_utils.F,v 1.20 2005/05/19 01:18:31 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: GETDIAG

C     !INTERFACE:
      SUBROUTINE GETDIAG(
     I                    levreal, undef,
     O                    qtmp,
     I                    ipoint, mate, bi, bj, myThid )

C     !DESCRIPTION:
C     Retrieve averaged model diagnostic

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     levreal .... Diagnostic LEVEL
C     undef  ..... UNDEFINED VALUE
C     ipoint ..... DIAGNOSTIC NUMBER FROM MENU
C     mate   ..... counter DIAGNOSTIC NUMBER if any ; 0 otherwise
C     bi     ..... X-direction tile number
C     bj     ..... Y-direction tile number
C     myThid ..... my thread Id number
      _RL levreal
      _RL undef
      INTEGER ipoint, mate
      INTEGER bi,bj, myThid

C     !OUTPUT PARAMETERS:
C     qtmp    ..... AVERAGED DIAGNOSTIC QUANTITY
      _RL qtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C     !LOCAL VARIABLES:
      _RL factor
      INTEGER i, j, ipnt,ipCt
      INTEGER lev, levCt, klev

      IF (ipoint.GE.1) THEN
       lev = NINT(levreal)
       klev = kdiag(ipoint)
       IF (lev.LE.klev) THEN

        IF ( mate.EQ.0 ) THEN
C-      No counter diagnostics => average = Sum / ndiag :

          ipnt = idiag(ipoint) + lev - 1
c         factor = 1.0
c         if (ndiag(ipoint).ne.0) factor = 1.0/ndiag(ipoint)
          factor = FLOAT(ndiag(ipoint))
          IF (ndiag(ipoint).NE.0) factor = 1. _d 0 / factor

          DO j = 1,sNy+1
            DO i = 1,sNx+1
              IF ( qdiag(i,j,ipnt,bi,bj) .LE. undef ) THEN
                qtmp(i,j) = qdiag(i,j,ipnt,bi,bj)*factor
              ELSE
                qtmp(i,j) = undef
              ENDIF
            ENDDO
          ENDDO

        ELSE
C-      With counter diagnostics => average = Sum / counter:

          ipnt = idiag(ipoint) + lev - 1
          levCt= MIN(lev,kdiag(mate))
          ipCt = idiag(mate) + levCt - 1
          DO j = 1,sNy+1
            DO i = 1,sNx+1
              IF ( qdiag(i,j,ipCt,bi,bj) .NE. 0. ) THEN
                qtmp(i,j) = qdiag(i,j,ipnt,bi,bj)
     &                    / qdiag(i,j,ipCt,bi,bj)
              ELSE
                qtmp(i,j) = undef
              ENDIF
            ENDDO
          ENDDO

        ENDIF
       ENDIF
      ENDIF

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
C     !ROUTINE: DIAGNOSTICS_COUNT
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_COUNT (chardiag, 
     I                              biArg, bjArg, myThid) 

C     !DESCRIPTION:
C***********************************************************************
C   routine to increment the diagnostic counter only
C***********************************************************************
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C***********************************************************************
C  Arguments Description
C  ----------------------
C     chardiag :: Character expression for diag to increment the counter
C     biArg    :: X-direction tile number, or 0 if called outside bi,bj loops
C     bjArg    :: Y-direction tile number, or 0 if called outside bi,bj loops
C     myThid   :: my thread Id number
C***********************************************************************
      CHARACTER*8 chardiag
      INTEGER biArg, bjArg
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C ===============
      INTEGER m, n 
      INTEGER ndiagnum, ipointer
c     INTEGER bi, bj
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C Run through list of active diagnostics to make sure
C we are trying to increment a valid diagnostic-counter

      ndiagnum = 0
      ipointer = 0
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( chardiag.EQ.flds(m,n) ) THEN
         ndiagnum = jdiag(m,n)
         IF (ndiag(ndiagnum).GE.0) ipointer = idiag(ndiagnum)
        ENDIF
       ENDDO
      ENDDO

C If-sequence to see if we are a valid and an active diagnostic

      IF ( ndiagnum.NE.0 .AND. ipointer.NE.0 ) THEN

C Increment the counter for the diagnostic (if we are at bi=bj=myThid=1)
       _BEGIN_MASTER(myThid)
        IF ( (biArg.EQ.1 .AND. bjArg.EQ.1) .OR. 
     &       (biArg.EQ.0 .AND. bjArg.EQ.0) )
     &                     ndiag(ndiagnum) = ndiag(ndiagnum) + 1
       _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- note: counter could become a tiled array, and then it would be:
c       IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
c        DO bj=myByLo(myThid), myByHi(myThid)
c         DO bi=myBxLo(myThid), myBxHi(myThid)
c          ndiag(ndiagnum,bi,bj) = ndiag(ndiagnum,bi,bj) + 1
c         ENDDO
c        ENDDO
c       ELSE
c          bi = MIN(biArg,nSx)
c          bj = MIN(bjArg,nSy)
c          ndiag(ndiagnum,bi,bj) = ndiag(ndiagnum,bi,bj) + 1
c       ENDIF

      ENDIF

      RETURN 
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGS_MK_UNITS

C     !INTERFACE:
      CHARACTER*16 FUNCTION DIAGS_MK_UNITS( 
     I                            diagUnitsInPieces, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION DIAGS_MK_UNITS
C     | o Return the diagnostic units string (16c) removing 
C     |   blanks from the input string
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     diagUnitsInPieces :: string for diagnostic units: in several 
C                          pieces, with blanks in between
C     myThid            ::  my thread Id number
      CHARACTER*(*) diagUnitsInPieces
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,n

      DIAGS_MK_UNITS = '          ' 
      n = LEN(diagUnitsInPieces)
      
      j = 0
      DO i=1,n
       IF (diagUnitsInPieces(i:i) .NE. ' ' ) THEN
         j = j+1
         IF ( j.LE.16 ) DIAGS_MK_UNITS(j:j) = diagUnitsInPieces(i:i)
       ENDIF
      ENDDO

      IF ( j.GT.16 ) THEN
         WRITE(msgBuf,'(2A,I4,A)') '**WARNING** ',
     &   'DIAGS_MK_UNITS: too long (',j,' >16) input string'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(3A)') '**WARNING** ',
     &   'DIAGS_MK_UNITS: input=', diagUnitsInPieces
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
      ENDIF

      RETURN
      END
