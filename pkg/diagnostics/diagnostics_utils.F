C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_utils.F,v 1.18 2005/02/07 03:07:49 jmc Exp $
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
