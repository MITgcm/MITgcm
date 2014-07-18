C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_utils.F,v 1.33 2014/07/18 22:04:10 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C--   File diagnostics_utils.F: General purpose support routines
C--    Contents:
C--    o DIAGNOSTICS_COUNT
C--    o DIAGNOSTICS_GET_DIAG
C--    o DIAGNOSTICS_GET_POINTERS
C--    o DIAGNOSTICS_SETKLEV
C--    o DIAGS_GET_PARMS_I (Function)
C--    o DIAGS_MK_UNITS (Function)
C--    o DIAGS_MK_TITLE (Function)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_COUNT
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_COUNT( diagName,
     I                              biArg, bjArg, myThid )

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
C     diagName :: name of diagnostic to increment the counter
C     biArg    :: X-direction tile number, or 0 if called outside bi,bj loops
C     bjArg    :: Y-direction tile number, or 0 if called outside bi,bj loops
C     myThid   :: my thread Id number
C***********************************************************************
      CHARACTER*8 diagName
      INTEGER biArg, bjArg
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C ===============
      INTEGER m, n
      INTEGER bi, bj
      INTEGER ipt, ndId
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
        bi = myBxLo(myThid)
        bj = myByLo(myThid)
      ELSE
        bi = MIN(biArg,nSx)
        bj = MIN(bjArg,nSy)
      ENDIF

C--   Run through list of active diagnostics to find which counter
C     to increment (needs to be a valid & active diagnostic-counter)
      DO n=1,nLists
       DO m=1,nActive(n)
        IF ( diagName.EQ.flds(m,n) .AND. idiag(m,n).GT.0 ) THEN
         ipt = idiag(m,n)
         IF (ndiag(ipt,bi,bj).GE.0) THEN
          ndId = jdiag(m,n)
          ipt = ipt + pdiag(n,bi,bj)*kdiag(ndId)
C-    Increment the counter for the diagnostic
          IF ( biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
           DO bj=myByLo(myThid), myByHi(myThid)
            DO bi=myBxLo(myThid), myBxHi(myThid)
             ndiag(ipt,bi,bj) = ndiag(ipt,bi,bj) + 1
            ENDDO
           ENDDO
          ELSE
             ndiag(ipt,bi,bj) = ndiag(ipt,bi,bj) + 1
          ENDIF
C-    Increment is done
         ENDIF
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_GET_DIAG

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_GET_DIAG(
     I                    kl, undefRL,
     O                    qtmp,
     I                    ndId, mate, ip, im, bi, bj, myThid )

C     !DESCRIPTION:
C     Retrieve time-averaged (or snap-shot) diagnostic field

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     kl      :: level selection: >0 : single selected lev ; =0 : all kdiag levels
C     undefRL :: undefined "_RL" type value
C     ndId    :: diagnostic Id number (in available diagnostics list)
C     mate    :: counter diagnostic number if any ; 0 otherwise
C     ip      :: pointer to storage array location for diag.
C     im      :: pointer to storage array location for mate
C     bi      :: X-direction tile number
C     bj      :: Y-direction tile number
C     myThid  :: my thread Id number
      INTEGER kl
      _RL undefRL
      INTEGER ndId, mate, ip, im
      INTEGER bi, bj, myThid

C     !OUTPUT PARAMETERS:
C     qtmp    :: time-averaged (or snap-shot) diagnostic field
      _RL qtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,*)
CEOP

C     !LOCAL VARIABLES:
      _RL factor
      INTEGER i, j, ipnt, ipCt
      INTEGER k, kd, km, kLev

      IF (ndId.GE.1) THEN
       kLev = kdiag(ndId)
       IF ( kl.GE.1 .AND. kl.LE.kLev ) THEN
        kLev = 1
       ELSEIF ( kl.NE.0 ) THEN
        kLev = 0
       ENDIF

       DO k = 1,kLev
        kd = k
        IF ( kl.GE.1 ) kd = kl

        IF ( mate.EQ.0 ) THEN
C-      No counter diagnostics => average = Sum / ndiag :

          ipnt = ip + kd - 1
          factor = FLOAT(ndiag(ip,bi,bj))
          IF (ndiag(ip,bi,bj).NE.0) factor = 1. _d 0 / factor

#ifdef ALLOW_FIZHI
          DO j = 1,sNy+1
            DO i = 1,sNx+1
              IF ( qdiag(i,j,ipnt,bi,bj) .LE. undefRL ) THEN
                qtmp(i,j,k) = qdiag(i,j,ipnt,bi,bj)*factor
              ELSE
                qtmp(i,j,k) = undefRL
              ENDIF
            ENDDO
          ENDDO
#else /* ALLOW_FIZHI */
          DO j = 1,sNy+1
            DO i = 1,sNx+1
              qtmp(i,j,k) = qdiag(i,j,ipnt,bi,bj)*factor
            ENDDO
          ENDDO
#endif /* ALLOW_FIZHI */

        ELSE
C-      With counter diagnostics => average = Sum / counter:

          ipnt = ip + kd - 1
          km = MIN(kd,kdiag(mate))
          ipCt = im + km - 1
          DO j = 1,sNy+1
            DO i = 1,sNx+1
              IF ( qdiag(i,j,ipCt,bi,bj) .NE. 0. ) THEN
                qtmp(i,j,k) = qdiag(i,j,ipnt,bi,bj)
     &                      / qdiag(i,j,ipCt,bi,bj)
              ELSE
                qtmp(i,j,k) = undefRL
              ENDIF
            ENDDO
          ENDDO

        ENDIF
       ENDDO
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_GET_POINTERS
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_GET_POINTERS(
     I                       diagName, listId,
     O                       ndId, ip,
     I                       myThid )

C     !DESCRIPTION:
C     *================================================================*
C     | o Returns the diagnostic Id number and diagnostic
C     |   pointer to storage array for a specified diagnostic.
C     *================================================================*
C     | Note: A diagnostics field can be stored multiple times
C     |       (for different output frequency,phase, ...).
C     | operates in 2 ways:
C     | o listId =0 => find 1 diagnostics Id & pointer which name matches.
C     | o listId >0 => find the unique diagnostic Id & pointer with
C     |      the right name and same output time as "listId" output-list
C     | o return ip=0 if did not find the right diagnostic;
C     |   (ndId <>0 if diagnostic exist but output time does not match)
C     *================================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     diagName :: diagnostic identificator name (8 characters long)
C     listId   :: list number that specify the output frequency
C     myThid   :: my Thread Id number
C     !OUTPUT PARAMETERS:
C     ndId     :: diagnostics  Id number (in available diagnostics list)
C     ip       :: diagnostics  pointer to storage array

      CHARACTER*8 diagName
      INTEGER listId
      INTEGER ndId, ip
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER n,m

      ip   = 0
      ndId = 0

      IF ( listId.LE.0 ) THEN
C--   select the 1rst one which name matches:

C-    search for this diag. in the active 2D/3D diagnostics list
        DO n=1,nLists
         DO m=1,nActive(n)
           IF ( ip.EQ.0 .AND. diagName.EQ.flds(m,n)
     &                  .AND. idiag(m,n).NE.0 ) THEN
            ip   = ABS(idiag(m,n))
            ndId = jdiag(m,n)
           ENDIF
         ENDDO
        ENDDO

      ELSEIF ( listId.LE.nLists ) THEN
C--   select the unique diagnostic with output-time identical to listId

C-    search for this diag. in the active 2D/3D diagnostics list
        DO n=1,nLists
         IF ( ip.EQ.0
     &        .AND. freq(n) .EQ. freq(listId)
     &        .AND. phase(n).EQ.phase(listId)
     &        .AND. averageFreq(n) .EQ.averageFreq(listId)
     &        .AND. averagePhase(n).EQ.averagePhase(listId)
     &        .AND. averageCycle(n).EQ.averageCycle(listId)
     &      ) THEN
          DO m=1,nActive(n)
           IF ( ip.EQ.0 .AND. diagName.EQ.flds(m,n)
     &                  .AND. idiag(m,n).NE.0 ) THEN
            ip   = ABS(idiag(m,n))
            ndId = jdiag(m,n)
           ENDIF
          ENDDO
         ELSEIF ( ip.EQ.0 ) THEN
          DO m=1,nActive(n)
           IF ( ip.EQ.0 .AND. diagName.EQ.flds(m,n)
     &                  .AND. idiag(m,n).NE.0 ) THEN
            ndId = jdiag(m,n)
           ENDIF
          ENDDO
         ENDIF
        ENDDO

      ELSE
        STOP 'DIAGNOSTICS_GET_POINTERS: invalid listId number'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_SETKLEV

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SETKLEV(
     I                                diagName, nLevDiag, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R DIAGNOSTICS_SETKLEV
C     | o Define explicitly the number of level (stored in kdiag)
C     |   of a diagnostic field. For most diagnostics, the number
C     |   of levels is derived (in S/R SET_LEVELS) from gdiag(10)
C     |   but occasionally one may want to set it explicitly.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     diagName  :: diagnostic identificator name (8 characters long)
C     nLevDiag  :: number of level to set for this diagnostics field
C     myThid    :: my Thread Id number
      CHARACTER*8  diagName
      INTEGER nLevDiag
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER n, ndId

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER( myThid)

C--   Check if this S/R is called from the right place ;
C     needs to be after DIAGNOSTICS_INIT_EARLY and before DIAGNOSTICS_INIT_FIXED
      IF ( diag_pkgStatus.NE.ready2setDiags ) THEN
        CALL DIAGNOSTICS_STATUS_ERROR( 'DIAGNOSTICS_SETKLEV',
     &                   ' ', diagName, ready2setDiags, myThid )
      ENDIF

C--   Find this diagnostics in the list of available diag.
      ndId = 0
      DO n = 1,ndiagt
        IF ( diagName.EQ.cdiag(n) ) THEN
          ndId = n
        ENDIF
      ENDDO
      IF ( ndId.EQ.0 ) THEN
        WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_SETKLEV: ',
     &     'diagName="', diagName, '" not known.'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_SETKLEV'
      ENDIF

C-    Optional level number diagnostics (X): set number of levels
      IF ( kdiag(ndId).EQ.0
     &   .AND. gdiag(ndId)(10:10).EQ.'X' ) THEN
        kdiag(ndId) = nLevDiag
      ELSEIF ( kdiag(ndId).EQ.nLevDiag
     &   .AND. gdiag(ndId)(10:10).EQ.'X' ) THEN
C-    level number already set to same value: send warning
        WRITE(msgBuf,'(4A,I5)') '** WARNING ** DIAGNOSTICS_SETKLEV: ',
     &     'diagName="', diagName, '" , nLevDiag=', nLevDiag
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(2A,I5,A)')'** WARNING ** DIAGNOSTICS_SETKLEV:',
     &     ' level Nb (=', kdiag(ndId), ') already set.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ELSEIF ( gdiag(ndId)(10:10).EQ.'X' ) THEN
C-    level number already set to a different value: do not reset but stop
        WRITE(msgBuf,'(4A,I5)') 'DIAGNOSTICS_SETKLEV: ',
     &     'diagName="', diagName, '" , nLevDiag=', nLevDiag
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I5,3A)') 'DIAGNOSTICS_SETKLEV: ',
     &     'level Nb already set to', kdiag(ndId), ' => STOP'
        CALL PRINT_ERROR( msgBuf, myThid )
      ELSE
C-    for now, do nothing but just send a warning
        WRITE(msgBuf,'(4A,I5)') '** WARNING ** DIAGNOSTICS_SETKLEV: ',
     &     'diagName="', diagName, '" , nLevDiag=', nLevDiag
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(2A,I5,3A)') '** WARNING ** will set level Nb',
     &     ' from diagCode(ndId=', ndId, ')="', gdiag(ndId)(1:10), '"'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(4A)') '** WARNING ** DIAGNOSTICS_SETKLEV',
     &     '("', diagName, '") <== Ignore this call.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

      _END_MASTER( myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGS_GET_PARMS_I

C     !INTERFACE:
      INTEGER FUNCTION DIAGS_GET_PARMS_I(
     I                            parName, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION DIAGS_GET_PARMS_I
C     | o Return the value of integer parameter
C     |   from one of the DIAGNOSTICS.h common blocs
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     parName   :: string used to identify which parameter to get
C     myThid    :: my Thread Id number
      CHARACTER*(*) parName
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER n

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      n = LEN(parName)
c     write(0,'(3A,I4)')
c    &  'DIAGS_GET_PARMS_I: parName="',parName,'" , length=',n

      IF ( parName.EQ.'LAST_DIAG_ID' ) THEN
         DIAGS_GET_PARMS_I = ndiagt
      ELSE
         WRITE(msgBuf,'(4A)') 'DIAGS_GET_PARMS_I: ',
     &    ' parName="', parName, '" not known.'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R DIAGS_GET_PARMS_I'
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
      INTEGER i,j,n,nbc

      DIAGS_MK_UNITS = '                '
      n = LEN(diagUnitsInPieces)

      j = 0
      DO i=1,n
       IF (diagUnitsInPieces(i:i) .NE. ' ' ) THEN
         j = j+1
         IF ( j.LE.16 ) DIAGS_MK_UNITS(j:j) = diagUnitsInPieces(i:i)
       ENDIF
      ENDDO
      nbc = j

      IF ( nbc.GT.16 ) THEN
C-    try to reduce length by changing m^2 & m^3 to m2 & m3:
       DIAGS_MK_UNITS = '                '
       j = 0
       DO i=1,n
        IF ( diagUnitsInPieces(i:i) .NE. ' ' ) THEN
         IF ( j.GE.1 .AND. nbc.GT.16 .AND.
     &         diagUnitsInPieces(i:i).EQ.'^' ) THEN
          IF ( diagUnitsInPieces(i-1:i-1).EQ.'m' ) THEN
            nbc = nbc - 1
          ELSE
           j = j+1
           IF ( j.LE.16 ) DIAGS_MK_UNITS(j:j) = diagUnitsInPieces(i:i)
          ENDIF
         ELSE
          j = j+1
          IF ( j.LE.16 ) DIAGS_MK_UNITS(j:j) = diagUnitsInPieces(i:i)
         ENDIF
        ENDIF
       ENDDO
      ENDIF

      IF ( j.GT.16 ) THEN
         WRITE(msgBuf,'(2A,I4,A)') '** WARNING ** ',
     &   'DIAGS_MK_UNITS: too long (',j,' >16) input string'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(3A)') '** WARNING ** ',
     &   'DIAGS_MK_UNITS: input=', diagUnitsInPieces
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGS_MK_TITLE

C     !INTERFACE:
      CHARACTER*80 FUNCTION DIAGS_MK_TITLE(
     I                            diagTitleInPieces, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION DIAGS_MK_TITLE
C     | o Return the diagnostic title string (80c) removing
C     |   consecutive blanks from the input string
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     diagTitleInPieces :: string for diagnostic units: in several
C                          pieces, with blanks in between
C     myThid            ::  my Thread Id number
      CHARACTER*(*) diagTitleInPieces
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL flag
      INTEGER i,j,n

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DIAGS_MK_TITLE = '                                        '
     &               //'                                        '
      n = LEN(diagTitleInPieces)

      j = 0
      flag = .FALSE.
      DO i=1,n
       IF (diagTitleInPieces(i:i) .NE. ' ' ) THEN
         IF ( flag ) THEN
           j = j+1
           IF (j.LE.80) DIAGS_MK_TITLE(j:j) = ' '
         ENDIF
         j = j+1
         IF ( j.LE.80 ) DIAGS_MK_TITLE(j:j) = diagTitleInPieces(i:i)
         flag = .FALSE.
       ELSE
         flag = j.GE.1
       ENDIF
      ENDDO

      IF ( j.GT.80 ) THEN
         WRITE(msgBuf,'(2A,I4,A)') '** WARNING ** ',
     &   'DIAGS_MK_TITLE: too long (',j,' >80) input string'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(3A)') '** WARNING ** ',
     &   'DIAGS_MK_TITLE: input=', diagTitleInPieces
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
      ENDIF

      RETURN
      END
