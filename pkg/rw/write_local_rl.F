C $Header: /u/gcmpack/MITgcm/pkg/rw/write_local_rl.F,v 1.8 2009/09/01 19:28:24 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

CBOP
C     !ROUTINE: WRITE_LOCAL_RL
C     !INTERFACE:
      SUBROUTINE WRITE_LOCAL_RL(
     I                 pref, suff, nNr, field,
     I                 bi, bj, iRec, myIter, myThArg )

C     !DESCRIPTION:
C     Write "RL" type local-tile array "field" corresponding to tile bi,bj
C     to binary file (prefix,suffix) at record position "iRec".

C     !USES:
      IMPLICIT NONE
C     Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     pref    :: file name prefix
C     suff    :: file name suffix
C     nNr     :: Number of levels to write
C     field   :: field array to write
C     bi,bj   :: tile indices
C     iRec    :: record number in output file
C     myIter  :: Iteration number
C     myThArg :: thread argument (= my Thread Id or = 0 to simply
C                     write 1 tile without thread synchronisation)
      CHARACTER*(*) pref,suff
      INTEGER nNr
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nNr)
      INTEGER bi,bj
      INTEGER iRec
      INTEGER myIter
      INTEGER myThArg

C     !FUNCTIONS
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

C     Common block
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
      LOGICAL useCurrentDir
      _RS dummyRS(1)
      CHARACTER*(2) fType
      CHARACTER*(MAX_LEN_FNAM) fullName
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      INTEGER myThid

C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSEIF ( suff .EQ. 'I10' ) THEN
       WRITE( fullName, '(A,A,I10.10)' ) pref(s1Lo:s1Hi),'.',myIter
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      useCurrentDir = .TRUE.
      fType='RL'
#ifdef ALLOW_MDSIO
      IF (nSx.EQ.1.AND.nSy.EQ.1) THEN
C The hack below replaces MDS_WRITELOCAL with MDS_WRITE_FIELD for
C single-threaded execution because the former does not support the
C singleCpuIo option. This is a placeholder until MDS_WRITELOCAL
C functionality is superseded by pkg/diagnostics.
         myThid = MAX(myThArg,1)
         CALL MDS_WRITE_FIELD(
     I                        fullName, writeBinaryPrec,
     I                        globalFile, useCurrentDir,
     I                        fType, nNr, 1, nNr, field, dummyRS,
     I                        iRec, myIter, myThid )
      ELSE
         CALL MDS_WRITELOCAL(
     I                        fullName, writeBinaryPrec, globalFile,
     I                        fType, nNr, field, dummyRS,
     I                        bi, bj, iRec, myIter, myThArg )
      ENDIF
#endif

      RETURN
      END
