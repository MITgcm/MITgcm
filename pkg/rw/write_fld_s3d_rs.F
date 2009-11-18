C $Header: /u/gcmpack/MITgcm/pkg/rw/write_fld_s3d_rs.F,v 1.1 2009/11/18 00:33:58 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

CBOP
C     !ROUTINE: WRITE_FLD_S3D_RS
C     !INTERFACE:
      SUBROUTINE WRITE_FLD_S3D_RS(
     I                 pref, suff, Ovl, nNz, field, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE WRITE_FLD_S3D_RS
C     |  Front-end interface to low-level I/O subroutine (MDSIO).
C     |  Write short (smaller overlap) 3-D "RS" type field
C     |   to binary file (prefix,suffix).
C     *==========================================================*
C     | Note: Use a local copy to full overlap array
C     |  - not very efficient
C     |  - max number of level is limited (set to kSiz)
C     |  But since it is used mainly for debugging purpose,
C     |   no attempt to improve efficiency/flexibility
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid    :: my Thread Id number
      CHARACTER*(*) pref,suff
      INTEGER Ovl
      INTEGER nNz
      _RS field(1-Ovl:sNx+Ovl,1-Ovl:sNy+Ovl,nNz,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

#ifndef RW_DISABLE_SMALL_OVERLAP
C     !FUNCTIONS:
      INTEGER  ILNBLNK, IFNBLNK
      EXTERNAL ILNBLNK, IFNBLNK

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      INTEGER kSiz
      PARAMETER ( kSiz = Nr )
      _RS locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSiz,nSx,nSy)

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fName
      INTEGER fPrec, iRec
      INTEGER i,j,k,bi,bj
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( Ovl.GT.OLx .OR. Ovl.GT.OLy ) THEN
        WRITE(msgBuf,'(2A,2(I3,A))') 'WRITE_FLD_S3D_RS:',
     &  ' Argument Ovl (=', Ovl, ' ) too large (>', MIN(OLx,OLy), ' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R WRITE_FLD_S3D_RS'
      ENDIF
      IF ( nNz.GT.kSiz ) THEN
        WRITE(msgBuf,'(2A,2(I3,A))') 'WRITE_FLD_S3D_RS:',
     &  ' Argument nNz (=', nNz, ' ) too large (> kSiz=', kSiz, ' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R WRITE_FLD_S3D_RS'
      ENDIF

      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fName, '(A)' ) pref(s1Lo:s1Hi)
      ELSEIF ( suff .EQ. 'I10' ) THEN
       WRITE( fName, '(A,A,I10.10)' ) pref(s1Lo:s1Hi),'.',myIter
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,nNz
          DO j=1,sNy
           DO i=1,sNx
            locVar(i,j,k,bi,bj) = field(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      fPrec = writeBinaryPrec
      iRec  = 1
      CALL WRITE_REC_LEV_RS(
     I                       fName, fPrec, kSiz, 1, nNz, locVar,
     I                       iRec, myIter, myThid )

#else  /* RW_DISABLE_SMALL_OVERLAP */

      STOP 'ABNORMAL END: S/R WRITE_FLD_S3D_RS empty'

#endif /* RW_DISABLE_SMALL_OVERLAP */

      RETURN
      END
