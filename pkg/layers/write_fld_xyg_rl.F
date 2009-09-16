C $Header: /u/gcmpack/MITgcm/pkg/layers/Attic/write_fld_xyg_rl.F,v 1.1 2009/09/16 21:25:47 rpa Exp $
C $Name:  $

#include "RW_OPTIONS.h"

      SUBROUTINE WRITE_FLD_XYG_RL( pref,suff,field,myIter,myThid )

C     This routine is based on WRITE_FLD_XYZ_RL from the rw package.
C     It is for writing a 3D MDS file with G (temp/density/etc.) as the vertical coordinate

      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"

C Arguments
      CHARACTER*(*) pref,suff
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      LOGICAL useCurrentDir
      CHARACTER*(2) fType
      _RS dummyRS(1)
      INTEGER nNz
      INTEGER iRec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(MAX_LEN_FNAM) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      useCurrentDir = .FALSE.
      fType='RL'
      nNz=Nlayers
      iRec=1
#ifdef ALLOW_MDSIO
      CALL MDS_WRITE_FIELD(
     I                      fullName, writeBinaryPrec,
     I                      globalFile, useCurrentDir,
     I                      fType, nNz, 1, nNz, field, dummyRS,
     I                      iRec, myIter, myThid )
#endif
      RETURN
      END
