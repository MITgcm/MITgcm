C $Header: /u/gcmpack/MITgcm/model/src/Attic/askparam.F,v 1.1 2004/10/26 19:11:57 molod Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: ASKPARAMRL

C     !INTERFACE:
      FUNCTION ASKPARAMRL(NAME)

C     !DESCRIPTION:
C     Real function askparam queries common blocks in PARAMS.h
C     Function is called with a character name to interrogate a
C     real value.
      
C     !USES:
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) NAME
CEOP

      integer I
      INTEGER MAXCON
C So far we can query no values - the routine can be taught
      PARAMETER (MAXCON=1)
      CHARACTER*16 ANAME(MAXCON)
      data ANAME(1)/'               '/
      _RL  ASKPARAMRL

      DO I=1,MAXCON
      IF(NAME.EQ.ANAME(1)) THEN
         ASKPARAMRL = 0.
         RETURN
      ENDIF
      ENDDO

900   PRINT *,' ASKPARAMRL CANNOT FIND - ',NAME
      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: ASKPARAML

C     !INTERFACE:
      FUNCTION ASKPARAML(NAME)

C     !DESCRIPTION:
C     Logical function askparam queries common blocks in PARAMS.h
C     Function is called with a character name to interrogate a
C     logical value.
      
C     !USES:
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) NAME
CEOP

      integer I
      INTEGER MAXCON
C So far we can query 1 value only - the routine can be taught more
      PARAMETER (MAXCON=1)
      CHARACTER*16 ANAME(MAXCON)
      data ANAME(1)/'useMNC         '/
      logical  ASKPARAML

      DO I=1,MAXCON
      IF(NAME.EQ.ANAME(1)) THEN
         ASKPARAML = useMNC
         RETURN
      ENDIF
      ENDDO

900   PRINT *,' ASKPARAML CANNOT FIND - ',NAME
      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: ASKPARAMI

C     !INTERFACE:
      FUNCTION ASKPARAMI(NAME)

C     !DESCRIPTION:
C     Integer function askparam queries common blocks in PARAMS.h
C     Function is called with a character name to interrogate an
C     integer value.
      
C     !USES:
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) NAME
CEOP

      integer I
      INTEGER MAXCON
C So far we can query 1 value only - the routine can be taught more
      PARAMETER (MAXCON=1)
      CHARACTER*16 ANAME(MAXCON)
      data ANAME(1)/'NITER0         '/
      integer  ASKPARAMI

      DO I=1,MAXCON
      IF(NAME.EQ.ANAME(1)) THEN
         ASKPARAMI = nIter0
         RETURN
      ENDIF
      ENDDO

900   PRINT *,' ASKPARAMI CANNOT FIND - ',NAME
      RETURN
      END
