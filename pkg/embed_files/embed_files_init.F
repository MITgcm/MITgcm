C $Header: /u/gcmpack/MITgcm/pkg/embed_files/embed_files_init.F,v 1.1 2006/01/12 03:24:09 edhill Exp $
C $Name:  $
      
#include "EMBED_FILES_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: EMBED_FILES_INIT

C     !INTERFACE:
      SUBROUTINE EMBED_FILES_INIT( myThid )

C     !DESCRIPTION:
C     The functionality for the embed_files package is very simple, so
C     all this "initialization" routine needs to do is call the C
C     routine which writes whatever files have been embedded.
      
C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:

      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
      IF ( mpiMyId.EQ.0 ) THEN
#endif

        CALL embdec

#ifdef ALLOW_USE_MPI
      ENDIF
#endif

      _END_MASTER( myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
