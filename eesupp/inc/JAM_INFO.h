C $Header: /u/gcmpack/MITgcm/eesupp/inc/Attic/JAM_INFO.h,v 1.3 2001/09/21 03:54:36 cnh Exp $
C $Name:  $
CBOP
C      !ROUTINE: JAM_INFO.h
C      !INTERFACE:
C      include "JAM_INFO.h"
C      !DESCRIPTION:
C      Global parameters used with JAM.
CEOP

       COMMON /JAM_INFO/
     &  jam_pid,     jam_np, jam_exchKey,
     &  jam_northId, jam_southId
       INTEGER jam_pid
       INTEGER jam_np
       INTEGER jam_exchKey
       INTEGER jam_northId
       INTEGER jam_southId
