C $Header: /u/gcmpack/MITgcm/pkg/fizhi/Attic/land_coms.h,v 1.2 2004/05/05 00:39:21 edhill Exp $
C $Name:  $

c Land State Common
c -----------------
      common /land_state/ tcanopy, tdeep, ecanopy, 
     . swetshal, swetroot, swetdeep, snodep, capac
      _RL tcanopy(nchp), tdeep(nchp), ecanopy(nchp), swetshal(nchp)
      _RL swetroot(nchp), swetdeep(nchp), snodep(nchp), capac(nchp)

c Land Mapping Common
c -------------------
      common /land_mapping/ chlt, chlon, igrd
      _RL chlt(nchp), chlon(nchp)
      integer igrd(nchp) 
