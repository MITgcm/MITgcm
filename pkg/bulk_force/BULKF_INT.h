C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/BULKF_INT.h,v 1.2 2003/11/23 01:36:55 jmc Exp $
C $Name:  $

#ifdef ALLOW_BULK_FORCE
c     !ROUTINE: BULKF_INT.h

C     Intermediate variables for bulk forcing
      COMMON /BULKF_FFIELDS_INTERMEDIATE/
     &       evap, savssq, fsh, flh, fswnet, flwup, flwupnet,
     &       ustress, vstress
      _RL     evap(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     savssq(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     fsh(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     flh(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     fswnet(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     flwup(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     flwupnet(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     ustress(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
      _RL     vstress(1-olx:snx+olx,1-oly:sny+oly,nSx,nSy)
#endif /* ALLOW_BULK_FORCE */
