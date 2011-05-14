C $Header: /u/gcmpack/MITgcm/pkg/rbcs/RBCS_FIELDS.h,v 1.1 2011/05/14 19:52:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_RBCS

CBOP
C    !ROUTINE: RBCS_FIELDS.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains RBCS fields for 3-D relaxation
CEOP

C---  RBCS 3-D Fields:

#ifndef DISABLE_RBCS_MOM
      _RS RBC_maskU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS RBC_maskV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCuVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCvVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /RBCS_MASKS_UV/
     &          RBC_maskU,
     &          RBC_maskV
      COMMON /RBCS_FIELDS_UV/
     &          RBCuVel,
     &          RBCvVel
#endif
      _RS RBC_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,maskLEN)
      _RL RBCtemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /RBCS_MASKS_TR/
     &          RBC_mask
      COMMON /RBCS_FIELDS_TS/
     &          RBCtemp,
     &          RBCsalt

#ifdef ALLOW_PTRACERS
      _RL RBC_ptracers(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                PTRACERS_num)
      COMMON /RBCS_PTR_FIELDS/
     &          RBC_ptracers
#endif /* ALLOW_PTRACERS */

C     rbcsLdRec     :: time-record currently loaded (in temp arrays *[1])
      COMMON /RBCS_LOAD_I/ rbcsLdRec
      INTEGER rbcsLdRec(nSx,nSy)
#ifndef DISABLE_RBCS_MOM
      COMMON /RBCS_LOADED_UV/
     &                 rbcu0, rbcv0,
     &                 rbcu1, rbcv1
      _RS  rbcu0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcu1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcv0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcv1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      COMMON /RBCS_LOADED_TS/
     &                 rbct0, rbcs0,
     &                 rbct1, rbcs1
      _RS  rbct0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbct1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcs0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcs1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_PTRACERS
       COMMON /RBCS_LOADED_PTR/
     &               rbcptr0, rbcptr1
       _RS rbcptr0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
       _RS rbcptr1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_RBCS */
