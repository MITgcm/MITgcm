C $Header: /u/gcmpack/MITgcm/verification/tutorial_advection_in_gyre/code/Attic/PTRACERS_FIELDS_SOM.h,v 1.1 2008/01/28 20:09:08 jahn Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: PTRACERS_FIELDS_SOM.h
C    !INTERFACE:
C #include PTRACERS_FIELDS_SOM.h

C    !DESCRIPTION:
C Contains SOM array for passive tracer fields

CEOP

#ifdef GAD_ALLOW_SOM_ADVECT
C--   COMMON /PTRACERS_FIELD_SOM/ REAL, state variables for 2nd Order-Moment Advection
C     moments are stored in this order (3-D case): 1rst Order moments (3:x,y,z)
C                                   and 2nd Order moments (6:xx,yy,zz,xy,xz,yz).
C     som_Ptr  :: PTracer 1rst & 2nd Order Moments
      COMMON /PTRACERS_FIELDS_SOM/
     &                som_Ptr
      _RL  som_Ptr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,nSOM,
     &             PTRACERS_num)
#endif /* GAD_ALLOW_SOM_ADVECT */

#endif /* ALLOW_PTRACERS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
