C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/PTRACERS.h,v 1.1 2002/03/04 19:01:29 adcroft Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: PTRACERS.h
C    !INTERFACE:
C #include PTRACERS.h
 
C    !DESCRIPTION:
C Contains passive tracer fields and parameters.

C     Package flag
      logical PTRACERSisON
      COMMON /PTRACERS_PACKAGE/ PTRACERSisON

C     Number of tracers
      INTEGER PTRACERS_num
      PARAMETER(PTRACERS_num = NUMBER_OF_PTRACERS)

C     PTRACERS parameters
      _RL PTRACERS_diffKh(PTRACERS_num)
      _RL PTRACERS_diffK4(PTRACERS_num)
      _RL PTRACERS_diffKr(PTRACERS_num)
      INTEGER PTRACERS_advScheme(PTRACERS_num)
      LOGICAL PTRACERS_useGMRedi(PTRACERS_num)
      LOGICAL PTRACERS_useKPP(PTRACERS_num)
      INTEGER PTRACERS_numInUse
      LOGICAL PTRACERS_useRecords
      CHARACTER*(MAX_LEN_FNAM) PTRACERS_initialFile(PTRACERS_num)
      COMMON /PTRACERS_PARAMS/
     &                   PTRACERS_advScheme,
     &                   PTRACERS_diffKh,
     &                   PTRACERS_diffK4,
     &                   PTRACERS_diffKr,
     &                   PTRACERS_useGMRedi,
     &                   PTRACERS_useKPP,
     &                   PTRACERS_numInUse,
     &                   PTRACERS_initialFile,
     &                   PTRACERS_useRecords
      NAMELIST /PTRACERS_PARM01/
     &                   PTRACERS_advScheme,
     &                   PTRACERS_diffKh,
     &                   PTRACERS_diffK4,
     &                   PTRACERS_diffKr,
     &                   PTRACERS_useGMRedi,
     &                   PTRACERS_useKPP,
     &                   PTRACERS_numInUse,
     &                   PTRACERS_initialFile,
     &                   PTRACERS_useRecords

C     pTracer  :: passive tracer concentration (tr per unit volume).
C     gPtr     :: work-space for time-stepping
C     gPtrNM1  :: work-space for time-stepping
      _RL  pTracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
      _RL  gPtr    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
      _RL  gPtrNM1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
      COMMON /PTRACERS_FIELDS/ 
     &                   pTracer,
     &                   gPtr,
     &                   gPtrNM1

CEOP
#endif /* ALLOW_PTRACERS */
