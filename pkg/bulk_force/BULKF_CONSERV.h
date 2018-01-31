C     /==========================================================\
C     | BULKF_CONSERV.h                                          |
C     | o Header for Bulk formula conservation variables         |
C     \==========================================================/

#ifdef ALLOW_BULK_FORCE

#ifdef CONSERV_BULKF

C     Storage arrays for area-time-totals
      _RL CONS_Qnet    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CONS_EmPmR   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qnet_off, empmr_off, constim, conservcycle

      COMMON /BULKF_CONS/
     &                       CONS_Qnet, CONS_EmPmR, 
     &                       qnet_off, empmr_off, constim,
     &                       conservcycle


#endif /* CONSERV_BULKF */

#endif /* ALLOW_BULK_FORCE */
