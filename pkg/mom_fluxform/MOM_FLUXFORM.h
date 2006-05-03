
#ifdef NONLIN_FRSURF
# ifndef DISABLE_RSTAR_CODE
C     == Local variables in common block ==
C     dWtransC :: vertical transp. difference between r & r* coordinates
C     dWtransU :: same but above u.point location (West  face)
C     dWtransV :: same but above v.point location (South face)
      COMMON /LOCAL_MOM_CALC_RTRANS/ 
     &       dWtransC, dWtransU, dWtransV
      _RL dWtransC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)  
      _RL dWtransU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)  
      _RL dWtransV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)  
# endif
#endif /* NONLIN_FRSURF */

