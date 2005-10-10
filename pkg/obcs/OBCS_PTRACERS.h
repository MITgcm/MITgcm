C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_PTRACERS.h,v 1.1 2005/10/10 05:53:48 mlosch Exp $
C $Name:  $

#ifdef ALLOW_OBCS

C-- Fields and files for OBCS-support for passive tracers package PTRACERS

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_OBCS_NORTH
      COMMON /GRID_N_PTR_OB/ OBNptr
      _RL OBNptr (1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_N_PTR_OB_AUX/ OBNptr0, OBNptr1
      _RL OBNptr0(1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
      _RL OBNptr1(1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
      COMMON /GRID_S_PTR_OB/ OBSptr
      _RL OBSptr (1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_S_PTR_OB_AUX/ OBSptr0, OBSptr1
      _RL OBSptr0(1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
      _RL OBSptr1(1-Olx:sNx+Olx,Nr,nSx,nSy,PTRACERS_num)
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_SOUTH */
#ifdef ALLOW_OBCS_EAST
      COMMON /GRID_E_PTR_OB/ OBEptr
      _RL OBEptr (1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_E_PTR_OB_AUX/ OBEptr0, OBEptr1
      _RL OBEptr0(1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL OBEptr1(1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
      COMMON /GRID_W_PTR_OB/ OBWptr
      _RL OBWptr (1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
#ifdef ALLOW_OBCS_PRESCRIBE
      COMMON /GRID_W_PTR_OB_AUX/ OBWptr0, OBWptr1
      _RL OBWptr0(1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL OBWptr1(1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
#endif /* ALLOW_OBCS_PRESCRIBE */
#endif /* ALLOW_OBCS_WEST */

      COMMON /OB_PTR_FILES/
     &     OBNptrFile,OBSptrFile,OBEptrFile,OBWptrFile
      CHARACTER*(MAX_LEN_FNAM)
     &     OBNptrFile(PTRACERS_num),
     &     OBSptrFile(PTRACERS_num),
     &     OBEptrFile(PTRACERS_num),
     &     OBWptrFile(PTRACERS_num)
#endif /* ALLOW_PTRACERS */


#endif /* ALLOW_OBCS */
