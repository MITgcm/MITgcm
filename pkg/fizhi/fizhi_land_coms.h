c Land State Common
c -----------------
      common /land_state/ tcanopy, tdeep, ecanopy, 
     . swetshal, swetroot, swetdeep, snodep, capac
      _RL tcanopy(nchp,Nsx,Nsy)
      _RL tdeep(nchp,Nsx,Nsy)
      _RL ecanopy(nchp,Nsx,Nsy)
      _RL swetshal(nchp,Nsx,Nsy)
      _RL swetroot(nchp,Nsx,Nsy)
      _RL swetdeep(nchp,Nsx,Nsy)
      _RL snodep(nchp,Nsx,Nsy)
      _RL capac(nchp,Nsx,Nsy)

c Land Mapping Common
c -------------------
      common /land_mapping/ chlt, chlon, igrd
      _RL chlt(nchp,Nsx,Nsy)
      _RL chlon(nchp,Nsx,Nsy)
      integer igrd(nchp,Nsx,Nsy)
