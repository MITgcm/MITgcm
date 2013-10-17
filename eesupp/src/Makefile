# $Header: /u/gcmpack/MITgcm/eesupp/src/Makefile,v 1.19 2013/10/17 00:27:53 jmc Exp $
# $Name:  $

# Makefile for generate eesupp source code from templates.
# Why do we do this?  Answer: saves typing!

RL_FILES = exch_xy_rl.F exch_3d_rl.F exch_xyz_rl.F exch_s3d_rl.F \
           exch_uv_xy_rl.F exch_uv_3d_rl.F exch_uv_xyz_rl.F \
           exch_z_3d_rl.F exch_sm_3d_rl.F \
           exch_uv_agrid_3d_rl.F exch_uv_bgrid_3d_rl.F exch_uv_dgrid_3d_rl.F \
           exch0_rl.F exch1_rl.F \
           exch1_rl_cube.F exch1_z_rl_cube.F exch1_uv_rl_cube.F exch1_bg_rl_cube.F \
           exch1_rl_ad.F exch1_rl_cube_ad.F \
           exch_rl_recv_get_x.F exch_rl_recv_get_y.F \
           exch_rl_send_put_x.F exch_rl_send_put_y.F

RS_FILES = exch_xy_rs.F exch_3d_rs.F exch_xyz_rs.F exch_s3d_rs.F \
           exch_uv_xy_rs.F exch_uv_3d_rs.F exch_uv_xyz_rs.F \
           exch_z_3d_rs.F exch_sm_3d_rs.F \
           exch_uv_agrid_3d_rs.F exch_uv_bgrid_3d_rs.F exch_uv_dgrid_3d_rs.F \
           exch0_rs.F exch1_rs.F \
           exch1_rs_cube.F exch1_z_rs_cube.F exch1_uv_rs_cube.F exch1_bg_rs_cube.F \
           exch1_rs_ad.F exch1_rs_cube_ad.F \
           exch_rs_recv_get_x.F exch_rs_recv_get_y.F \
           exch_rs_send_put_x.F exch_rs_send_put_y.F

R4_FILES = exch_xy_r4.F exch_3d_r4.F exch_xyz_r4.F exch_s3d_r4.F \
           exch_uv_xy_r4.F exch_uv_3d_r4.F exch_uv_xyz_r4.F \
           exch_z_3d_r4.F exch_sm_3d_r4.F \
           exch_uv_agrid_3d_r4.F exch_uv_bgrid_3d_r4.F exch_uv_dgrid_3d_r4.F \
           exch0_r4.F exch1_r4.F \
           exch1_r4_cube.F exch1_z_r4_cube.F exch1_uv_r4_cube.F exch1_bg_r4_cube.F \
           exch_r4_recv_get_x.F exch_r4_recv_get_y.F \
           exch_r4_send_put_x.F exch_r4_send_put_y.F \
           gather_2d_r4.F scatter_2d_r4.F \
           gather_vec_r4.F scatter_vec_r4.F \
           gather_2d_wh_r4.F scatter_2d_wh_r4.F

R8_FILES = exch_xy_r8.F exch_3d_r8.F exch_xyz_r8.F exch_s3d_r8.F \
           exch_uv_xy_r8.F exch_uv_3d_r8.F exch_uv_xyz_r8.F \
           exch_z_3d_r8.F exch_sm_3d_r8.F \
           exch_uv_agrid_3d_r8.F exch_uv_bgrid_3d_r8.F exch_uv_dgrid_3d_r8.F \
           exch0_r8.F exch1_r8.F \
           exch1_r8_cube.F exch1_z_r8_cube.F exch1_uv_r8_cube.F exch1_bg_r8_cube.F \
           exch_r8_recv_get_x.F exch_r8_recv_get_y.F \
           exch_r8_send_put_x.F exch_r8_send_put_y.F \
           gather_2d_r8.F scatter_2d_r8.F \
           gather_vec_r8.F scatter_vec_r8.F \
           gather_2d_wh_r8.F scatter_2d_wh_r8.F

OLD_FILES = exch_z_xy_rl.F exch_z_xy_rs.F exch_z_xy_r4.F exch_z_xy_r8.F \
 exch_z_xyz_rl.F exch_z_xyz_rs.F exch_z_xyz_r4.F exch_z_xyz_r8.F \
 exch_uv_agrid_xy_rl.F exch_uv_agrid_xy_rs.F exch_uv_agrid_xy_r4.F exch_uv_agrid_xy_r8.F \
 exch_uv_agrid_xyz_rl.F exch_uv_agrid_xyz_rs.F exch_uv_agrid_xyz_r4.F exch_uv_agrid_xyz_r8.F \
 exch_rl.F exch_rs.F exch_r4.F exch_r8.F \
 exch_rl_cube.F exch_rs_cube.F exch_r4_cube.F exch_r8_cube.F \
 exch_z_rl_cube.F exch_z_rs_cube.F exch_z_r4_cube.F exch_z_r8_cube.F \
 exch_uv_rl_cube.F exch_uv_rs_cube.F exch_uv_r4_cube.F exch_uv_r8_cube.F \
 exch_bg_rl_cube.F exch_bg_rs_cube.F exch_bg_r4_cube.F exch_bg_r8_cube.F

all: $(RL_FILES) $(RS_FILES) $(R4_FILES) $(R8_FILES)

clean_old:
	-rm -f $(OLD_FILES)

clean:
	-rm -f $(RL_FILES) $(RS_FILES) $(R4_FILES) $(R8_FILES) $(OLD_FILES)

# exch_xy_rx
exch_xy_rl.F: exch_xy_rx.template
	sed 's/RX/RL/g' exch_xy_rx.template > $@
exch_xy_rs.F: exch_xy_rx.template
	sed 's/RX/RS/g' exch_xy_rx.template > $@
exch_xy_r8.F: exch_xy_rx.template
	sed 's/RX/R8/g' exch_xy_rx.template > $@
exch_xy_r4.F: exch_xy_rx.template
	sed 's/RX/R4/g' exch_xy_rx.template > $@

# exch_3d_rx
exch_3d_rl.F: exch_3d_rx.template
	sed 's/RX/RL/g' exch_3d_rx.template > $@
exch_3d_rs.F: exch_3d_rx.template
	sed 's/RX/RS/g' exch_3d_rx.template > $@
exch_3d_r8.F: exch_3d_rx.template
	sed 's/RX/R8/g' exch_3d_rx.template > $@
exch_3d_r4.F: exch_3d_rx.template
	sed 's/RX/R4/g' exch_3d_rx.template > $@

# exch_xyz_rx
exch_xyz_rl.F: exch_xyz_rx.template
	sed 's/RX/RL/g' exch_xyz_rx.template > $@
exch_xyz_rs.F: exch_xyz_rx.template
	sed 's/RX/RS/g' exch_xyz_rx.template > $@
exch_xyz_r8.F: exch_xyz_rx.template
	sed 's/RX/R8/g' exch_xyz_rx.template > $@
exch_xyz_r4.F: exch_xyz_rx.template
	sed 's/RX/R4/g' exch_xyz_rx.template > $@

# exch_s3d_rx
exch_s3d_rl.F: exch_s3d_rx.template
	sed 's/RX/RL/g' exch_s3d_rx.template > $@
exch_s3d_rs.F: exch_s3d_rx.template
	sed 's/RX/RS/g' exch_s3d_rx.template > $@
exch_s3d_r8.F: exch_s3d_rx.template
	sed 's/RX/R8/g' exch_s3d_rx.template > $@
exch_s3d_r4.F: exch_s3d_rx.template
	sed 's/RX/R4/g' exch_s3d_rx.template > $@

# exch_uv_xy_rx
exch_uv_xy_rl.F: exch_uv_xy_rx.template
	sed 's/RX/RL/g' exch_uv_xy_rx.template > $@
exch_uv_xy_rs.F: exch_uv_xy_rx.template
	sed 's/RX/RS/g' exch_uv_xy_rx.template > $@
exch_uv_xy_r8.F: exch_uv_xy_rx.template
	sed 's/RX/R8/g' exch_uv_xy_rx.template > $@
exch_uv_xy_r4.F: exch_uv_xy_rx.template
	sed 's/RX/R4/g' exch_uv_xy_rx.template > $@

# exch_uv_3d_rx
exch_uv_3d_rl.F: exch_uv_3d_rx.template
	sed 's/RX/RL/g' exch_uv_3d_rx.template > $@
exch_uv_3d_rs.F: exch_uv_3d_rx.template
	sed 's/RX/RS/g' exch_uv_3d_rx.template > $@
exch_uv_3d_r8.F: exch_uv_3d_rx.template
	sed 's/RX/R8/g' exch_uv_3d_rx.template > $@
exch_uv_3d_r4.F: exch_uv_3d_rx.template
	sed 's/RX/R4/g' exch_uv_3d_rx.template > $@

# exch_uv_xyz_rx
exch_uv_xyz_rl.F: exch_uv_xyz_rx.template
	sed 's/RX/RL/g' exch_uv_xyz_rx.template > $@
exch_uv_xyz_rs.F: exch_uv_xyz_rx.template
	sed 's/RX/RS/g' exch_uv_xyz_rx.template > $@
exch_uv_xyz_r8.F: exch_uv_xyz_rx.template
	sed 's/RX/R8/g' exch_uv_xyz_rx.template > $@
exch_uv_xyz_r4.F: exch_uv_xyz_rx.template
	sed 's/RX/R4/g' exch_uv_xyz_rx.template > $@

# exch_z_3d_rx
exch_z_3d_rl.F: exch_z_3d_rx.template
	sed 's/RX/RL/g' exch_z_3d_rx.template > $@
exch_z_3d_rs.F: exch_z_3d_rx.template
	sed 's/RX/RS/g' exch_z_3d_rx.template > $@
exch_z_3d_r8.F: exch_z_3d_rx.template
	sed 's/RX/R8/g' exch_z_3d_rx.template > $@
exch_z_3d_r4.F: exch_z_3d_rx.template
	sed 's/RX/R4/g' exch_z_3d_rx.template > $@

# exch_sm_3d_rx
exch_sm_3d_rl.F: exch_sm_3d_rx.template
	sed 's/RX/RL/g' exch_sm_3d_rx.template > $@
exch_sm_3d_rs.F: exch_sm_3d_rx.template
	sed 's/RX/RS/g' exch_sm_3d_rx.template > $@
exch_sm_3d_r8.F: exch_sm_3d_rx.template
	sed 's/RX/R8/g' exch_sm_3d_rx.template > $@
exch_sm_3d_r4.F: exch_sm_3d_rx.template
	sed 's/RX/R4/g' exch_sm_3d_rx.template > $@

# exch_uv_agrid_3d_rx
exch_uv_agrid_3d_rl.F: exch_uv_agrid_3d_rx.template
	sed 's/RX/RL/g' exch_uv_agrid_3d_rx.template > $@
exch_uv_agrid_3d_rs.F: exch_uv_agrid_3d_rx.template
	sed 's/RX/RS/g' exch_uv_agrid_3d_rx.template > $@
exch_uv_agrid_3d_r8.F: exch_uv_agrid_3d_rx.template
	sed 's/RX/R8/g' exch_uv_agrid_3d_rx.template > $@
exch_uv_agrid_3d_r4.F: exch_uv_agrid_3d_rx.template
	sed 's/RX/R4/g' exch_uv_agrid_3d_rx.template > $@

# exch_uv_bgrid_3d_rx
exch_uv_bgrid_3d_rl.F: exch_uv_bgrid_3d_rx.template
	sed 's/RX/RL/g' exch_uv_bgrid_3d_rx.template > $@
exch_uv_bgrid_3d_rs.F: exch_uv_bgrid_3d_rx.template
	sed 's/RX/RS/g' exch_uv_bgrid_3d_rx.template > $@
exch_uv_bgrid_3d_r8.F: exch_uv_bgrid_3d_rx.template
	sed 's/RX/R8/g' exch_uv_bgrid_3d_rx.template > $@
exch_uv_bgrid_3d_r4.F: exch_uv_bgrid_3d_rx.template
	sed 's/RX/R4/g' exch_uv_bgrid_3d_rx.template > $@

# exch_uv_dgrid_3d_rx
exch_uv_dgrid_3d_rl.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/RL/g' exch_uv_dgrid_3d_rx.template > $@
exch_uv_dgrid_3d_rs.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/RS/g' exch_uv_dgrid_3d_rx.template > $@
exch_uv_dgrid_3d_r8.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/R8/g' exch_uv_dgrid_3d_rx.template > $@
exch_uv_dgrid_3d_r4.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/R4/g' exch_uv_dgrid_3d_rx.template > $@

# exch0_rx
exch0_rl.F: exch0_rx.template
	sed 's/RX/RL/g' exch0_rx.template > $@
exch0_rs.F: exch0_rx.template
	sed 's/RX/RS/g' exch0_rx.template > $@
exch0_r8.F: exch0_rx.template
	sed 's/RX/R8/g' exch0_rx.template > $@
exch0_r4.F: exch0_rx.template
	sed 's/RX/R4/g' exch0_rx.template > $@

# exch1_rx
exch1_rl.F: exch1_rx.template
	sed 's/RX/RL/g' exch1_rx.template > $@
exch1_rs.F: exch1_rx.template
	sed 's/RX/RS/g' exch1_rx.template > $@
exch1_r8.F: exch1_rx.template
	sed 's/RX/R8/g' exch1_rx.template > $@
exch1_r4.F: exch1_rx.template
	sed 's/RX/R4/g' exch1_rx.template > $@

# exch1_rx_cube
exch1_rl_cube.F: exch1_rx_cube.template
	sed 's/RX/RL/g' exch1_rx_cube.template > $@
exch1_rs_cube.F: exch1_rx_cube.template
	sed 's/RX/RS/g' exch1_rx_cube.template > $@
exch1_r8_cube.F: exch1_rx_cube.template
	sed 's/RX/R8/g' exch1_rx_cube.template > $@
exch1_r4_cube.F: exch1_rx_cube.template
	sed 's/RX/R4/g' exch1_rx_cube.template > $@

# exch1_z_rx_cube
exch1_z_rl_cube.F: exch1_z_rx_cube.template
	sed 's/RX/RL/g' exch1_z_rx_cube.template > $@
exch1_z_rs_cube.F: exch1_z_rx_cube.template
	sed 's/RX/RS/g' exch1_z_rx_cube.template > $@
exch1_z_r8_cube.F: exch1_z_rx_cube.template
	sed 's/RX/R8/g' exch1_z_rx_cube.template > $@
exch1_z_r4_cube.F: exch1_z_rx_cube.template
	sed 's/RX/R4/g' exch1_z_rx_cube.template > $@

# exch1_uv_rx_cube
exch1_uv_rl_cube.F: exch1_uv_rx_cube.template
	sed 's/RX/RL/g' exch1_uv_rx_cube.template > $@
exch1_uv_rs_cube.F: exch1_uv_rx_cube.template
	sed 's/RX/RS/g' exch1_uv_rx_cube.template > $@
exch1_uv_r8_cube.F: exch1_uv_rx_cube.template
	sed 's/RX/R8/g' exch1_uv_rx_cube.template > $@
exch1_uv_r4_cube.F: exch1_uv_rx_cube.template
	sed 's/RX/R4/g' exch1_uv_rx_cube.template > $@

# exch1_bg_rx_cube
exch1_bg_rl_cube.F: exch1_bg_rx_cube.template
	sed 's/RX/RL/g' exch1_bg_rx_cube.template > $@
exch1_bg_rs_cube.F: exch1_bg_rx_cube.template
	sed 's/RX/RS/g' exch1_bg_rx_cube.template > $@
exch1_bg_r8_cube.F: exch1_bg_rx_cube.template
	sed 's/RX/R8/g' exch1_bg_rx_cube.template > $@
exch1_bg_r4_cube.F: exch1_bg_rx_cube.template
	sed 's/RX/R4/g' exch1_bg_rx_cube.template > $@

# exch1_rx_ad
exch1_rl_ad.F: exch1_rx_ad.template
	sed 's/RX/RL/g' exch1_rx_ad.template > $@
exch1_rs_ad.F: exch1_rx_ad.template
	sed 's/RX/RS/g' exch1_rx_ad.template > $@
#exch1_r8_ad.F: exch1_rx_ad.template
#	sed 's/RX/R8/g' exch1_rx_ad.template > $@
#exch1_r4_ad.F: exch1_rx_ad.template
#	sed 's/RX/R4/g' exch1_rx_ad.template > $@

# exch1_rx_cube_ad
exch1_rl_cube_ad.F: exch1_rx_cube_ad.template
	sed 's/RX/RL/g' exch1_rx_cube_ad.template > $@
exch1_rs_cube_ad.F: exch1_rx_cube_ad.template
	sed 's/RX/RS/g' exch1_rx_cube_ad.template > $@
#exch1_r8_cube_ad.F: exch1_rx_cube_ad.template
#	sed 's/RX/R8/g' exch1_rx_cube_ad.template > $@
#exch1_r4_cube_ad.F: exch1_rx_cube_ad.template
#	sed 's/RX/R4/g' exch1_rx_cube_ad.template > $@

# exch_rx_recv_get_x
exch_rl_recv_get_x.F: exch_rx_recv_get_x.template
	sed 's/RX/RL/g' exch_rx_recv_get_x.template > $@
exch_rs_recv_get_x.F: exch_rx_recv_get_x.template
	sed 's/RX/RS/g' exch_rx_recv_get_x.template > $@
exch_r8_recv_get_x.F: exch_rx_recv_get_x.template
	sed 's/RX/R8/g' exch_rx_recv_get_x.template > $@
exch_r4_recv_get_x.F: exch_rx_recv_get_x.template
	sed 's/RX/R4/g' exch_rx_recv_get_x.template > $@

# exch_rx_recv_get_y
exch_rl_recv_get_y.F: exch_rx_recv_get_y.template
	sed 's/RX/RL/g' exch_rx_recv_get_y.template > $@
exch_rs_recv_get_y.F: exch_rx_recv_get_y.template
	sed 's/RX/RS/g' exch_rx_recv_get_y.template > $@
exch_r8_recv_get_y.F: exch_rx_recv_get_y.template
	sed 's/RX/R8/g' exch_rx_recv_get_y.template > $@
exch_r4_recv_get_y.F: exch_rx_recv_get_y.template
	sed 's/RX/R4/g' exch_rx_recv_get_y.template > $@

# exch_rx_send_put_x
exch_rl_send_put_x.F: exch_rx_send_put_x.template
	sed 's/RX/RL/g' exch_rx_send_put_x.template > $@
exch_rs_send_put_x.F: exch_rx_send_put_x.template
	sed 's/RX/RS/g' exch_rx_send_put_x.template > $@
exch_r8_send_put_x.F: exch_rx_send_put_x.template
	sed 's/RX/R8/g' exch_rx_send_put_x.template > $@
exch_r4_send_put_x.F: exch_rx_send_put_x.template
	sed 's/RX/R4/g' exch_rx_send_put_x.template > $@

# exch_rx_send_put_y
exch_rl_send_put_y.F: exch_rx_send_put_y.template
	sed 's/RX/RL/g' exch_rx_send_put_y.template > $@
exch_rs_send_put_y.F: exch_rx_send_put_y.template
	sed 's/RX/RS/g' exch_rx_send_put_y.template > $@
exch_r8_send_put_y.F: exch_rx_send_put_y.template
	sed 's/RX/R8/g' exch_rx_send_put_y.template > $@
exch_r4_send_put_y.F: exch_rx_send_put_y.template
	sed 's/RX/R4/g' exch_rx_send_put_y.template > $@

# gather_2d_rx, scatter_2d_rx
gather_2d_r8.F: gather_2d_rx.template
	sed 's/RX/R8/g' gather_2d_rx.template > $@
gather_2d_r4.F: gather_2d_rx.template
	sed 's/RX/R4/g' gather_2d_rx.template > $@
scatter_2d_r8.F: scatter_2d_rx.template
	sed 's/RX/R8/g' scatter_2d_rx.template > $@
scatter_2d_r4.F: scatter_2d_rx.template
	sed 's/RX/R4/g' scatter_2d_rx.template > $@

# gather_vec_rx, scatter_vec_rx
gather_vec_r8.F: gather_vec_rx.template
	sed 's/RX/R8/g' gather_vec_rx.template > $@
gather_vec_r4.F: gather_vec_rx.template
	sed 's/RX/R4/g' gather_vec_rx.template > $@
scatter_vec_r8.F: scatter_vec_rx.template
	sed 's/RX/R8/g' scatter_vec_rx.template > $@
scatter_vec_r4.F: scatter_vec_rx.template
	sed 's/RX/R4/g' scatter_vec_rx.template > $@

# gather_2d_wh_rx, scatter_2d_wh_rx
gather_2d_wh_r8.F: gather_2d_wh_rx.template
	sed 's/RX/R8/g' gather_2d_wh_rx.template > $@
gather_2d_wh_r4.F: gather_2d_wh_rx.template
	sed 's/RX/R4/g' gather_2d_wh_rx.template > $@
scatter_2d_wh_r8.F: scatter_2d_wh_rx.template
	sed 's/RX/R8/g' scatter_2d_wh_rx.template > $@
scatter_2d_wh_r4.F: scatter_2d_wh_rx.template
	sed 's/RX/R4/g' scatter_2d_wh_rx.template > $@

