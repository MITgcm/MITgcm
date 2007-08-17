# $Header: /u/gcmpack/MITgcm/eesupp/src/Makefile,v 1.12 2007/08/17 18:34:40 jmc Exp $
# $Name:  $

# Makefile for generate eesupp source code from templates.
# Why do we do this?  Answer: saves typing!

RL_FILES = exch_xy_rl.F exch_3d_rl.F exch_xyz_rl.F exch_s3d_rl.F \
           exch_uv_xy_rl.F exch_uv_3d_rl.F exch_uv_xyz_rl.F \
           exch_z_3d_rl.F exch_sm_3d_rl.F \
           exch_uv_agrid_3d_rl.F exch_uv_dgrid_3d_rl.F \
           exch_rl.F \
           exch_rl_cube.F exch_uv_rl_cube.F exch_z_rl_cube.F \
           exch_rl_recv_get_x.F exch_rl_recv_get_y.F \
           exch_rl_send_put_x.F exch_rl_send_put_y.F

RS_FILES = exch_xy_rs.F exch_3d_rs.F exch_xyz_rs.F exch_s3d_rs.F \
           exch_uv_xy_rs.F exch_uv_3d_rs.F exch_uv_xyz_rs.F \
           exch_z_3d_rs.F exch_sm_3d_rs.F \
           exch_uv_agrid_3d_rs.F exch_uv_dgrid_3d_rs.F \
           exch_rs.F \
           exch_rs_cube.F exch_uv_rs_cube.F exch_z_rs_cube.F \
           exch_rs_recv_get_x.F exch_rs_recv_get_y.F \
           exch_rs_send_put_x.F exch_rs_send_put_y.F

R4_FILES = exch_xy_r4.F exch_3d_r4.F exch_xyz_r4.F exch_s3d_r4.F \
           exch_uv_xy_r4.F exch_uv_3d_r4.F exch_uv_xyz_r4.F \
           exch_z_3d_r4.F exch_sm_3d_r4.F \
           exch_uv_agrid_3d_r4.F exch_uv_dgrid_3d_r4.F \
           exch_r4.F \
           exch_r4_cube.F exch_uv_r4_cube.F exch_z_r4_cube.F \
           exch_r4_recv_get_x.F exch_r4_recv_get_y.F \
           exch_r4_send_put_x.F exch_r4_send_put_y.F

R8_FILES = exch_xy_r8.F exch_3d_r8.F exch_xyz_r8.F exch_s3d_r8.F \
           exch_uv_xy_r8.F exch_uv_3d_r8.F exch_uv_xyz_r8.F \
           exch_z_3d_r8.F exch_sm_3d_r8.F \
           exch_uv_agrid_3d_r8.F exch_uv_dgrid_3d_r8.F \
           exch_r8.F \
           exch_r8_cube.F exch_uv_r8_cube.F exch_z_r8_cube.F \
           exch_r8_recv_get_x.F exch_r8_recv_get_y.F \
           exch_r8_send_put_x.F exch_r8_send_put_y.F

OLD_FILES = exch_z_xy_rl.F exch_z_xy_rs.F exch_z_xy_r4.F exch_z_xy_r8.F \
 exch_z_xyz_rl.F exch_z_xyz_rs.F exch_z_xyz_r4.F exch_z_xyz_r8.F \
 exch_uv_agrid_xy_rl.F exch_uv_agrid_xy_rs.F exch_uv_agrid_xy_r4.F exch_uv_agrid_xy_r8.F \
 exch_uv_agrid_xyz_rl.F exch_uv_agrid_xyz_rs.F exch_uv_agrid_xyz_r4.F exch_uv_agrid_xyz_r8.F


all: $(RL_FILES) $(RS_FILES) $(R4_FILES) $(R8_FILES)

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

# exch_uv_dgrid_3d_rx
exch_uv_dgrid_3d_rl.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/RL/g' exch_uv_dgrid_3d_rx.template > $@
exch_uv_dgrid_3d_rs.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/RS/g' exch_uv_dgrid_3d_rx.template > $@
exch_uv_dgrid_3d_r8.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/R8/g' exch_uv_dgrid_3d_rx.template > $@
exch_uv_dgrid_3d_r4.F: exch_uv_dgrid_3d_rx.template
	sed 's/RX/R4/g' exch_uv_dgrid_3d_rx.template > $@

# exch_rx
exch_rl.F: exch_rx.template
	sed 's/RX/RL/g' exch_rx.template > $@
exch_rs.F: exch_rx.template
	sed 's/RX/RS/g' exch_rx.template > $@
exch_r8.F: exch_rx.template
	sed 's/RX/R8/g' exch_rx.template > $@
exch_r4.F: exch_rx.template
	sed 's/RX/R4/g' exch_rx.template > $@

# exch_rx_cube
exch_rl_cube.F: exch_rx_cube.template
	sed 's/RX/RL/g' exch_rx_cube.template > $@
exch_rs_cube.F: exch_rx_cube.template
	sed 's/RX/RS/g' exch_rx_cube.template > $@
exch_r8_cube.F: exch_rx_cube.template
	sed 's/RX/R8/g' exch_rx_cube.template > $@
exch_r4_cube.F: exch_rx_cube.template
	sed 's/RX/R4/g' exch_rx_cube.template > $@

# exch_uv_rx_cube
exch_uv_rl_cube.F: exch_uv_rx_cube.template
	sed 's/RX/RL/g' exch_uv_rx_cube.template > $@
exch_uv_rs_cube.F: exch_uv_rx_cube.template
	sed 's/RX/RS/g' exch_uv_rx_cube.template > $@
exch_uv_r8_cube.F: exch_uv_rx_cube.template
	sed 's/RX/R8/g' exch_uv_rx_cube.template > $@
exch_uv_r4_cube.F: exch_uv_rx_cube.template
	sed 's/RX/R4/g' exch_uv_rx_cube.template > $@

# exch_z_rx_cube
exch_z_rl_cube.F: exch_z_rx_cube.template
	sed 's/RX/RL/g' exch_z_rx_cube.template > $@
exch_z_rs_cube.F: exch_z_rx_cube.template
	sed 's/RX/RS/g' exch_z_rx_cube.template > $@
exch_z_r8_cube.F: exch_z_rx_cube.template
	sed 's/RX/R8/g' exch_z_rx_cube.template > $@
exch_z_r4_cube.F: exch_z_rx_cube.template
	sed 's/RX/R4/g' exch_z_rx_cube.template > $@

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

