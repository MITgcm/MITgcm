# $Header: /u/gcmpack/MITgcm/pkg/exch2/Makefile,v 1.17 2010/05/27 14:40:09 jmc Exp $
# $Name:  $

# Makefile for generate eesupp source code from templates.
# Why do we do this?  Answer: saves typing!

RL_FILES = exch2_get_rl1.F \
           exch2_get_rl2.F \
           exch2_put_rl1.F \
           exch2_put_rl2.F \
           exch2_ad_get_rl1.F \
           exch2_ad_get_rl2.F \
           exch2_ad_put_rl1.F \
           exch2_ad_put_rl2.F \
           exch2_recv_rl1.F \
           exch2_recv_rl2.F \
           exch2_send_rl1.F \
           exch2_send_rl2.F \
           exch2_rl1_cube.F \
           exch2_rl2_cube.F \
           exch2_rl1_cube_ad.F \
           exch2_rl2_cube_ad.F \
           exch2_3d_rl.F \
           exch2_s3d_rl.F \
           exch2_z_3d_rl.F \
           exch2_sm_3d_rl.F \
           exch2_uv_3d_rl.F \
           exch2_uv_agrid_3d_rl.F \
           exch2_uv_bgrid_3d_rl.F \
           exch2_uv_cgrid_3d_rl.F \
           exch2_uv_dgrid_3d_rl.F

RS_FILES = exch2_get_rs1.F \
           exch2_get_rs2.F \
           exch2_put_rs1.F \
           exch2_put_rs2.F \
           exch2_ad_get_rs1.F \
           exch2_ad_get_rs2.F \
           exch2_ad_put_rs1.F \
           exch2_ad_put_rs2.F \
           exch2_recv_rs1.F \
           exch2_recv_rs2.F \
           exch2_send_rs1.F \
           exch2_send_rs2.F \
           exch2_rs1_cube.F \
           exch2_rs2_cube.F \
           exch2_rs1_cube_ad.F \
           exch2_rs2_cube_ad.F \
           exch2_3d_rs.F \
           exch2_s3d_rs.F \
           exch2_z_3d_rs.F \
           exch2_sm_3d_rs.F \
           exch2_uv_3d_rs.F \
           exch2_uv_agrid_3d_rs.F \
           exch2_uv_bgrid_3d_rs.F \
           exch2_uv_cgrid_3d_rs.F \
           exch2_uv_dgrid_3d_rs.F

R4_FILES = exch2_get_r41.F \
           exch2_get_r42.F \
           exch2_put_r41.F \
           exch2_put_r42.F \
           exch2_ad_get_r41.F \
           exch2_ad_get_r42.F \
           exch2_ad_put_r41.F \
           exch2_ad_put_r42.F \
           exch2_recv_r41.F \
           exch2_recv_r42.F \
           exch2_send_r41.F \
           exch2_send_r42.F \
           exch2_r41_cube.F \
           exch2_r42_cube.F \
           exch2_r41_cube_ad.F \
           exch2_r42_cube_ad.F \
           exch2_3d_r4.F \
           exch2_s3d_r4.F \
           exch2_z_3d_r4.F \
           exch2_sm_3d_r4.F \
           exch2_uv_3d_r4.F \
           exch2_uv_agrid_3d_r4.F \
           exch2_uv_bgrid_3d_r4.F \
           exch2_uv_cgrid_3d_r4.F \
           exch2_uv_dgrid_3d_r4.F

R8_FILES = exch2_get_r81.F \
           exch2_get_r82.F \
           exch2_put_r81.F \
           exch2_put_r82.F \
           exch2_ad_get_r81.F \
           exch2_ad_get_r82.F \
           exch2_ad_put_r81.F \
           exch2_ad_put_r82.F \
           exch2_recv_r81.F \
           exch2_recv_r82.F \
           exch2_send_r81.F \
           exch2_send_r82.F \
           exch2_r81_cube.F \
           exch2_r82_cube.F \
           exch2_r81_cube_ad.F \
           exch2_r82_cube_ad.F \
           exch2_3d_r8.F \
           exch2_s3d_r8.F \
           exch2_z_3d_r8.F \
           exch2_sm_3d_r8.F \
           exch2_uv_3d_r8.F \
           exch2_uv_agrid_3d_r8.F \
           exch2_uv_bgrid_3d_r8.F \
           exch2_uv_cgrid_3d_r8.F \
           exch2_uv_dgrid_3d_r8.F

OLD_FILES = exch2_xy_rl.F exch2_xy_rs.F exch2_xy_r4.F exch2_xy_r8.F \
 exch2_xyz_rl.F    exch2_xyz_rs.F    exch2_xyz_r4.F    exch2_xyz_r8.F \
 exch2_z_xy_rl.F   exch2_z_xy_rs.F   exch2_z_xy_r4.F   exch2_z_xy_r8.F \
 exch2_uv_xy_rl.F  exch2_uv_xy_rs.F  exch2_uv_xy_r4.F  exch2_uv_xy_r8.F \
 exch2_uv_xyz_rl.F exch2_uv_xyz_rs.F exch2_uv_xyz_r4.F exch2_uv_xyz_r8.F \
 exch2_uv_agrid_xy_rl.F exch2_uv_agrid_xy_rs.F exch2_uv_agrid_xy_r4.F exch2_uv_agrid_xy_r8.F \
 exch2_uv_agrid_xyz_rl.F exch2_uv_agrid_xyz_rs.F exch2_uv_agrid_xyz_r4.F exch2_uv_agrid_xyz_r8.F \
 exch2_recv_rl1_ad.F exch2_recv_rs1_ad.F exch2_recv_r41_ad.F exch2_recv_r81_ad.F \
 exch2_recv_rl2_ad.F exch2_recv_rs2_ad.F exch2_recv_r42_ad.F exch2_recv_r82_ad.F \
 exch2_send_rl1_ad.F exch2_send_rs1_ad.F exch2_send_r41_ad.F exch2_send_r81_ad.F \
 exch2_send_rl2_ad.F exch2_send_rs2_ad.F exch2_send_r42_ad.F exch2_send_r82_ad.F

all: $(RL_FILES) $(RS_FILES) $(R4_FILES) $(R8_FILES)

clean_old:
	-rm -f $(OLD_FILES)

clean:
	-rm -f $(RL_FILES) $(RS_FILES) $(R4_FILES) $(R8_FILES) $(OLD_FILES)

# exch2_get_rx1
exch2_get_rl1.F:    exch2_get_rx1.template
	sed 's/RX/RL/g' exch2_get_rx1.template > $@
exch2_get_rs1.F:    exch2_get_rx1.template
	sed 's/RX/RS/g' exch2_get_rx1.template > $@
exch2_get_r41.F:    exch2_get_rx1.template
	sed 's/RX/R4/g' exch2_get_rx1.template > $@
exch2_get_r81.F:    exch2_get_rx1.template
	sed 's/RX/R8/g' exch2_get_rx1.template > $@

# exch2_get_rx2
exch2_get_rl2.F:    exch2_get_rx2.template
	sed 's/RX/RL/g' exch2_get_rx2.template > $@
exch2_get_rs2.F:    exch2_get_rx2.template
	sed 's/RX/RS/g' exch2_get_rx2.template > $@
exch2_get_r42.F:    exch2_get_rx2.template
	sed 's/RX/R4/g' exch2_get_rx2.template > $@
exch2_get_r82.F:    exch2_get_rx2.template
	sed 's/RX/R8/g' exch2_get_rx2.template > $@

# exch2_put_rx1
exch2_put_rl1.F:    exch2_put_rx1.template
	sed 's/RX/RL/g' exch2_put_rx1.template > $@
exch2_put_rs1.F:    exch2_put_rx1.template
	sed 's/RX/RS/g' exch2_put_rx1.template > $@
exch2_put_r41.F:    exch2_put_rx1.template
	sed 's/RX/R4/g' exch2_put_rx1.template > $@
exch2_put_r81.F:    exch2_put_rx1.template
	sed 's/RX/R8/g' exch2_put_rx1.template > $@

# exch2_put_rx2
exch2_put_rl2.F:    exch2_put_rx2.template
	sed 's/RX/RL/g' exch2_put_rx2.template > $@
exch2_put_rs2.F:    exch2_put_rx2.template
	sed 's/RX/RS/g' exch2_put_rx2.template > $@
exch2_put_r42.F:    exch2_put_rx2.template
	sed 's/RX/R4/g' exch2_put_rx2.template > $@
exch2_put_r82.F:    exch2_put_rx2.template
	sed 's/RX/R8/g' exch2_put_rx2.template > $@

# exch2_ad_get_rx1
exch2_ad_get_rl1.F:    exch2_ad_get_rx1.template
	sed 's/RX/RL/g' exch2_ad_get_rx1.template > $@
exch2_ad_get_rs1.F:    exch2_ad_get_rx1.template
	sed 's/RX/RS/g' exch2_ad_get_rx1.template > $@
exch2_ad_get_r41.F:    exch2_ad_get_rx1.template
	sed 's/RX/R4/g' exch2_ad_get_rx1.template > $@
exch2_ad_get_r81.F:    exch2_ad_get_rx1.template
	sed 's/RX/R8/g' exch2_ad_get_rx1.template > $@

# exch2_ad_get_rx2
exch2_ad_get_rl2.F:    exch2_ad_get_rx2.template
	sed 's/RX/RL/g' exch2_ad_get_rx2.template > $@
exch2_ad_get_rs2.F:    exch2_ad_get_rx2.template
	sed 's/RX/RS/g' exch2_ad_get_rx2.template > $@
exch2_ad_get_r42.F:    exch2_ad_get_rx2.template
	sed 's/RX/R4/g' exch2_ad_get_rx2.template > $@
exch2_ad_get_r82.F:    exch2_ad_get_rx2.template
	sed 's/RX/R8/g' exch2_ad_get_rx2.template > $@

# exch2_ad_put_rx1
exch2_ad_put_rl1.F:    exch2_ad_put_rx1.template
	sed 's/RX/RL/g' exch2_ad_put_rx1.template > $@
exch2_ad_put_rs1.F:    exch2_ad_put_rx1.template
	sed 's/RX/RS/g' exch2_ad_put_rx1.template > $@
exch2_ad_put_r41.F:    exch2_ad_put_rx1.template
	sed 's/RX/R4/g' exch2_ad_put_rx1.template > $@
exch2_ad_put_r81.F:    exch2_ad_put_rx1.template
	sed 's/RX/R8/g' exch2_ad_put_rx1.template > $@

# exch2_ad_put_rx2
exch2_ad_put_rl2.F:    exch2_ad_put_rx2.template
	sed 's/RX/RL/g' exch2_ad_put_rx2.template > $@
exch2_ad_put_rs2.F:    exch2_ad_put_rx2.template
	sed 's/RX/RS/g' exch2_ad_put_rx2.template > $@
exch2_ad_put_r42.F:    exch2_ad_put_rx2.template
	sed 's/RX/R4/g' exch2_ad_put_rx2.template > $@
exch2_ad_put_r82.F:    exch2_ad_put_rx2.template
	sed 's/RX/R8/g' exch2_ad_put_rx2.template > $@

# exch2_recv_rx1
exch2_recv_rl1.F:       exch2_recv_rx1.template
	sed 's/RX/RL/g' exch2_recv_rx1.template > $@
exch2_recv_rs1.F:       exch2_recv_rx1.template
	sed 's/RX/RS/g' exch2_recv_rx1.template > $@
exch2_recv_r41.F:       exch2_recv_rx1.template
	sed 's/RX/R4/g' exch2_recv_rx1.template > $@
exch2_recv_r81.F:       exch2_recv_rx1.template
	sed 's/RX/R8/g' exch2_recv_rx1.template > $@

# exch2_recv_rx2
exch2_recv_rl2.F:       exch2_recv_rx2.template
	sed 's/RX/RL/g' exch2_recv_rx2.template > $@
exch2_recv_rs2.F:       exch2_recv_rx2.template
	sed 's/RX/RS/g' exch2_recv_rx2.template > $@
exch2_recv_r42.F:       exch2_recv_rx2.template
	sed 's/RX/R4/g' exch2_recv_rx2.template > $@
exch2_recv_r82.F:       exch2_recv_rx2.template
	sed 's/RX/R8/g' exch2_recv_rx2.template > $@

# exch2_send_rx1
exch2_send_rl1.F:       exch2_send_rx1.template
	sed 's/RX/RL/g' exch2_send_rx1.template > $@
exch2_send_rs1.F:       exch2_send_rx1.template
	sed 's/RX/RS/g' exch2_send_rx1.template > $@
exch2_send_r41.F:       exch2_send_rx1.template
	sed 's/RX/R4/g' exch2_send_rx1.template > $@
exch2_send_r81.F:       exch2_send_rx1.template
	sed 's/RX/R8/g' exch2_send_rx1.template > $@

# exch2_send_rx2
exch2_send_rl2.F:       exch2_send_rx2.template
	sed 's/RX/RL/g' exch2_send_rx2.template > $@
exch2_send_rs2.F:       exch2_send_rx2.template
	sed 's/RX/RS/g' exch2_send_rx2.template > $@
exch2_send_r42.F:       exch2_send_rx2.template
	sed 's/RX/R4/g' exch2_send_rx2.template > $@
exch2_send_r82.F:       exch2_send_rx2.template
	sed 's/RX/R8/g' exch2_send_rx2.template > $@

# exch2_rx1_cube
exch2_rl1_cube.F:       exch2_rx1_cube.template
	sed 's/RX/RL/g' exch2_rx1_cube.template > $@
exch2_rs1_cube.F:       exch2_rx1_cube.template
	sed 's/RX/RS/g' exch2_rx1_cube.template > $@
exch2_r41_cube.F:       exch2_rx1_cube.template
	sed 's/RX/R4/g' exch2_rx1_cube.template > $@
exch2_r81_cube.F:       exch2_rx1_cube.template
	sed 's/RX/R8/g' exch2_rx1_cube.template > $@

# exch2_rx2_cube
exch2_rl2_cube.F:       exch2_rx2_cube.template
	sed 's/RX/RL/g' exch2_rx2_cube.template > $@
exch2_rs2_cube.F:       exch2_rx2_cube.template
	sed 's/RX/RS/g' exch2_rx2_cube.template > $@
exch2_r42_cube.F:       exch2_rx2_cube.template
	sed 's/RX/R4/g' exch2_rx2_cube.template > $@
exch2_r82_cube.F:       exch2_rx2_cube.template
	sed 's/RX/R8/g' exch2_rx2_cube.template > $@

# exch2_rx1_cube_ad
exch2_rl1_cube_ad.F:    exch2_rx1_cube_ad.template
	sed 's/RX/RL/g' exch2_rx1_cube_ad.template > $@
exch2_rs1_cube_ad.F:    exch2_rx1_cube_ad.template
	sed 's/RX/RS/g' exch2_rx1_cube_ad.template > $@
exch2_r41_cube_ad.F:    exch2_rx1_cube_ad.template
	sed 's/RX/R4/g' exch2_rx1_cube_ad.template > $@
exch2_r81_cube_ad.F:    exch2_rx1_cube_ad.template
	sed 's/RX/R8/g' exch2_rx1_cube_ad.template > $@

# exch2_rx2_cube_ad
exch2_rl2_cube_ad.F:    exch2_rx2_cube_ad.template
	sed 's/RX/RL/g' exch2_rx2_cube_ad.template > $@
exch2_rs2_cube_ad.F:    exch2_rx2_cube_ad.template
	sed 's/RX/RS/g' exch2_rx2_cube_ad.template > $@
exch2_r42_cube_ad.F:    exch2_rx2_cube_ad.template
	sed 's/RX/R4/g' exch2_rx2_cube_ad.template > $@
exch2_r82_cube_ad.F:    exch2_rx2_cube_ad.template
	sed 's/RX/R8/g' exch2_rx2_cube_ad.template > $@

# exch2_3d_rx
exch2_3d_rl.F:          exch2_3d_rx.template
	sed 's/RX/RL/g' exch2_3d_rx.template > $@
exch2_3d_rs.F:          exch2_3d_rx.template
	sed 's/RX/RS/g' exch2_3d_rx.template > $@
exch2_3d_r4.F:          exch2_3d_rx.template
	sed 's/RX/R4/g' exch2_3d_rx.template > $@
exch2_3d_r8.F:          exch2_3d_rx.template
	sed 's/RX/R8/g' exch2_3d_rx.template > $@

# exch2_s3d_rx
exch2_s3d_rl.F:         exch2_s3d_rx.template
	sed 's/RX/RL/g' exch2_s3d_rx.template > $@
exch2_s3d_rs.F:         exch2_s3d_rx.template
	sed 's/RX/RS/g' exch2_s3d_rx.template > $@
exch2_s3d_r4.F:         exch2_s3d_rx.template
	sed 's/RX/R4/g' exch2_s3d_rx.template > $@
exch2_s3d_r8.F:         exch2_s3d_rx.template
	sed 's/RX/R8/g' exch2_s3d_rx.template > $@

# exch2_z_3d_rx
exch2_z_3d_rl.F:        exch2_z_3d_rx.template
	sed 's/RX/RL/g' exch2_z_3d_rx.template > $@
exch2_z_3d_rs.F:        exch2_z_3d_rx.template
	sed 's/RX/RS/g' exch2_z_3d_rx.template > $@
exch2_z_3d_r4.F:        exch2_z_3d_rx.template
	sed 's/RX/R4/g' exch2_z_3d_rx.template > $@
exch2_z_3d_r8.F:        exch2_z_3d_rx.template
	sed 's/RX/R8/g' exch2_z_3d_rx.template > $@

# exch2_sm_3d_rx
exch2_sm_3d_rl.F:       exch2_sm_3d_rx.template
	sed 's/RX/RL/g' exch2_sm_3d_rx.template > $@
exch2_sm_3d_rs.F:       exch2_sm_3d_rx.template
	sed 's/RX/RS/g' exch2_sm_3d_rx.template > $@
exch2_sm_3d_r4.F:       exch2_sm_3d_rx.template
	sed 's/RX/R4/g' exch2_sm_3d_rx.template > $@
exch2_sm_3d_r8.F:       exch2_sm_3d_rx.template
	sed 's/RX/R8/g' exch2_sm_3d_rx.template > $@

# exch2_uv_3d_rx
exch2_uv_3d_rl.F:       exch2_uv_3d_rx.template
	sed 's/RX/RL/g' exch2_uv_3d_rx.template > $@
exch2_uv_3d_rs.F:       exch2_uv_3d_rx.template
	sed 's/RX/RS/g' exch2_uv_3d_rx.template > $@
exch2_uv_3d_r4.F:       exch2_uv_3d_rx.template
	sed 's/RX/R4/g' exch2_uv_3d_rx.template > $@
exch2_uv_3d_r8.F:       exch2_uv_3d_rx.template
	sed 's/RX/R8/g' exch2_uv_3d_rx.template > $@

# exch2_uv_agrid_3d_rx
exch2_uv_agrid_3d_rl.F: exch2_uv_agrid_3d_rx.template
	sed 's/RX/RL/g' exch2_uv_agrid_3d_rx.template > $@
exch2_uv_agrid_3d_rs.F: exch2_uv_agrid_3d_rx.template
	sed 's/RX/RS/g' exch2_uv_agrid_3d_rx.template > $@
exch2_uv_agrid_3d_r4.F: exch2_uv_agrid_3d_rx.template
	sed 's/RX/R4/g' exch2_uv_agrid_3d_rx.template > $@
exch2_uv_agrid_3d_r8.F: exch2_uv_agrid_3d_rx.template
	sed 's/RX/R8/g' exch2_uv_agrid_3d_rx.template > $@

# exch2_uv_bgrid_3d_rx
exch2_uv_bgrid_3d_rl.F: exch2_uv_bgrid_3d_rx.template
	sed 's/RX/RL/g' exch2_uv_bgrid_3d_rx.template > $@
exch2_uv_bgrid_3d_rs.F: exch2_uv_bgrid_3d_rx.template
	sed 's/RX/RS/g' exch2_uv_bgrid_3d_rx.template > $@
exch2_uv_bgrid_3d_r4.F: exch2_uv_bgrid_3d_rx.template
	sed 's/RX/R4/g' exch2_uv_bgrid_3d_rx.template > $@
exch2_uv_bgrid_3d_r8.F: exch2_uv_bgrid_3d_rx.template
	sed 's/RX/R8/g' exch2_uv_bgrid_3d_rx.template > $@

# exch2_uv_cgrid_3d_rx
exch2_uv_cgrid_3d_rl.F: exch2_uv_cgrid_3d_rx.template
	sed 's/RX/RL/g' exch2_uv_cgrid_3d_rx.template > $@
exch2_uv_cgrid_3d_rs.F: exch2_uv_cgrid_3d_rx.template
	sed 's/RX/RS/g' exch2_uv_cgrid_3d_rx.template > $@
exch2_uv_cgrid_3d_r4.F: exch2_uv_cgrid_3d_rx.template
	sed 's/RX/R4/g' exch2_uv_cgrid_3d_rx.template > $@
exch2_uv_cgrid_3d_r8.F: exch2_uv_cgrid_3d_rx.template
	sed 's/RX/R8/g' exch2_uv_cgrid_3d_rx.template > $@

# exch2_uv_dgrid_3d_rx
exch2_uv_dgrid_3d_rl.F: exch2_uv_dgrid_3d_rx.template
	sed 's/RX/RL/g' exch2_uv_dgrid_3d_rx.template > $@
exch2_uv_dgrid_3d_rs.F: exch2_uv_dgrid_3d_rx.template
	sed 's/RX/RS/g' exch2_uv_dgrid_3d_rx.template > $@
exch2_uv_dgrid_3d_r4.F: exch2_uv_dgrid_3d_rx.template
	sed 's/RX/R4/g' exch2_uv_dgrid_3d_rx.template > $@
exch2_uv_dgrid_3d_r8.F: exch2_uv_dgrid_3d_rx.template
	sed 's/RX/R8/g' exch2_uv_dgrid_3d_rx.template > $@

