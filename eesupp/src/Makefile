# $Header: /u/gcmpack/MITgcm/eesupp/src/Makefile,v 1.2 2001/05/29 14:01:36 adcroft Exp $
# $Name:  $

# Makefile for generate eesupp source code from templates.
# Why do we do this?  Answer: saves typing!

RL_FILES = exch_xy_rl.F  exch_uv_xy_rl.F  exch_z_xy_rl.F  \
           exch_xyz_rl.F exch_uv_xyz_rl.F exch_z_xyz_rl.F \
           exch_xy_rl.F exch_xyz_rl.F \
           exch_rl.F \
           exch_rl_cube.F exch_uv_rl_cube.F exch_z_rl_cube.F \
           exch_rl_recv_get_x.F exch_rl_recv_get_y.F \
           exch_rl_send_put_x.F exch_rl_send_put_y.F

RS_FILES = exch_xy_rs.F  exch_uv_xy_rs.F  exch_z_xy_rs.F  \
           exch_xyz_rs.F exch_uv_xyz_rs.F exch_z_xyz_rs.F \
           exch_xy_rs.F exch_xyz_rs.F \
           exch_rs.F \
           exch_rs_cube.F exch_uv_rs_cube.F exch_z_rs_cube.F \
           exch_rs_recv_get_x.F exch_rs_recv_get_y.F \
           exch_rs_send_put_x.F exch_rs_send_put_y.F

all: $(RL_FILES) $(RS_FILES)

clean:
	-rm -f $(RL_FILES) $(RS_FILES)

# exch_xy_rx
exch_xy_rl.F: exch_xy_rx.template
	sed 's/RX/RL/g' exch_xy_rx.template > $@
exch_xy_rs.F: exch_xy_rx.template
	sed 's/RX/RS/g' exch_xy_rx.template > $@

# exch_xyz_rx
exch_xyz_rl.F: exch_xyz_rx.template
	sed 's/RX/RL/g' exch_xyz_rx.template > $@
exch_xyz_rs.F: exch_xyz_rx.template
	sed 's/RX/RS/g' exch_xyz_rx.template > $@

# exch_uv_xy_rx
exch_uv_xy_rl.F: exch_uv_xy_rx.template
	sed 's/RX/RL/g' exch_uv_xy_rx.template > $@
exch_uv_xy_rs.F: exch_uv_xy_rx.template
	sed 's/RX/RS/g' exch_uv_xy_rx.template > $@

# exch_uv_xyz_rx
exch_uv_xyz_rl.F: exch_uv_xyz_rx.template
	sed 's/RX/RL/g' exch_uv_xyz_rx.template > $@
exch_uv_xyz_rs.F: exch_uv_xyz_rx.template
	sed 's/RX/RS/g' exch_uv_xyz_rx.template > $@

# exch_x_xy_rx
exch_z_xy_rl.F: exch_z_xy_rx.template
	sed 's/RX/RL/g' exch_z_xy_rx.template > $@
exch_z_xy_rs.F: exch_z_xy_rx.template
	sed 's/RX/RS/g' exch_z_xy_rx.template > $@

# exch_z_xyz_rx
exch_z_xyz_rl.F: exch_z_xyz_rx.template
	sed 's/RX/RL/g' exch_z_xyz_rx.template > $@
exch_z_xyz_rs.F: exch_z_xyz_rx.template
	sed 's/RX/RS/g' exch_z_xyz_rx.template > $@

# exch_rx
exch_rl.F: exch_rx.template
	sed 's/RX/RL/g' exch_rx.template > $@
exch_rs.F: exch_rx.template
	sed 's/RX/RS/g' exch_rx.template > $@

# exch_rx_cube
exch_rl_cube.F: exch_rx_cube.template
	sed 's/RX/RL/g' exch_rx_cube.template > $@
exch_rs_cube.F: exch_rx_cube.template
	sed 's/RX/RS/g' exch_rx_cube.template > $@

# exch_uv_rx_cube
exch_uv_rl_cube.F: exch_uv_rx_cube.template
	sed 's/RX/RL/g' exch_uv_rx_cube.template > $@
exch_uv_rs_cube.F: exch_uv_rx_cube.template
	sed 's/RX/RS/g' exch_uv_rx_cube.template > $@

# exch_z_rx_cube
exch_z_rl_cube.F: exch_z_rx_cube.template
	sed 's/RX/RL/g' exch_z_rx_cube.template > $@
exch_z_rs_cube.F: exch_z_rx_cube.template
	sed 's/RX/RS/g' exch_z_rx_cube.template > $@

# exch_rx_recv_get_x
exch_rl_recv_get_x.F: exch_rx_recv_get_x.template
	sed 's/RX/RL/g' exch_rx_recv_get_x.template > $@
exch_rs_recv_get_x.F: exch_rx_recv_get_x.template
	sed 's/RX/RS/g' exch_rx_recv_get_x.template > $@

# exch_rx_recv_get_y
exch_rl_recv_get_y.F: exch_rx_recv_get_y.template
	sed 's/RX/RL/g' exch_rx_recv_get_y.template > $@
exch_rs_recv_get_y.F: exch_rx_recv_get_y.template
	sed 's/RX/RS/g' exch_rx_recv_get_y.template > $@

# exch_rx_send_put_x
exch_rl_send_put_x.F: exch_rx_send_put_x.template
	sed 's/RX/RL/g' exch_rx_send_put_x.template > $@
exch_rs_send_put_x.F: exch_rx_send_put_x.template
	sed 's/RX/RS/g' exch_rx_send_put_x.template > $@

# exch_rx_send_put_y
exch_rl_send_put_y.F: exch_rx_send_put_y.template
	sed 's/RX/RL/g' exch_rx_send_put_y.template > $@
exch_rs_send_put_y.F: exch_rx_send_put_y.template
	sed 's/RX/RS/g' exch_rx_send_put_y.template > $@
