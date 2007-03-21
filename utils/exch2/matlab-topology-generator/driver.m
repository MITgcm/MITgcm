% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/driver.m,v 1.3 2007/03/21 02:02:12 jmc Exp $
% $Name:  $

% Create exch2 communication map and schedule for a cube sphere grid with
% constant tile size tnx x tny.

% Use red-green-blue shorthand for cube index space specification
% In this notation cube faces are laid out as shown below
%
%                         f5(nr,ng)  f6(nb,ng)
%              f3(ng,nb)  f4(nr,nb)
%   f1(nb,nr)  f2(ng,nr)
%---
%nr=64;  nb=32; ng=128;
%nr=576;  nb=576; ng=576;
%nr=672;  nb=672; ng=672;
nr=32; nb=32; ng=32;
%nr=510; nb=510; ng=510;
%nr=30; nb=30; ng=30;

%- Choose tile subgrid sizes for each face.
% nr,nb,ng must be integer multiples of tnx and tny.
%---
%tnx=85;tny=85;
%tnx=10;tny=10;
%tnx=16;tny=16;
tnx=16;tny=32;
%tnx=32;tny=32;
%tnx=32;tny=8;
%tnx=192;tny=64;
tnx=8;tny=4;

% nr = 360; ng = 90; nb = 90; tnx=90; tny=90; %- polar-cap grid

%- select option for global-IO mapping: mapIO
%  =-1 : old format: put domains 1 after the other in the X direction
%        this is not necessary "compact"
%  = 1 : compact format, 1 domain after the other (mostly in Y direction)
%        but needs to fold some domains (face) if too large
%  = 0 : compact format (= 1 long line), one domain after the other
%---
mapIO=-1;

if mapIO==1,
%- calculate size in X of global-IO map: = greater divider of nr,nb,ng
  [divlist]=exch2_divider([nr nb ng]);
   mapIO=prod(divlist);
end

% Make list of domains. Assume MITgcm standard cube layout, three
% color path labeling and global indexing convention.
clear domain ndomains domain_nx domain_ny
[ndomains,domain,domain_nx,domain_ny] = exch2_setup_cs6_domains(nr,nb,ng);

% Now create basic tile definitions for each domain with their offsets
% within the domain
% tn[xy]        :: tile extents in x and y
% tbase[xy]     :: offset of tile local coords from domain coords
%               :: tdom[xy] = tlocal[xy] + tbase[xy]
% t[xy]globallo :: global composite domain coordinate associated with
%               :: tlocal[xy]=(1,1)
% mydomain      :: domain number the tile belongs to
% tileid        :: identifier number for tile
% tx            :: tile x coordinate within domain tiling
% ty            :: tile y coordinate within domain tiling
clear tile
[tile,ntiles,ierr,domain]= ...
 exch2_setup_cs6_tiles(tnx,tny,domain,ndomains,mapIO);

% Set neighbor domains for each tile
[tile] = exch2_setup_cs6_get_neighbor_domains(tile, domain, ntiles);

% Let try and figure out what points I send my edges to. We do this by
% a search procedure rather than a functional relationship. The search
% procedure visits each edge of each tile in turn.  For internal edges
% (edges that don't cross a domain boundary) the index range at +/-1
% in the normal direction to the edge is searched for. This identifies
% all the tiles that border this tile.
[tile] = exch2_setup_cs6_get_internal_neighbor_tiles(       tile, domain, ntiles);
[tile] = exch2_setup_cs6_get_internal_neighbor_index_ranges(tile, domain, ntiles);

[tile] = exch2_setup_cs6_get_external_neighbor_tiles(       tile, domain, ntiles);

% Draw a picture of the full domain and its tiles in the standard cube layout
% exch2_setup_cs6_plot(domain, tile, tnx, tny);

% Squeeze the blank tiles out
[tile, domain] = exch2_setup_squeeze_blanks(domain, tile, tnx, tny);

% Write attributes for a tile
exch2_setup_cs6_print( domain, tile, tnx, tny, mapIO, 1)
