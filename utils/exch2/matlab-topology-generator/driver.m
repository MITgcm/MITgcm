% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/driver.m,v 1.5 2012/10/20 17:43:41 jmc Exp $
% $Name:  $

% Create exch2 communication map and schedule for a cube sphere grid with
% constant tile size tnx x tny.

fprintf('Since Oct 13, 2010 (after checkpoint62l), matlab-topology-generator\n');
fprintf(' is no longer supported and does not work anymore (due to additions to\n')
fprintf(' pkg/exch2 which have not been incorporated into the matlab scripts).\n')
fprintf('examples of "data.exch2" file are provided in MITgcm/utils/exch2/input/\n')
return

% Use red-green-blue shorthand for cube index space specification
% In this notation cube faces are laid out as shown below
%
%                        f5(ng,nb)  f6(nr,nb)
%             f3(nb,nr)  f4(ng,nr)
%  f1(nr,ng)  f2(nb,ng)
%---
%nr=32; ng=64; nb=128;
nr=32; ng=32; nb=32;
%nr=510; ng=510; nb=510;
%nr=30; ng=30; nb=30;

%- Choose tile subgrid sizes for each face.
% nr,ng,nb must be integer multiples of tnx and tny.
%---
%tnx=85;tny=85;
%tnx=10;tny=10;
%tnx=16;tny=16;
tnx=16;tny=32;
%tnx=32;tny=32;
%tnx=32;tny=8;
%tnx=192;tny=64;
tnx=8;tny=4;
%tnx=16;tny=8; % <- adjustment.cs-32x32x1

%- select option for global-IO mapping: mapIO
%  =-1 : old format: put domains 1 after the other in the X direction
%        this is not necessary "compact"
%  = 1 : compact format, 1 domain after the other (mostly in Y direction)
%        but needs to fold some domains (face) if too large
%  = 0 : compact format (= 1 long line), one domain after the other
%---
mapIO=-1;

% nr = 90; ng = 360; nb = 90; tnx=90; tny=90; mapIO=1; %- polar-cap grid

if mapIO==1,
%- calculate size in X of global-IO map: = greater divider of nr,ng,nb
  [divlist]=exch2_divider([nr ng nb]);
   mapIO=prod(divlist);
end

% Make list of domains. Assume MITgcm standard cube layout, three
% color path labeling and global indexing convention.
clear domain ndomains domain_nx domain_ny
[ndomains,domain,domain_nx,domain_ny] = exch2_setup_cs6_domains(nr,ng,nb);

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
