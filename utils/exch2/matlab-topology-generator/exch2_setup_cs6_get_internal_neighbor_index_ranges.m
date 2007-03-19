function [tile] = ...
         exch2_setup_cs6_get_internal_neighbor_index_ranges(tile,domain,ntiles)
% For internal tiles, figure out what range of points I send my edges to and 
% setup the reindexing operators. 
% This functions sets the following for each west face neighbor of the source 
% tile
%   tile.wYLo_tl  :: Target local lower bound index in Y
%   tile.wYHi_tl  :: Target local upper bound index in Y
%   tile.wYOff_t  :: Target local to canonical index offset conversion in Y
%   tile.eYLo_tl  :: Target local lower bound index in Y
%   tile.eYHi_tl  :: Target local upper bound index in Y
%   tile.eYOff_t  :: Target local to canonical index offset conversion in Y
%   tile.nXLo_tl  :: Target local lower bound index in X
%   tile.nXHi_tl  :: Target local upper bound index in X
%   tile.nXOff_t  :: Target local to canonical index offset conversion in X
%   tile.sXLo_tl  :: Target local lower bound index in X
%   tile.sXHi_tl  :: Target local upper bound index in X
%   tile.sXOff_t  :: Target local to canonical index offset conversion in X

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_get_internal_neighbor_index_ranges.m,v 1.2 2007/03/19 20:34:26 jmc Exp $
% $Name:  $

for it=1:ntiles
 % First do tiles that are internal to the domain
 % Set west and east edge numbers
 % Source bounds
 srangeLoY_c=tile(it).tbasey+1;
 srangeHiY_c=tile(it).tny+tile(it).tbasey;
 if tile(it).wDomain==tile(it).mydomain
  nW = tile(it).nW;
  for j=1:nW
   % get target tile
   tt=tile(it).wTile(j);
   % find indexes in target bounds that intersect target range 
   % (no rotations needed since we are doing internal tiles here).
   trangeLoY_l=1;
   trangeLoY_c=trangeLoY_l+tile(tt).tbasey;
   trangeHiY_l=tile(tt).tny;
   trangeHiY_c=trangeHiY_l+tile(tt).tbasey;
   intersectYHi_c=min(trangeHiY_c,srangeHiY_c);
   intersectYLo_c=max(trangeLoY_c,srangeLoY_c);
   tintersectYHi_l=intersectYHi_c-tile(tt).tbasey;
   tintersectYLo_l=intersectYLo_c-tile(tt).tbasey;
   tile(it).w_jtlo_c(j)=intersectYLo_c-1;
   tile(it).w_jthi_c(j)=intersectYHi_c+1;
   tile(it).w_itlo_c(j)=tile(tt).tnx+tile(tt).tbasex+1;
   tile(it).w_ithi_c(j)=tile(tt).tnx+tile(tt).tbasex+1;
   tile(it).w_jtlo_l(j)=tile(it).w_jtlo_c(j)-tile(tt).tbasey;
   tile(it).w_jthi_l(j)=tile(it).w_jthi_c(j)-tile(tt).tbasey;
   tile(it).w_itlo_l(j)=tile(it).w_itlo_c(j)-tile(tt).tbasex;
   tile(it).w_ithi_l(j)=tile(it).w_ithi_c(j)-tile(tt).tbasex;
   Pi=[1 0];
   Pj=[0 1];
   tile(it).w_pi(j,:)=Pi;
   tile(it).w_pj(j,:)=Pj;
   tile(it).w_oi(j)=-(tile(tt).tnx+tile(tt).tbasex);
   tile(it).w_oi(j)=0;
   tile(it).w_oj(j)=0;
   tile(it).w_oi_f(j)=-(tile(tt).tnx+tile(tt).tbasex);
   tile(it).w_oi_f(j)=0;
   tile(it).w_oj_f(j)=0;
%  tile(it).wYLo_tl(j)=tintersectYLo_l;
%  tile(it).wYHi_tl(j)=tintersectYHi_l;
%  tile(it).wYOff_t(j)=tile(tt).tbasey;
  end
 end
 % Set east edge numbers
 if tile(it).eDomain==tile(it).mydomain
  nE = tile(it).nE;
  for j=1:nE
   % get target tile
   tt=tile(it).eTile(j);
   % find indexes in target bounds that intersect target range 
   % (no rotations needed since we are doing internal tiles here).
   trangeLoY_l=1;
   trangeLoY_c=trangeLoY_l+tile(tt).tbasey;
   trangeHiY_l=tile(tt).tny;
   trangeHiY_c=trangeHiY_l+tile(tt).tbasey;
   intersectYHi_c=min(trangeHiY_c,srangeHiY_c);
   intersectYLo_c=max(trangeLoY_c,srangeLoY_c);
   tintersectYHi_l=intersectYHi_c-tile(tt).tbasey;
   tintersectYLo_l=intersectYLo_c-tile(tt).tbasey;
   tile(it).e_jtlo_c(j)=intersectYLo_c-1;
   tile(it).e_jthi_c(j)=intersectYHi_c+1;
   tile(it).e_itlo_c(j)=0+tile(tt).tbasex;
   tile(it).e_ithi_c(j)=0+tile(tt).tbasex;
   tile(it).e_jtlo_l(j)=tile(it).e_jtlo_c(j)-tile(tt).tbasey;
   tile(it).e_jthi_l(j)=tile(it).e_jthi_c(j)-tile(tt).tbasey;
   tile(it).e_itlo_l(j)=tile(it).e_itlo_c(j)-tile(tt).tbasex;
   tile(it).e_ithi_l(j)=tile(it).e_ithi_c(j)-tile(tt).tbasex;
   Pi=[1 0];
   Pj=[0 1];
   tile(it).e_pi(j,:)=Pi;
   tile(it).e_pj(j,:)=Pj;
   tile(it).e_oi(j)=tile(it).tnx+tile(it).tbasex;
   tile(it).e_oi(j)=0;
   tile(it).e_oj(j)=0;
   tile(it).e_oi_f(j)=tile(it).tnx+tile(it).tbasex;
   tile(it).e_oi_f(j)=0;
   tile(it).e_oj_f(j)=0;
%  tile(it).eYLo_tl(j)=tintersectYLo_l;
%  tile(it).eYHi_tl(j)=tintersectYHi_l;
%  tile(it).eYOff_t(j)=tile(tt).tbasey;
  end
 end
 % Set north and south edge numbers
 % Source bounds
 srangeLoX_c=tile(it).tbasex+1-1;
 srangeHiX_c=tile(it).tnx+tile(it).tbasex+1;
 if tile(it).nDomain==tile(it).mydomain
  nN = tile(it).nN;
  for j=1:nN
   % get target tile
   tt=tile(it).nTile(j);
   % find indexes in target bounds that intersect target range 
   % (no rotations needed since we are doing internal tiles here).
   trangeLoX_l=0;
   trangeLoX_c=trangeLoX_l+tile(tt).tbasex;
   trangeHiX_l=tile(tt).tnx+1;
   trangeHiX_c=trangeHiX_l+tile(tt).tbasex;
   intersectXHi_c=min(trangeHiX_c,srangeHiX_c);
   intersectXLo_c=max(trangeLoX_c,srangeLoX_c);
   tintersectXHi_l=intersectXHi_c-tile(tt).tbasex;
   tintersectXLo_l=intersectXLo_c-tile(tt).tbasex;
   tile(it).n_jtlo_c(j)=0+tile(tt).tbasey;
   tile(it).n_jthi_c(j)=0+tile(tt).tbasey;
   tile(it).n_itlo_c(j)=intersectXLo_c;
   tile(it).n_ithi_c(j)=intersectXHi_c;
   tile(it).n_jtlo_l(j)=tile(it).n_jtlo_c(j)-tile(tt).tbasey;
   tile(it).n_jthi_l(j)=tile(it).n_jthi_c(j)-tile(tt).tbasey;
   tile(it).n_itlo_l(j)=tile(it).n_itlo_c(j)-tile(tt).tbasex;
   tile(it).n_ithi_l(j)=tile(it).n_ithi_c(j)-tile(tt).tbasex;
   Pi=[1 0];
   Pj=[0 1];
   tile(it).n_pi(j,:)=Pi;
   tile(it).n_pj(j,:)=Pj;
   tile(it).n_oi(j)=0;
   tile(it).n_oj(j)=tile(it).tny+tile(it).tbasey;
   tile(it).n_oj(j)=0;
   tile(it).n_oi_f(j)=0;
   tile(it).n_oj_f(j)=tile(it).tny+tile(it).tbasey;
   tile(it).n_oj_f(j)=0;
%  tile(it).nXLo_tl(j)=tintersectXLo_l;
%  tile(it).nXHi_tl(j)=tintersectXHi_l;
%  tile(it).nXOff_t(j)=tile(tt).tbasex;
  end
 end
 if tile(it).sDomain==tile(it).mydomain
 % Set south edge numbers
  nS = tile(it).nS;
  for j=1:nS
   % get target tile
   tt=tile(it).sTile(j);
   % find indexes in target bounds that intersect target range 
   % (no rotations needed since we are doing internal tiles here).
   trangeLoX_l=1;
   trangeLoX_c=trangeLoX_l+tile(tt).tbasex;
   trangeHiX_l=tile(tt).tnx;
   trangeHiX_c=trangeHiX_l+tile(tt).tbasex;
   intersectXHi_c=min(trangeHiX_c,srangeHiX_c);
   intersectXLo_c=max(trangeLoX_c,srangeLoX_c);
   tintersectXHi_l=intersectXHi_c-tile(tt).tbasex;
   tintersectXLo_l=intersectXLo_c-tile(tt).tbasex;
   tile(it).s_jtlo_c(j)=tile(tt).tny+tile(tt).tbasey+1;
   tile(it).s_jthi_c(j)=tile(tt).tny+tile(tt).tbasey+1;
   tile(it).s_itlo_c(j)=intersectXLo_c;
   tile(it).s_ithi_c(j)=intersectXHi_c;
   tile(it).s_jtlo_l(j)=tile(it).s_jtlo_c(j)-tile(tt).tbasey;
   tile(it).s_jthi_l(j)=tile(it).s_jthi_c(j)-tile(tt).tbasey;
   tile(it).s_itlo_l(j)=tile(it).s_itlo_c(j)-tile(tt).tbasex;
   tile(it).s_ithi_l(j)=tile(it).s_ithi_c(j)-tile(tt).tbasex;
   Pi=[1 0];
   Pj=[0 1];
   tile(it).s_pi(j,:)=Pi;
   tile(it).s_pj(j,:)=Pj;
   tile(it).s_oi(j)=0;
   tile(it).s_oj(j)=-(tile(tt).tny+tile(tt).tbasey);
   tile(it).s_oj(j)=0;
   tile(it).s_oi_f(j)=0;
   tile(it).s_oj_f(j)=-(tile(tt).tny+tile(tt).tbasey);
   tile(it).s_oj_f(j)=0;
%  tile(it).sXLo_tl(j)=tintersectXLo_l;
%  tile(it).sXHi_tl(j)=tintersectXHi_l;
%  tile(it).sXOff_t(j)=tile(tt).tbasex;
  end
 end
end

return
