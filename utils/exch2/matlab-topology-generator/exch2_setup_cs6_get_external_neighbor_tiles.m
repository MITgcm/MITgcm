function [tile] = ...
         exch2_setup_cs6_get_external_neighbor_tiles(tile, domain, ntiles)
% Figure out what points I send my edges to. We do this by a search procedure 
% rather than a functional relationship. The search procedure visits each edge 
% of each tile in turn.  For external edges (edges that do cross a domain 
% boundary) the index range at +/-1 in the normal direction to the edge is 
% searched for subject to a possible rotation and translation moving betwee
% domains.

% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_setup_cs6_get_external_neighbor_tiles.m,v 1.2 2007/03/19 20:34:26 jmc Exp $
% $Name:  $

for it=1:ntiles
% Only do tiles that are external to the domain
  myD = tile(it).mydomain;
% First do tile to the west
  wD  = tile(it).wDomain;
  if wD~=myD
   tile(it).isWedge=1;
%  Now need to find remote tile(s) that have target points on the line one to the
%  west and that have the north-south extents of this tile.
%  Convert west edge canonical coordinate (_sc) to canonical coordinate in remote 
%  domain (_tc) using rule for west->north domain boundary.
%  Notation
%  nW       :: Number of neighbor tiles for my polygon face called "west".
%  wTile    :: List of tile numbers on my polygon face called "west".
%           :: w_ is west.
%  w_itlo_c :: Canonical low i value of target neighbor tile for a w-edge, single width 
%           :: exchange [ in itlo_c, i is i, t is target, lo is low, c is canonical ]
%  w_ithi_c :: Canonical high i value of target neighbor tile for a w-edge, single width
%           :: exchange
%  w_itlo_l :: Local low i value of target neighbor tile for an exchange
%  w_ithi_l :: Local high i value of target neighbor tile for an exchange
%  w_jtlo_c :: Canonical low j value of target neighbor tile for a w-edge, single width 
%           :: exchange [ in jtlo_c, j is j, t is target, lo is low, c is canonical ]
%  w_jthi_c :: Canonical low j value of target neighbor tile for a w-edge, single width 
%           :: exchange
%  w_jtlo_l :: Local low j value of target neighbor tile for an exchange
%  w_jthi_l :: Local high j value of target neighbor tile for an exchange
%  Pi       :: Permutation operator used in mapping (w_itXX_c, w_jtXX_c) to
%           :: (w_isXX_c) [where s is source tile i.e. this tile].
%  oi       :: offset operator used in mapping (w_itXX_c, w_jtXX_c) to
%           :: (w_isXX_c).
%  Pj       :: Permutation operator used in mapping (w_itXX_c, w_jtXX_c) to
%           :: (w_jsXX_c) [where s is source tile i.e. this tile].
%  oj       :: offset operator used in mapping (w_itXX_c, w_jtXX_c) to
%           :: (w_jsXX_c).
%  w_islo_c :: Canonical low i value of source neighbor tile for a w-edge, single width 
%           :: exchange [ in islo_c, i is i, s is source, lo is low, c is canonical ]
%  w_ishi_c :: Canonical high i value of source neighbor tile for a w-edge, single width
%           :: exchange
%  w_islo_l :: Local low i value of source neighbor tile for an exchange
%  w_ishi_l :: Local high i value of source neighbor tile for an exchange
%  w_jslo_c :: Canonical low j value of source neighbor tile for a w-edge, single width 
%           :: exchange [ in jtlo_c, j is j, t is target, lo is low, c is canonical ]
%  w_jshi_c :: Canonical low j value of source neighbor tile for a w-edge, single width 
%           :: exchange
%  w_jslo_l :: Local low j value of target neighbor tile for an exchange
%  w_jshi_l :: Local high j value of target neighbor tile for an exchange
%  
%  Together these numbers define the relationship
%  sourcetile(islo_l:ishi_l:is_step,jslo_l:jshi_l:js_step) --> 
%  targettile(itlo_l:ithi_l,jtlo_l:jthi_l)
%  Trial plane/line is along the western edge.
%  ryLo_sc :: Canonical coordinate in source tile that defines lower bound of trial plane/line 
%          :: to calculate exchange numbers for.
%  ryHi_sc :: Canonical coordinate in source tile that defines upper bound of trial plane/line
%          :: to calculate exchange numbers for.
%  rFace   :: Remote face orientation label.
   ryLo_sc=tile(it).tbasey+1;
   ryHi_sc=tile(it).tbasey+tile(it).tny;
   rFace = domain(myD).wFace;
   if rFace == 'n'
%   A w <-> n interface (rotation is required)
%   Find the tile north face(s) in the domain to my west that abut the trial line/plane.
%   Trial plane/line is along northern edge with horizontal sequencing inverted.
    ryHi_tc=domain(wD).dny;
    rxHi_tc=-ryLo_sc+domain(wD).dnx+1;  % This is the rule for cell-centered quantities
    rxLo_tc=-ryHi_sc+domain(wD).dnx+1;  % This is the rule for cell-centered quantities
                                        % For cell centered quantities 1 -> domain(wD).dnx, 
                                        %                              2 -> domain(wD).dnx-1, etc...
                                        % For cell face quantities 1 -> domain(wD).dnx+1, 
                                        %                          2 -> domain(wD).dnx, etc...
    nW=0;
    for j=domain(wD).tileidlo:domain(wD).tileidhi
     if  tile(j).tny+tile(j).tbasey == ryHi_tc ...
       & 1 + tile(j).tbasex         <= rxHi_tc ...
       & tile(j).tnx+tile(j).tbasex >= rxLo_tc
      nW = nW+1;
      tile(it).nW=nW;
      tile(it).wTile(nW)=j;
      tile(it).w_itlo_c(nW)=max(1 + tile(j).tbasex,rxLo_tc)-1;
      tile(it).w_ithi_c(nW)=min(tile(j).tnx+tile(j).tbasex,rxHi_tc)+1;
      tile(it).w_itlo_l(nW)=tile(it).w_itlo_c(nW)-tile(j).tbasex;
      tile(it).w_ithi_l(nW)=tile(it).w_ithi_c(nW)-tile(j).tbasex;
      tile(it).w_jtlo_c(nW)=tile(j).tbasey+tile(j).tny+1;
      tile(it).w_jthi_c(nW)=tile(j).tbasey+tile(j).tny+1;
      tile(it).w_jtlo_l(nW)=tile(it).w_jtlo_c(nW)-tile(j).tbasey;
      tile(it).w_jthi_l(nW)=tile(it).w_jthi_c(nW)-tile(j).tbasey;
      Pi=[ 0  1];                     % Pi,Pj,oj[_f],oi are the rules for mapping north face 
                                      % target domain canonical coordinate to west face
      Pj=[-1  0];                     % face source domain canonical coord.
      oj=  domain(myD).dny+1;         % oj is the offset rule for cell-centered quantities
      oj_f=domain(myD).dny+2;         % oj_f is the offset rule for face quantities.
      oi=-domain(wD).dny;             % oi is the offset rule for both cell-centered and 
                                      % face quantities.
      tile(it).w_pi(nW,:)=Pi;
      tile(it).w_pj(nW,:)=Pj;
      tile(it).w_oj(nW)=oj;
      tile(it).w_oi(nW)=oi;
      tile(it).w_oj_f(nW)=oj_f;
      tile(it).w_oi_f(nW)=oi;
      tile(it).w_jshi_c(nW) = tile(it).w_pj(nW,1)*tile(it).w_ithi_c(nW) ...
                             +tile(it).w_pj(nW,2)*tile(it).w_jthi_c(nW) ...
                             +tile(it).w_oj(nW);
      tile(it).w_jslo_c(nW) = tile(it).w_pj(nW,1)*tile(it).w_itlo_c(nW) ...
                             +tile(it).w_pj(nW,2)*tile(it).w_jtlo_c(nW) ...
                             +tile(it).w_oj(nW);
      tile(it).w_js_step(nW)= tile(it).w_pj(nW,1)*1 ...
                             +tile(it).w_pj(nW,2)*1;
      tile(it).w_jslo_l(nW) = tile(it).w_jslo_c(nW)-tile(it).tbasey;
      tile(it).w_jshi_l(nW) = tile(it).w_jshi_c(nW)-tile(it).tbasey;
      tile(it).w_ishi_c(nW) = tile(it).w_pi(nW,1)*tile(it).w_ithi_c(nW) ...
                             +tile(it).w_pi(nW,2)*tile(it).w_jthi_c(nW) ...
                             +tile(it).w_oi(nW);
      tile(it).w_islo_c(nW) = tile(it).w_pi(nW,1)*tile(it).w_itlo_c(nW) ...
                             +tile(it).w_pi(nW,2)*tile(it).w_jtlo_c(nW) ...
                             +tile(it).w_oi(nW);
      tile(it).w_is_step(nW)= tile(it).w_pi(nW,1)*1 ...
                             +tile(it).w_pi(nW,2)*1;
      tile(it).w_islo_l(nW) = tile(it).w_islo_c(nW)-tile(it).tbasex;
      tile(it).w_ishi_l(nW) = tile(it).w_ishi_c(nW)-tile(it).tbasex;
     end
    end
   elseif rFace == 'e'
%   A w <-> e interface (no rotation required)
%   Find the tile east face(s) in the domain to my west that abut the trial line/plane.
%   Trial plane/line is along east edge with sequencing unchanged.
    ryLo_tc=ryLo_sc;         % For the trial line/plane normal to the exchange direction the
    ryHi_tc=ryHi_sc;         % source and target bounds are the same when there is no 
                             % face rotation.
    rxHi_tc=domain(wD).dnx;  % This is the boundary point for cell-centered and face quantities.
    nW=0;
    for j=domain(wD).tileidlo:domain(wD).tileidhi
     if  tile(j).tnx+tile(j).tbasex == rxHi_tc ...
       & 1 + tile(j).tbasey         <= ryHi_tc ...
       & tile(j).tny+tile(j).tbasey >= ryLo_tc
      nW = nW+1;
      tile(it).nW=nW;
      tile(it).wTile(nW)=j;
      tile(it).w_jtlo_c(nW)=max(1 + tile(j).tbasey,ryLo_tc)-1;
      tile(it).w_jthi_c(nW)=min(tile(j).tny+tile(j).tbasey,ryHi_tc)+1;
      tile(it).w_jtlo_l(nW)=tile(it).w_jtlo_c(nW)-tile(j).tbasey;
      tile(it).w_jthi_l(nW)=tile(it).w_jthi_c(nW)-tile(j).tbasey;
      tile(it).w_itlo_c(nW)=tile(j).tbasex+tile(j).tnx+1;
      tile(it).w_ithi_c(nW)=tile(j).tbasex+tile(j).tnx+1;
      tile(it).w_itlo_l(nW)=tile(it).w_itlo_c(nW)-tile(j).tbasex;
      tile(it).w_ithi_l(nW)=tile(it).w_ithi_c(nW)-tile(j).tbasex;
      Pi=[ 1  0];             % Permutation is identity for a non-rotated exchange
      Pj=[ 0  1];           
      oj=0;                   % Offset of line/plane normal to exchange direction is
                              % always zero for non-rotated exchange.
      oj_f=oj;
      oi  =-domain(wD).dnx;   % Canonical index in target is offset by the size of the domain
                              % in the exchange direction for both cell-centered and face
      oi_f=-domain(wD).dnx;   % quantities in a non-rotated exchange.
      tile(it).w_pi(nW,:)=Pi;
      tile(it).w_pj(nW,:)=Pj;
      tile(it).w_oj(nW)=oj;
      tile(it).w_oj_f(nW)=oj_f;
      tile(it).w_oi(nW)=oi;
      tile(it).w_oi_f(nW)=oi_f;
      tile(it).w_jshi_c(nW) = tile(it).w_pj(nW,1)*tile(it).w_ithi_c(nW) ...
                             +tile(it).w_pj(nW,2)*tile(it).w_jthi_c(nW) ...
                             +tile(it).w_oj(nW);
      tile(it).w_jslo_c(nW) = tile(it).w_pj(nW,1)*tile(it).w_itlo_c(nW) ...
                             +tile(it).w_pj(nW,2)*tile(it).w_jtlo_c(nW) ...
                             +tile(it).w_oj(nW);
      tile(it).w_js_step(nW)= tile(it).w_pj(nW,1)*1 ...
                             +tile(it).w_pj(nW,2)*1;
      tile(it).w_jslo_l(nW) = tile(it).w_jslo_c(nW)-tile(it).tbasey;
      tile(it).w_jshi_l(nW) = tile(it).w_jshi_c(nW)-tile(it).tbasey;
      tile(it).w_ishi_c(nW) = tile(it).w_pi(nW,1)*tile(it).w_ithi_c(nW) ...
                             +tile(it).w_pi(nW,2)*tile(it).w_jthi_c(nW) ...
                             +tile(it).w_oi(nW);
      tile(it).w_islo_c(nW) = tile(it).w_pi(nW,1)*tile(it).w_itlo_c(nW) ...
                             +tile(it).w_pi(nW,2)*tile(it).w_jtlo_c(nW) ...
                             +tile(it).w_oi(nW);
      tile(it).w_is_step(nW)= tile(it).w_pi(nW,1)*1 ...
                             +tile(it).w_pi(nW,2)*1;
      tile(it).w_islo_l(nW) = tile(it).w_islo_c(nW)-tile(it).tbasex;
      tile(it).w_ishi_l(nW) = tile(it).w_ishi_c(nW)-tile(it).tbasex;
     end
    end
   else
    % Error as only w<->n and e<->w should happen.
    printf('Error west face should only connect to east or north face in cs6\n');
   end
  end
% Now do tile to the north
  nD  = tile(it).nDomain;
  if ( nD ~= myD )
   tile(it).isNedge=1;
%  Trial plane/line is along the northern edge. Set its canonical coordinate 
%  bounds
   rxLo_sc=tile(it).tbasex+1;
   rxHi_sc=tile(it).tbasex+tile(it).tnx;
   ryHi_sc=tile(it).tbasey+tile(it).tny;
   rFace = domain(myD).nFace;
   if     rFace == 'w'
%   A n <-> w interface (rotation is required)
%   Find the tile west face(s) in the domain to my north that abut the 
%   trial line/plane.
%   Trial plane/line is along west edge with sequencing permuted and reversed 
%   horizontally.
    ryLo_tc=-rxHi_sc+domain(myD).dnx+1;
    ryHi_tc=-rxLo_sc+domain(myD).dnx+1;
    rxHi_tc=1;
    nN=0;
    for j=domain(nD).tileidlo:domain(nD).tileidhi
     if  1+tile(j).tbasex           == rxHi_tc ...
       & 1 + tile(j).tbasey         <= ryHi_tc ...
       & tile(j).tny+tile(j).tbasey >= ryLo_tc
      nN=nN+1;
      tile(it).nN=nN;
      tile(it).nTile(nN)=j;
      tile(it).n_jtlo_c(nN)=max(1 + tile(j).tbasey,ryLo_tc)-1;
      tile(it).n_jthi_c(nN)=min(tile(j).tny+tile(j).tbasey,ryHi_tc)+1;
      tile(it).n_jtlo_l(nN)=tile(it).n_jtlo_c(nN)-tile(j).tbasey;
      tile(it).n_jthi_l(nN)=tile(it).n_jthi_c(nN)-tile(j).tbasey;
      tile(it).n_itlo_c(nN)=tile(j).tbasex;
      tile(it).n_ithi_c(nN)=tile(j).tbasex;
      tile(it).n_itlo_l(nN)=tile(it).n_itlo_c(nN)-tile(j).tbasex;
      tile(it).n_ithi_l(nN)=tile(it).n_ithi_c(nN)-tile(j).tbasex;
      Pj=[ 1  0];                     % Pi,Pj,oj[_f],oi are the rules for 
                                      % mapping west face target domain 
                                      % canonical coordinate to north face
      Pi=[ 0 -1];                     % face source domain canonical coord.
      oj=  domain(myD).dny;           % oj is the offset rule for cell-centered 
                                      % quantities
      oj_f=oj;                        % oj_f is the offset rule for face 
                                      % quantities.
      oi=  domain(myD).dnx+1;         % oi is the offset rule for cell-centered 
                                      % quantities
      oi_f=domain(myD).dnx+2;         % oi_f is the offset rule for face 
                                      % quantities
      % Save the canonical coordinate offset and permutation operators
      tile(it).n_pi(nN,:)=Pi;
      tile(it).n_pj(nN,:)=Pj;
      tile(it).n_oj(nN)=oj;
      tile(it).n_oj_f(nN)=oj_f;
      tile(it).n_oi(nN)=oi;
      tile(it).n_oi_f(nN)=oi_f;
      % Apply the canonical coordinate offset and permutation operators
      tile(it).n_jshi_c(nN) = tile(it).n_pj(nN,1)*tile(it).n_ithi_c(nN) ...
                             +tile(it).n_pj(nN,2)*tile(it).n_jthi_c(nN) ...
                             +tile(it).n_oj(nN);
      tile(it).n_jslo_c(nN) = tile(it).n_pj(nN,1)*tile(it).n_itlo_c(nN) ...
                             +tile(it).n_pj(nN,2)*tile(it).n_jtlo_c(nN) ...
                             +tile(it).n_oj(nN);
      tile(it).n_js_step(nN)= tile(it).n_pj(nN,1)*1 ...
                             +tile(it).n_pj(nN,2)*1;
      tile(it).n_jslo_l(nN) = tile(it).n_jslo_c(nN)-tile(it).tbasey;
      tile(it).n_jshi_l(nN) = tile(it).n_jshi_c(nN)-tile(it).tbasey;

      tile(it).n_ishi_c(nN) = tile(it).n_pi(nN,1)*tile(it).n_ithi_c(nN) ...
                             +tile(it).n_pi(nN,2)*tile(it).n_jthi_c(nN) ...
                             +tile(it).n_oi(nN);
      tile(it).n_islo_c(nN) = tile(it).n_pi(nN,1)*tile(it).n_itlo_c(nN) ...
                             +tile(it).n_pi(nN,2)*tile(it).n_jtlo_c(nN) ...
                             +tile(it).n_oi(nN);
      tile(it).n_is_step(nN)= tile(it).n_pi(nN,1)*1 ...
                             +tile(it).n_pi(nN,2)*1;
      tile(it).n_islo_l(nN) = tile(it).n_islo_c(nN)-tile(it).tbasex;
      tile(it).n_ishi_l(nN) = tile(it).n_ishi_c(nN)-tile(it).tbasex;
     end
    end
   elseif rFace == 's'
%   A n <-> s interface (no rotation required)
%   Find the tile south face(s) in the domain to my north that abut the 
%   trial line/plane.
%   Trial plane/line is along south edge with source and target sequencing 
%   and indexing aligned for the non-rotated case.
%   Define trial line/plane along southern edge in target canonical coords
    rxLo_tc=rxLo_sc;
    rxHi_tc=rxHi_sc;
    ryHi_tc=1;
    nN=0;
    for j=domain(nD).tileidlo:domain(nD).tileidhi
     if  1+tile(j).tbasey           == ryHi_tc ...
       & 1 + tile(j).tbasex         <= rxHi_tc ...
       & tile(j).tnx+tile(j).tbasex >= rxLo_tc
      nN=nN+1;
      tile(it).nN=nN;
      tile(it).nTile(nN)=j;
      tile(it).n_itlo_c(nN)=max(1 + tile(j).tbasex,rxLo_tc)-1;
      tile(it).n_ithi_c(nN)=min(tile(j).tnx+tile(j).tbasex,rxHi_tc)+1;
      tile(it).n_itlo_l(nN)=tile(it).n_itlo_c(nN)-tile(j).tbasex;
      tile(it).n_ithi_l(nN)=tile(it).n_ithi_c(nN)-tile(j).tbasex;
      tile(it).n_jtlo_c(nN)=tile(j).tbasey;
      tile(it).n_jthi_c(nN)=tile(j).tbasey;
      tile(it).n_jtlo_l(nN)=tile(it).n_jtlo_c(nN)-tile(j).tbasey;
      tile(it).n_jthi_l(nN)=tile(it).n_jthi_c(nN)-tile(j).tbasey;
      Pi=[ 1  0];                     % Pi,Pj,oj[_f],oi are the rules for 
                                      % mapping west face target domain 
                                      % canonical coordinate to north face
      Pj=[ 0  1];                     % face source domain canonical coord.
      oj=domain(myD).dny;             % oj is the offset rule for cell-centered
                                      % quantities.
      oj_f=oj;                        % oj_f is the offset rule for face 
                                      % quantities.
      oi=  0;                         % oi is the offset rule for cell-centered 
                                      % quantities
      oi_f=0;                         % oi_f is the offset rule for face 
                                      % quantities
      % Save the canonical coordinate offset and permutation operators
      tile(it).n_pi(nN,:)=Pi;
      tile(it).n_pj(nN,:)=Pj;
      tile(it).n_oj(nN)=oj;
      tile(it).n_oj_f(nN)=oj_f;
      tile(it).n_oi(nN)=oi;
      tile(it).n_oi_f(nN)=oi_f;
      % Apply the canonical coordinate offset and permutation operators
      tile(it).n_jshi_c(nN) = tile(it).n_pj(nN,1)*tile(it).n_ithi_c(nN) ...
                             +tile(it).n_pj(nN,2)*tile(it).n_jthi_c(nN) ...
                             +tile(it).n_oj(nN);
      tile(it).n_jslo_c(nN) = tile(it).n_pj(nN,1)*tile(it).n_itlo_c(nN) ...
                             +tile(it).n_pj(nN,2)*tile(it).n_jtlo_c(nN) ...
                             +tile(it).n_oj(nN);
      tile(it).n_js_step(nN)= tile(it).n_pj(nN,1)*1 ...
                             +tile(it).n_pj(nN,2)*1;
      tile(it).n_jslo_l(nN) = tile(it).n_jslo_c(nN)-tile(it).tbasey;
      tile(it).n_jshi_l(nN) = tile(it).n_jshi_c(nN)-tile(it).tbasey;
 
      tile(it).n_ishi_c(nN) = tile(it).n_pi(nN,1)*tile(it).n_ithi_c(nN) ...
                             +tile(it).n_pi(nN,2)*tile(it).n_jthi_c(nN) ...
                             +tile(it).n_oi(nN);
      tile(it).n_islo_c(nN) = tile(it).n_pi(nN,1)*tile(it).n_itlo_c(nN) ...
                             +tile(it).n_pi(nN,2)*tile(it).n_jtlo_c(nN) ...
                             +tile(it).n_oi(nN);
      tile(it).n_is_step(nN)= tile(it).n_pi(nN,1)*1 ...
                             +tile(it).n_pi(nN,2)*1;
      tile(it).n_islo_l(nN) = tile(it).n_islo_c(nN)-tile(it).tbasex;
      tile(it).n_ishi_l(nN) = tile(it).n_ishi_c(nN)-tile(it).tbasex;
     end
    end
   else
    % Error as only w<->n and n<->s should happen.
    printf(...
    'Error north face should only connect to south face or west face in cs6\n');
   end
  end
% Now do tile to the east
  eD  = tile(it).eDomain;
  if ( eD ~= myD )
   tile(it).isEedge=1;
%  Trial plane/line is along the eastern edge. Set its canonical coordinate 
%  bounds
   ryLo_sc=tile(it).tbasey+1;
   ryHi_sc=tile(it).tbasey+tile(it).tny;
   rxHi_sc=tile(it).tbasex+tile(it).tnx;
   rFace = domain(myD).nFace;
   if         rFace == 's'
%   Define trial line/plane along southern edge in target canonical coords
%   e -> s rotation needed
    rxLo_tc=-ryHi_sc+domain(eD).dnx+1;
    rxHi_tc=-ryLo_sc+domain(eD).dnx+1;
    ryHi_tc=1;
    nE=0;
    for j=domain(eD).tileidlo:domain(eD).tileidhi
     if  1+tile(j).tbasey           == ryHi_tc ...
       & 1 + tile(j).tbasex         <= rxHi_tc ...
       & tile(j).tnx+tile(j).tbasex >= rxLo_tc
      nE=nE+1;
      tile(it).nE=nE; 
      tile(it).eTile(nE)=j; 
      tile(it).e_itlo_c(nE)=max(1 + tile(j).tbasex,rxLo_tc)-1;
      tile(it).e_ithi_c(nE)=min(tile(j).tnx+tile(j).tbasex,rxHi_tc)+1;
      tile(it).e_itlo_l(nE)=tile(it).e_itlo_c(nE)-tile(j).tbasex;
      tile(it).e_ithi_l(nE)=tile(it).e_ithi_c(nE)-tile(j).tbasex;
      tile(it).e_jtlo_c(nE)=0;
      tile(it).e_jthi_c(nE)=0;
      tile(it).e_jtlo_l(nE)=tile(it).e_jtlo_c(nE)-tile(j).tbasey;
      tile(it).e_jthi_l(nE)=tile(it).e_jthi_c(nE)-tile(j).tbasey;
%     Set (src e,targ s) canonical coordinate permutation matrix + offset vector
      Pi=[ 0  1];                     % Pi,Pj,oj[_f],oi are the rules for 
                                      % mapping west face target domain 
                                      % canonical coordinate to north face
      Pj=[-1  0];                     % face source domain canonical coord.
      oi=  domain(myD).dnx;           % oi is the offset rule for cell-centered 
                                      % quantities
      oi_f=oi;                        % oi_f is the offset rule for face 
                                      % quantities.
      oj=  domain(myD).dny+1;         % oj is the offset rule for cell-centered 
                                      % quantities
      oj_f=domain(myD).dny+2;         % oj_f is the offset rule for face 
                                      % quantities
      % Save the canonical coordinate offset and permutation operators
      tile(it).e_pi(nE,:)=Pi;
      tile(it).e_pj(nE,:)=Pj;
      tile(it).e_oj(nE)=oj;
      tile(it).e_oj_f(nE)=oj_f;
      tile(it).e_oi(nE)=oi;
      tile(it).e_oi_f(nE)=oi_f;
      % Apply the canonical coordinate offset and permutation operators
      tile(it).e_jshi_c(nE) = tile(it).e_pj(nE,1)*tile(it).e_ithi_c(nE) ...
                             +tile(it).e_pj(nE,2)*tile(it).e_jthi_c(nE) ...
                             +tile(it).e_oj(nE);
      tile(it).e_jslo_c(nE) = tile(it).e_pj(nE,1)*tile(it).e_itlo_c(nE) ...
                             +tile(it).e_pj(nE,2)*tile(it).e_jtlo_c(nE) ...
                             +tile(it).e_oj(nE);
      tile(it).e_js_step(nE)= tile(it).e_pj(nE,1)*1 ...
                             +tile(it).e_pj(nE,2)*1;
      tile(it).e_jslo_l(nE) = tile(it).e_jslo_c(nE)-tile(it).tbasey;
      tile(it).e_jshi_l(nE) = tile(it).e_jshi_c(nE)-tile(it).tbasey;
 
      tile(it).e_ishi_c(nE) = tile(it).e_pi(nE,1)*tile(it).e_ithi_c(nE) ...
                             +tile(it).e_pi(nE,2)*tile(it).e_jthi_c(nE) ...
                             +tile(it).e_oi(nE);
      tile(it).e_islo_c(nE) = tile(it).e_pi(nE,1)*tile(it).e_itlo_c(nE) ...
                             +tile(it).e_pi(nE,2)*tile(it).e_jtlo_c(nE) ...
                             +tile(it).e_oi(nE);
      tile(it).e_is_step(nE)= tile(it).e_pi(nE,1)*1 ...
                             +tile(it).e_pi(nE,2)*1;
      tile(it).e_islo_l(nE) = tile(it).e_islo_c(nE)-tile(it).tbasex;
      tile(it).e_ishi_l(nE) = tile(it).e_ishi_c(nE)-tile(it).tbasex;
     end
    end
   elseif     rFace == 'w'
%   Define trial line/plane along western edge in target canonical coords
%   e -> w shift needed
    ryLo_tc=ryLo_sc;
    ryHi_tc=ryHi_sc;
    rxHi_tc=1;
    nE=0;
    for j=domain(eD).tileidlo:domain(eD).tileidhi
     if  1+tile(j).tbasex           == rxHi_tc ...
       & 1 + tile(j).tbasey         <= ryHi_tc ...
       & tile(j).tny+tile(j).tbasey >= ryLo_tc
      nE=nE+1;
      tile(it).nE=nE; 
      tile(it).eTile(nE)=j; 
      tile(it).e_itlo_c(nE)=0;
      tile(it).e_ithi_c(nE)=0;
      tile(it).e_itlo_l(nE)=tile(it).e_itlo_c(nE)-tile(j).tbasex;
      tile(it).e_ithi_l(nE)=tile(it).e_ithi_c(nE)-tile(j).tbasex;
      tile(it).e_jtlo_c(nE)=max(1 + tile(j).tbasey,ryLo_tc)-1;
      tile(it).e_jthi_c(nE)=min(tile(j).tny+tile(j).tbasey,ryHi_tc)+1;
      tile(it).e_jtlo_l(nE)=tile(it).e_jtlo_c(nE)-tile(j).tbasey;
      tile(it).e_jthi_l(nE)=tile(it).e_jthi_c(nE)-tile(j).tbasey;
%     Set (src e,targ w) canonical coordinate permutation matrix + offset vector
      Pi=[ 1  0];                     % Pi,Pj,oj[_f],oi are the rules for 
                                      % mapping west face target domain 
                                      % canonical coordinate to north face
      Pj=[ 0  1];                     % face source domain canonical coord.
      oi=  domain(myD).dnx;           % oi is the offset rule for cell-centered 
                                      % quantities
      oi_f=oi;                        % oi_f is the offset rule for face 
                                      % quantities.
      oj=0;                           % oj is the offset rule for cell-centered 
                                      % quantities
      oj_f=0;                         % oj_f is the offset rule for face 
                                      % quantities
      % Save the canonical coordinate offset and permutation operators
      tile(it).e_pi(nE,:)=Pi;
      tile(it).e_pj(nE,:)=Pj;
      tile(it).e_oj(nE)=oj;
      tile(it).e_oj_f(nE)=oj_f;
      tile(it).e_oi(nE)=oi;
      tile(it).e_oi_f(nE)=oi_f;
      % Apply the canonical coordinate offset and permutation operators
      tile(it).e_jshi_c(nE) = tile(it).e_pj(nE,1)*tile(it).e_ithi_c(nE) ...
                             +tile(it).e_pj(nE,2)*tile(it).e_jthi_c(nE) ...
                             +tile(it).e_oj(nE);
      tile(it).e_jslo_c(nE) = tile(it).e_pj(nE,1)*tile(it).e_itlo_c(nE) ...
                             +tile(it).e_pj(nE,2)*tile(it).e_jtlo_c(nE) ...
                             +tile(it).e_oj(nE);
      tile(it).e_js_step(nE)= tile(it).e_pj(nE,1)*1 ...
                             +tile(it).e_pj(nE,2)*1;
      tile(it).e_jslo_l(nE) = tile(it).e_jslo_c(nE)-tile(it).tbasey;
      tile(it).e_jshi_l(nE) = tile(it).e_jshi_c(nE)-tile(it).tbasey;
 
      tile(it).e_ishi_c(nE) = tile(it).e_pi(nE,1)*tile(it).e_ithi_c(nE) ...
                             +tile(it).e_pi(nE,2)*tile(it).e_jthi_c(nE) ...
                             +tile(it).e_oi(nE);
      tile(it).e_islo_c(nE) = tile(it).e_pi(nE,1)*tile(it).e_itlo_c(nE) ...
                             +tile(it).e_pi(nE,2)*tile(it).e_jtlo_c(nE) ...
                             +tile(it).e_oi(nE);
      tile(it).e_is_step(nE)= tile(it).e_pi(nE,1)*1 ...
                             +tile(it).e_pi(nE,2)*1;
      tile(it).e_islo_l(nE) = tile(it).e_islo_c(nE)-tile(it).tbasex;
      tile(it).e_ishi_l(nE) = tile(it).e_ishi_c(nE)-tile(it).tbasex;
     end
    end
   else
    % Error as only e<->w and e<->s should happen.
    fprintf(...
    'Error east face should only connect to south face or west face in cs6\n');
   end
  end
% Now do tile to the south
  sD  = tile(it).sDomain;
  if ( sD ~= myD )
   tile(it).isSedge=1;
%  Trial plane/line is along the southern edge. Set its canonical coordinate 
%  bounds
   rxLo_sc=tile(it).tbasex+1;
   rxHi_sc=tile(it).tbasex+tile(it).tnx;
   ryHi_sc=1;
   rFace = domain(myD).sFace;
   if         rFace == 'e'
%   Define trial line/plane along eastern edge in target canonical coords
%   s -> e rotation needed
    ryLo_tc=-rxHi_sc+domain(sD).dny+1;
    ryHi_tc=-rxLo_sc+domain(sD).dny+1;
    rxHi_tc=domain(sD).dnx;
    nS=0;
    for j=domain(sD).tileidlo:domain(sD).tileidhi
     if  tile(j).tnx+tile(j).tbasex == rxHi_tc ...
       & 1 + tile(j).tbasey         <= ryHi_tc ...
       & tile(j).tny+tile(j).tbasey >= ryLo_tc
      nS=nS+1;
      tile(it).nS=nS; 
      tile(it).sTile(nS)=j; 
      tile(it).s_itlo_c(nS)=domain(sD).dnx+1;
      tile(it).s_ithi_c(nS)=domain(sD).dnx+1;
      tile(it).s_itlo_l(nS)=tile(it).s_itlo_c(nS)-tile(j).tbasex;
      tile(it).s_ithi_l(nS)=tile(it).s_ithi_c(nS)-tile(j).tbasex;
      tile(it).s_jtlo_c(nS)=max(1 + tile(j).tbasey,ryLo_tc)-1;
      tile(it).s_jthi_c(nS)=min(tile(j).tny+tile(j).tbasey,ryHi_tc)+1;
      tile(it).s_jtlo_l(nS)=tile(it).s_jtlo_c(nS)-tile(j).tbasey;
      tile(it).s_jthi_l(nS)=tile(it).s_jthi_c(nS)-tile(j).tbasey;
      % Set (src s,targ e) canonical coordinate permutation matrix + offset vector
      Pi=[ 0 -1];                     % Pi,Pj,oj[_f],oi are the rules for 
                                      % mapping west face target domain 
                                      % canonical coordinate to north face
      Pj=[ 1  0];                     % face source domain canonical coord.
      oi=  domain(sD).dny+1;          % oi is the offset rule for cell-centered 
                                      % quantities
      oi_f=domain(sD).dny+2;          % oi_f is the offset rule for face 
                                      % quantities.
      oj=-domain(sD).dnx;             % oj is the offset rule for cell-centered 
                                      % quantities
      oj_f=-domain(sD).dnx;           % oj_f is the offset rule for face 
                                      % quantities
      % Save the canonical coordinate offset and permutation operators
      tile(it).s_pi(nS,:)=Pi;
      tile(it).s_pj(nS,:)=Pj;
      tile(it).s_oj(nS)=oj;
      tile(it).s_oj_f(nS)=oj_f;
      tile(it).s_oi(nS)=oi;
      tile(it).s_oi_f(nS)=oi_f;
      % Apply the canonical coordinate offset and permutation operators
      tile(it).s_jshi_c(nS) = tile(it).s_pj(nS,1)*tile(it).s_ithi_c(nS) ...
                             +tile(it).s_pj(nS,2)*tile(it).s_jthi_c(nS) ...
                             +tile(it).s_oj(nS);
      tile(it).s_jslo_c(nS) = tile(it).s_pj(nS,1)*tile(it).s_itlo_c(nS) ...
                             +tile(it).s_pj(nS,2)*tile(it).s_jtlo_c(nS) ...
                             +tile(it).s_oj(nS);
      tile(it).s_js_step(nS)= tile(it).s_pj(nS,1)*1 ...
                             +tile(it).s_pj(nS,2)*1;
      tile(it).s_jslo_l(nS) = tile(it).s_jslo_c(nS)-tile(it).tbasey;
      tile(it).s_jshi_l(nS) = tile(it).s_jshi_c(nS)-tile(it).tbasey;
 
      tile(it).s_ishi_c(nS) = tile(it).s_pi(nS,1)*tile(it).s_ithi_c(nS) ...
                             +tile(it).s_pi(nS,2)*tile(it).s_jthi_c(nS) ...
                             +tile(it).s_oi(nS);
      tile(it).s_islo_c(nS) = tile(it).s_pi(nS,1)*tile(it).s_itlo_c(nS) ...
                             +tile(it).s_pi(nS,2)*tile(it).s_jtlo_c(nS) ...
                             +tile(it).s_oi(nS);
      tile(it).s_is_step(nS)= tile(it).s_pi(nS,1)*1 ...
                             +tile(it).s_pi(nS,2)*1;
      tile(it).s_islo_l(nS) = tile(it).s_islo_c(nS)-tile(it).tbasex;
      tile(it).s_ishi_l(nS) = tile(it).s_ishi_c(nS)-tile(it).tbasex;
     end
    end
   elseif     rFace == 'n'
%   Define trial line/plane along northern edge in target canonical coords
%   s -> n shift needed
    rxLo_tc=rxLo_sc;
    rxHi_tc=rxHi_sc;
    ryHi_tc=domain(sD).dny;
    nS=0;
    for j=domain(sD).tileidlo:domain(sD).tileidhi
     if  tile(j).tny+tile(j).tbasey == ryHi_tc ...
       & 1 + tile(j).tbasex         <= rxHi_tc ...
       & tile(j).tnx+tile(j).tbasex >= rxLo_tc
      nS=nS+1;
      tile(it).nS=nS; 
      tile(it).sTile(nS)=j; 
      tile(it).s_itlo_c(nS)=max(1 + tile(j).tbasex,rxLo_tc)-1;
      tile(it).s_ithi_c(nS)=min(tile(j).tnx+tile(j).tbasex,rxHi_tc)+1;
      tile(it).s_itlo_l(nS)=tile(it).s_itlo_c(nS)-tile(j).tbasex;
      tile(it).s_ithi_l(nS)=tile(it).s_ithi_c(nS)-tile(j).tbasex;
      tile(it).s_jtlo_c(nS)=domain(sD).dny+1;
      tile(it).s_jthi_c(nS)=domain(sD).dny+1;
      tile(it).s_jtlo_l(nS)=tile(it).s_jtlo_c(nS)-tile(j).tbasey;
      tile(it).s_jthi_l(nS)=tile(it).s_jthi_c(nS)-tile(j).tbasey;
      % Set (src s,targ n) canonical coordinate permutation matrix + offset vector
      Pi=[ 1  0];                     % Pi,Pj,oj[_f],oi are the rules for 
                                      % mapping west face target domain 
                                      % canonical coordinate to north face
      Pj=[ 0  1];                     % face source domain canonical coord.
      oi=0;                           % oi is the offset rule for cell-centered 
                                      % quantities
      oi_f=0;                         % oi_f is the offset rule for face 
                                      % quantities.
      oj=-domain(sD).dny;             % oj is the offset rule for cell-centered 
                                      % quantities
      oj_f=-domain(sD).dny;           % oj_f is the offset rule for face 
                                      % quantities
      % Save the canonical coordinate offset and permutation operators
      tile(it).s_pi(nS,:)=Pi;
      tile(it).s_pj(nS,:)=Pj;
      tile(it).s_oj(nS)=oj;
      tile(it).s_oj_f(nS)=oj_f;
      tile(it).s_oi(nS)=oi;
      tile(it).s_oi_f(nS)=oi_f;
      % Apply the canonical coordinate offset and permutation operators
      tile(it).s_jshi_c(nS) = tile(it).s_pj(nS,1)*tile(it).s_ithi_c(nS) ...
                             +tile(it).s_pj(nS,2)*tile(it).s_jthi_c(nS) ...
                             +tile(it).s_oj(nS);
      tile(it).s_jslo_c(nS) = tile(it).s_pj(nS,1)*tile(it).s_itlo_c(nS) ...
                             +tile(it).s_pj(nS,2)*tile(it).s_jtlo_c(nS) ...
                             +tile(it).s_oj(nS);
      tile(it).s_js_step(nS)= tile(it).s_pj(nS,1)*1 ...
                             +tile(it).s_pj(nS,2)*1;
      tile(it).s_jslo_l(nS) = tile(it).s_jslo_c(nS)-tile(it).tbasey;
      tile(it).s_jshi_l(nS) = tile(it).s_jshi_c(nS)-tile(it).tbasey;
 
      tile(it).s_ishi_c(nS) = tile(it).s_pi(nS,1)*tile(it).s_ithi_c(nS) ...
                             +tile(it).s_pi(nS,2)*tile(it).s_jthi_c(nS) ...
                             +tile(it).s_oi(nS);
      tile(it).s_islo_c(nS) = tile(it).s_pi(nS,1)*tile(it).s_itlo_c(nS) ...
                             +tile(it).s_pi(nS,2)*tile(it).s_jtlo_c(nS) ...
                             +tile(it).s_oi(nS);
      tile(it).s_is_step(nS)= tile(it).s_pi(nS,1)*1 ...
                             +tile(it).s_pi(nS,2)*1;
      tile(it).s_islo_l(nS) = tile(it).s_islo_c(nS)-tile(it).tbasex;
      tile(it).s_ishi_l(nS) = tile(it).s_ishi_c(nS)-tile(it).tbasex;
     end
    end
   else
    % Error as only e<->w and e<->s should happen.
    fprintf(...
    'Error east face should only connect to south face or west face in cs6\n');
   end
  end
end
% and what transformations to apply to the canonical index
% Should include figuring out the neighbor tiles here? There could be more than
% one for a face. How do figure this out?
return
