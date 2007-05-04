function b=compact2map(a,flag);
%  function b=compact2map(a,flag);
  % convert between mdsio compact format and a sensible 2d map for llc
  % fields
  % a: input field (2d, 3d, or 4d)
  % b: output field
  % flag == 0 (default): convert from compact format to 2d map
  % flag == 1          : convert from 2d map to compact format 
  
  % author: Martin Losch (Martin.Losch@awi.de)
  % $Name:  $
  % $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/latloncap/compact2map.m,v 1.1 2007/05/04 15:34:26 mlosch Exp $

  
  if nargin < 2
    flag = 0;
  end
  if flag
    % convert from "nice" to compact format
    [nx,ny,nz,nt]=size(a);
    if nx/4~=round(nx/4);
      error('not a llc field');
    end

    n4=nx/4;
    m=(ny-n4)/n4;
    m4=m*n4;
    for kt = 1:nt
      for kz = 1:nz
	for k=1:4
	  sides{k} = a([1:n4]+(k-1)*n4,1:m4,kz,kt);
	end
%	cap = a([1:n4]+1*n4,m4+1:end,kz,kt);
	cap = rot90(a([1:n4],m4+1:end,kz,kt),-1);
	% reformat
	btmp = [sides{1} ...
		sides{2} ...
		cap ...
		reshape(rot90(sides{3},1),[n4 m4]) ...
		reshape(rot90(sides{4},1),[n4 m4]) ...
		cap*0];
	b(:,:,kz,kt)=btmp;
      end
    end
  else
    % convert from compact to "nice" format
    [nx,m,nz,nt]=size(a);
    if m/nx~=round(m/nx);
      error('not a compact format llc field');
    end

    fac = 0;
    ntiles = m/nx;
    if mod(ntiles,2); % odd number of tiles
      tilesPerFace = (ntiles-1)/4;
    else
      tilesPerFace = (ntiles-2)/4;
    end
    n4=tilesPerFace*nx;
    for kt = 1:nt
      for kz = 1:nz
	for k=1:4
	  jj = [1:n4]+(k-1)*n4;
	  if k>2; jj = jj + nx; end
	  sides{k} = a(:,jj,kz,kt);
	end
	cap = rot90(a(:,[1:nx]+2*n4,kz,kt),1);
	% reformat
	btmp = [sides{1}; ...
		sides{2}; ...
		rot90(reshape(sides{3}(:),[n4 nx]),-1); ...
		rot90(reshape(sides{4}(:),[n4 nx]),-1) ];
	b(:,:,kz,kt)=[btmp ...
		      [cap; rot90(cap,-1)*abs(fac); ...
		       rot90(cap,-2)*abs(fac); ...
		       rot90(cap,-3)*abs(fac)]];
      end
    end
  end
  
  return
