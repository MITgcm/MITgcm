function b=compact2map(a,flag,rgbDim);
%  function b=compact2map(a,[flag,rgbDim]);
  % convert between mdsio compact format and a sensible 2d map for llc fields;
  % unless specified explicitly (optional argument rgbDim), try to guess 
  %  (assuming regular Lat-Lon cap) what the 3 dimensions of faces are.
  % a: input field (2d, 3d, or 4d)
  % b: output field
  % flag == 0 (default): convert from compact format to 2d map
  % flag == 1          : convert from 2d map to compact format 
  
  % author: Martin Losch (Martin.Losch@awi.de)
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
    dims=size(a); ndim=length(dims);
    if ndim == 2, dims=[dims 1]; end; n3d=prod(dims(3:end));
    nPts=prod(dims(1:2)); a=reshape(a,[nPts n3d]); 
    if nargin < 3,
      nx=dims(1); m=dims(2);
      if m/nx~=round(m/nx);
        error('not a compact format llc field');
      end
      ntiles = m/nx;
      if mod(ntiles,2); % odd number of tiles
        tilesPerFace = (ntiles-1)/4;
      else
        tilesPerFace = (ntiles-2)/4;
      end
      ng=tilesPerFace*nx;
      nr=nx; nb=nx; Nfaces=5;
    elseif length(rgbDim) ~= 3,
      error('3 elements array expected as 3rd argument (rgbDim)');
    else
      nr=rgbDim(1); ng=rgbDim(2); nb=rgbDim(3);
    end
    %-- set dimensions of each of the 6 faces:
    nf=ones(6,2);
    nf(1,:)=[nr ng]; nf(2,:)=[nb ng]; nf(3,:)=[nb nr];
    nf(4,:)=[ng nr]; nf(5,:)=[ng nb]; nf(6,:)=[nr nb];
    fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;
    if nargin == 3,
      [Nfaces]=find(fd2==nPts);
      if length(Nfaces) ~= 1,
        error('array size does not match 5 or 6 faces compact array dim');
      end
    end
    %-- set output 2-D map dimensions:
    n2=nr*2+nb*2;
    j1=0; if Nfaces==6, j1=max(nr,nb); end
    j2=j1+ng; m2=j2+max(nr,nb);
    b=zeros(n2,m2,n3d);
    
    for kz = 1:n3d
     i1=1; i2=0;
     for n=1:Nfaces,
      i1=i2+1;

    %-- extract face "n" data from compact array "a", level kz:
      vf1=reshape(a(fd1(n):fd2(n),kz),[nf(n,:)]);
    %-- put face data into a "nice" 2-D map:
      if n < 3,
      %- face 1 & 2 : strait copy
        i2=i2+nf(n,1);
        b(i1:i2,j1+1:j2,kz)=vf1;
      elseif n == 3,
      %- face 3 is copied 4 times (in continuation of each equatorial faces)
        ii1=1; ii2=nr;
        for k=1:4,
          if rem(k,2) == 0, mj=nr; else mj=nb ; end
          if k == 2,
            b(ii1:ii2,j2+1:j2+mj,kz)=vf1;
          else
            b(ii1:ii2,j2+1:j2+mj,kz)=rot90(vf1,2-k);
          end
          ii1=ii2+1;
          ii2=ii2+mj;
        end
      elseif n == 6,
      %- face 6 is copied 4 times (in continuation of each equatorial faces)
        ii1=1; ii2=nr;
        for k=1:4,
          if rem(k,2) == 0, mj=nr; else mj=nb ; end
          if k == 1,
            b(ii1:ii2,j1-mj+1:j1,kz)=vf1;
          else
            b(ii1:ii2,j1-mj+1:j1,kz)=rot90(vf1,k-1);
          end
          ii1=ii2+1;
          ii2=ii2+mj;
        end
      else
      %- face 4 & 5 : rotate & copy
        i2=i2+nf(n,2);
        b(i1:i2,j1+1:j2,kz)=rot90(vf1,-1);
      end

     end
    end
    if ndim  > 3, b=reshape(b,[n2 m2 dims(3:end)]);
    elseif ndim < 3, b=squeeze(b); end

  end
  
  return
