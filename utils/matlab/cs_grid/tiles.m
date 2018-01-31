function [a] = tile(b,varargin)
% a=tile(b,n);
%
% Extract single tile from cubed array
%
% b can have dimensions (M*6,M,Nr) or (M,M,Nr,6)
%
% n can be vector of integers between 1 and 6
%
% Written by adcroft@.mit.edu, 2001.
if nargin==1
 n=1:6;
else
 n=varargin{1};
end

if min(n)<1 | max(n)>6
 disp(sprintf('n=',n));
 error('tile: second argument n is out of range');
end

if size(b,ndims(b))==6
 switch ndims(b)
  case 3,
   a=b(:,:,n);
  case 4,
   a=b(:,:,:,n);
  otherwise
   error('tile: it seems that b has too many dimensions');
 end
elseif size(b,2)==6
 m=size(b,1);
 k=1;
 for N=n;
 switch ndims(b)
  case 3,
   a(:,:,k)=squeeze(b(:,N,:));
  case 4,
   a(:,:,:,k)=squeeze(b(:,N,:,:));
  otherwise
   error('tile: it seems that b has too many dimensions');
 end
 k=k+1;
 end
elseif size(b,1)==size(b,2)*6
 m=size(b,2);
 k=1;
 for N=n;
 switch ndims(b)
  case 2,
   a(:,:,k)=b((N-1)*m+1:N*m,:);
  case 3,
   a(:,:,:,k)=b((N-1)*m+1:N*m,:,:);
  otherwise
   error('tile: it seems that b has too many dimensions');
 end
 k=k+1;
 end
else
 disp(sprintf('Size(b) = %i %i %i %i %i %i',size(b)));
 error('tile: Size of first argument is not consistent with cubed array');
end
