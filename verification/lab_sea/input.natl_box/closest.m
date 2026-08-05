function y=closest(v,x,n)
% function CLOSEST(V,X,N)
% Return index of N element closest to value V in vector X.
% By default N=1.
% If N is zero, CLOSEST returns the two closest
%    values in vector X that straddle value V.
%
% See also MINMAX MMAX MMIN MMEAN

% d menemenlis 8/21/95

if nargin < 3, n=1;  end
if nargin < 2, error('insufficient number of arguments'); end

if n<0
  error('N cannot be a negative number')
elseif n==0
  y=[nan nan];
  ix=find(~isnan(x)&x<v);
  if ~isempty(ix)
    [my iy]=min(abs(x(ix)-v));
    y(1)=ix(iy);
  end
  ix=find(~isnan(x)&x>=v);
  if ~isempty(ix)
    [my iy]=min(abs(x(ix)-v));
    y(2)=ix(iy);
  end
else
  y=nan*ones(n,1);
  for i=1:n
    ix=find(~isnan(x));
    if ~isempty(ix)
      [my iy]=min(abs(x(ix)-v));
      y(i)=ix(iy);
      x(y(i))=nan;
    end
  end
end
