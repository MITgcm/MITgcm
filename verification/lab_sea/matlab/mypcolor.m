function h=mypcolor(x,y,z)
% mypcolor(x,y,z)     simulate pcolor using image (much faster)

if nargin == 1
  h=image(x,'CDataMapping','scaled');
elseif nargin ==3
  dx=diff(x);
  dy=diff(y);
  if min(dx)==max(dx) & min(dy)==max(dy)
    h=image(x,y,z,'CDataMapping','scaled');
  else
    dx=min(x):min(abs(dx)):max(x);
    dy=min(y):min(abs(dy)):max(y);
    dz=interp2(x,y,z,dx,dy');
    h=image(dx,dy,dz,'CDataMapping','scaled');
  end
else
  error('wrong number of arguments')
end 
set(gca,'ydir','normal')
