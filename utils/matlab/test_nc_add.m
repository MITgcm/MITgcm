
xi=-180:4:180;
yi=-90:4:90;
[X,Y]=ndgrid(xi,yi);
q=sin(X*pi/180).*cos(Y*pi/180);

nc=netcdf('test.nc','clobber');

nc_add(nc,'lon','Longitude','degrees east',{'lon'},xi)
nc_add(nc,'lat','Latitude','degrees north',{'lat'},yi)
nc_add(nc,'time','Time','Years',{'time'},[])
nc_add(nc,'q','Random','nothing',{'lat' 'lon'},q)

nc_add(nc,'time','Time','Years',{'time'},1977,1)
nc_add(nc,'time','Time','Years',{'time'},1979,2)
nc_add(nc,'w','Random','nothing',{'time' 'lat' 'lon'},q+1,1)
nc_add(nc,'w','Random','nothing',{'time' 'lat' 'lon'},q+2,2)

nc=close(nc);
