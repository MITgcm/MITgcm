function y = meanovernan(x,dim)

xsize = size(x);
xflat = reshape(x,[prod(xsize),1]);
nanindex = isnan(xflat);
xflat(nanindex) = 0;
nanindex = reshape(~nanindex,xsize);
xtemp = reshape(xflat,xsize);
warning off MATLAB:divideByZero
y = sum(xtemp,dim)./sum(nanindex,dim);
warning on MATLAB:divideByZero