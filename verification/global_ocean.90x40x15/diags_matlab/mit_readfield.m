function h = mit_readfield(fname,dims,accuracy)
%function h = mit_readfield(fname,dims,accuracy)
  
  ieee='ieee-be';

  [fid message] = fopen(fname,'r',ieee);
  if fid <= 0
    error([message ', filename: ', [fname]])
  end

  v = fread(fid,accuracy);
  if length(v) == prod(dims)
    h = reshape(v,dims);
  elseif length(v) == prod(dims(1:end-1))
    h = reshape(v,dims(1:end-1));
  else
    error('dimensions do not match')
  end
  fclose(fid);
  
  return
