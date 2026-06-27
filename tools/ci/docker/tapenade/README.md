Dockerfile that contains gfortran, java runtime, and the tapenade application. Needs > 1g memory for halfpipe streamice experiment

# Update image on docker hub for CI

Rebuild from Dockerfile in this directory

```
docker build --no-cache --tag mitgcm/tapenade-ubuntu:latest .
```

If successfull:

```
docker push mitgcm/tapenade-ubuntu:latest
```

Note: this does not work on MIT VPN.
