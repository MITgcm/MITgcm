FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y openjdk-11-jre
RUN apt-get install -y gcc
RUN apt-get install -y gfortran
RUN apt-get install -y git-all
RUN apt-get install -y wget
RUN apt-get install -y tar
RUN apt-get install -y make
RUN apt-get install -y libc6

RUN wget https://gitlab.inria.fr/tapenade/tapenade/-/archive/e89660a5921a1ccf68b4afc331b3b0910878a179/tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179.tar.gz
RUN tar -xzvf tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179.tar.gz

ENV PATH=$PATH:/tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179/bin
ENV _JAVA_OPTIONS=-Xmx1g
