FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y openjdk-11-jdk
RUN apt-get install -y gcc
RUN apt-get install -y gfortran
RUN apt-get install -y git-all
RUN apt-get install -y wget
RUN apt-get install -y tar
RUN apt-get install -y make
RUN apt-get install -y libc6
RUN apt-get install -y bison

RUN wget https://gitlab.inria.fr/tapenade/tapenade/-/archive/e89660a5921a1ccf68b4afc331b3b0910878a179/tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179.tar.gz
RUN tar -xzvf tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179.tar.gz
RUN cd tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179; ./gradlew

ENV PATH=$PATH:/tapenade-e89660a5921a1ccf68b4afc331b3b0910878a179/bin
