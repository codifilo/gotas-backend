FROM fpco/stack-build:lts-7.11
MAINTAINER Agustin Prats <aprats@gmail.com>
#RUN apt-get update
ADD ./ /opt/gotas
WORKDIR /opt/gotas
RUN rm -rf .stack-work
RUN stack clean
RUN stack build

# Instructions
# docker rmi gotas-build -f
# docker rmi codifilo/gotas:1.1.0 -f
# docker stop gotas-build-cont
# docker rm gotas-build-cont
#
# docker build -t gotas-build .
# docker create --name gotas-build-cont gotas-build
# docker cp gotas-build-cont:/opt/gotas/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/gotas/gotas ./docker-build/
