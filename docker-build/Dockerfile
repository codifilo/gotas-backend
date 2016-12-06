FROM fpco/stack-build:lts-7.11
MAINTAINER Agustin Prats <aprats@gmail.com>
ADD ./gotas /usr/bin/
EXPOSE 3000

# Instructions
# docker build -t gotas .
# docker tag gotas codifilo/gotas:1.1.0
# docker login
# docker push codifilo/gotas:1.1.0

# Server instruction
# docker stop ...
# docker pull codifilo/gotas:1.1.0
# docker run -d -p 3000:3000 codifilo/gotas:1.1.0 gotas
