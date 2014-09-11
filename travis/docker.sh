wget https://get.docker.io/builds/Linux/x86_64/docker-latest -O /tmp/docker
chmod +x /tmp/docker
/tmp/docker -H unix:///tmp/docker.sock build -t docker .
/tmp/docker -H unix:///tmp/docker.sock build -t time-attack .
