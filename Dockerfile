FROM wonderbeat/yesod
MAINTAINER Denis Golovachev <borov.htid@gmail.com>

COPY time-attack.cabal /
RUN apt-get install -qy libpq-dev
RUN cabal install --only-dependencies
RUN rm time-attack.cabal
