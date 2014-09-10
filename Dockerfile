FROM wonderbeat/yesod
MAINTAINER Denis Golovachev <borov.htid@gmail.com>

RUN apt-get install -qy libpq-dev
RUN cabal install yesod-auth==1.2.* yesod-static==1.2.* yesod-form==1.3.* persistent==1.* persistent-postgresql==1.* persistent-sqlite==1.* shakespeare==2.* warp==3.* fast-logger==2.*
