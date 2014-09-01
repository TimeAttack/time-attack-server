time-attack-server
==================

# Install
* `cabal update`  
* `cabal install yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals`
* `cabal sandbox init`
* `cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals`
## Postgress
* `docker run -d -p 127.0.0.1:5432:5432 -e POSTGRESQL_USER=timeattack -e POSTGRESQL_PASS=timeattack -e POSTGRESQL_DB=timeattack wyaeld/postgres`
* 
### Mac only
* `sudo port install postgresql94`
* `boot2docker ssh -L localhost:5432:localhost:5432`
