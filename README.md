shoes-app
=========

Installation
------------

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal install
```

Run
---

```
$ .cabal-sandbox/bin/shoes Development
```

Server is reachable at http://localhost:3000

RESTful API
-----------

```
POST /shoes (lib/Shoes.hs:postNewShoesR)
-- Create a shoes entry using json data supplied as request body
-- { "description": {shoes description :: String}
-- , "color"      : {shoes color :: String}
-- , "size"       : {shoes size :: String}
-- , "photo"      : {shoes photo. Base64 encoded :: String}
-- }

GET /shoes/{shoes-id :: Number} (lib/Shoes.hs:getShoesR)
-- Get a shoe as an HTML page listing the shoe details
-- HTML:
-- <div>
--     <img src={shoes photo location} />
--     <div>#{shoes description}</div>
--     <div>#{shoes color}</div>
--     <div>#{shoes size}</div>
-- </div>

GET /shoes_list (lib/Shoes.hs:getShoesListR)
-- Get a list of shoes as an HTML page with hyperlinks to all available shoes
-- HTML:
-- <a href={shoes location}>{shoes id}</a>
--                    .
--                    .
--                    .
```

Database
--------

Server uses a SQLite backend which is automatically created and initialized at first run

Photo directory
---------------

Server uses a directory to store shoes photos. This setting is in config/app.cfg. Make sure that directory
exists before starting Server. (Default photo dir is ./repo/)
