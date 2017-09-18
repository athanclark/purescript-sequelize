module Main where

import Data.URI (Host (..))
import Database.Sequelize (sequelize, Dialect (Postgres), authenticate)

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow, logShow)

main :: Eff _ Unit
main = do
  log "Hello sailor!"
  sql <- sequelize
    { database: "athan"
    , username: "athan"
    , password: "foo"
    , host: NameAddress "localhost"
    , dialect: Postgres
    , pool: {min: 0, max: 5, idle: 10000}
    }
  void $ runAff errorShow logShow $ do
    authenticate sql
    liftEff $ log "Yayuh!"
