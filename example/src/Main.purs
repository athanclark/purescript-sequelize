module Main where

import Data.URI (Host (..))
import Database.Sequelize

import Prelude
import Data.Symbol (SProxy (..))
import Data.Maybe (Maybe (..))
import Data.JSDate (JSDate)
import Data.Argonaut (encodeJson)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow, logShow)
import Unsafe.Coerce (unsafeCoerce)


type Fields =
  { foo :: String
  , time :: JSDate
  }

main :: Eff _ Unit
main = do
  log "Hello sailor!"
  let args =
        { database: "athan"
        , username: "athan"
        , password: "foo"
        , host: NameAddress "localhost"
        , dialect: Postgres
        , pool: {min: 0, max: 5, idle: 10000}
        }
  sql <- sequelize args
  void $ runAff errorShow log $ do
    authenticate sql
    foo <- liftEff $ define sql "foo"
             $ addField (SProxy :: SProxy "foo")
                { "type": sqlSTRING
                }
             $ addFieldWithDefault (SProxy :: SProxy "time")
                { "type": sqlDATE
                } sqlNOW
             $ addField (SProxy :: SProxy "ayy")
                { "type": sqlJSON
                }
             $ emptyModelDefinition
    baz <- liftEff $ define sql "baz"
             $ addFieldWithDefault (SProxy :: SProxy "baz")
                { "type": sqlSTRING
                } "baz!"
             $ emptyModelDefinition
    foo'sBazs <- (baz `belongsToMany` foo) {through: "BazFoo"}
    sync sql
    void $ create foo {foo: "ayy", ayy: encodeJson [1]}
    b <- create baz {}
    mI <- findOne foo {where: {foo: "ayy"}}
    case mI of
      Nothing -> do
        liftEff $ log "wut?!"
        pure "wut"
      Just i -> do
        foo'sBazs.add i [b] {through: {}}
        x <- liftEff $ get i {plain: true}
        pure $ unsafeCoerce x
