module Network.Ethereum.Uport.Connect
  ( UPORT
  , Connect
  , AppName(..)
  , ClientId(..)
  , connect
  , getProvider
  , Options
  , withCredentials
  , Credentials
  , withAppName
  , withNetwork
  , OptionsBuilder
  , optionsDefaults
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Network.Ethereum.Uport.Network (Network)
import Network.Ethereum.Uport.Signer (Signer)
import Network.Ethereum.Web3 (Provider)

-- | This effect is used to annotate code which possibly uses Uport lib
-- | for signing etherium transactions.
foreign import data UPORT :: Effect

-- | An instance of `Connect` class from `uport-connect`
foreign import data Connect :: Type

-- | A Type used to create `Connect`
type Options eff =
  { appName :: Maybe AppName
  , signer :: Maybe (Signer eff)
  , clientId :: Maybe ClientId
  , network :: Maybe Network
  }


-- | Builder type for Options used to construct `Connect`
type OptionsBuilder eff = Options eff -> Options eff

-- | OptionsBuilder which could be used to create Connect using default options
optionsDefaults :: forall eff. OptionsBuilder eff
optionsDefaults = id

-- Given OptionsBuilder constructs new instance of `Connect` class from `uport-connect`
connect :: forall eff. OptionsBuilder eff -> Eff (uport :: UPORT, exception :: EXCEPTION |eff) Connect
connect b = 
  let
    o = b
      { appName: Nothing
      , signer: Nothing
      , clientId: Nothing
      , network: Nothing
      }
    
  in
    runEffFn2 _newConnect
      (toNullable o.appName)
      { signer: toNullable o.signer
      , clientId: toNullable o.clientId
      , network: toNullable o.network
      }

-- | Instantiates and returns a web3 styple provider wrapped with uPort functionality
getProvider :: forall eff. Connect -> Eff (uport :: UPORT | eff) Provider
getProvider = runEffFn1 _getProvider


newtype ClientId = ClientId String

-- | A type used for representing Uport application name
newtype AppName = AppName String

-- | A combinator used as part of OptionsBuilder, it set's Uport
-- | application name. Example:
-- |
-- | ```purescript
-- | connect
-- |   $ withAppName (AppName "MyApp")
-- |   >>> withNetwork rinkeby
-- | ```
withAppName :: forall eff. AppName -> Options eff -> Options eff
withAppName n = _ {appName = Just n}

-- | A combinator used as part of OptionsBuilder, it set's network
-- | on which `Connect` will operate. Example:
-- |
-- | ```purescript
-- | connect
-- |   $ withAppName (AppName "MyApp")
-- |   >>> withNetwork rinkeby
-- | ```
withNetwork :: forall eff. Network -> Options eff -> Options eff
withNetwork n = _ {network = Just n}


type Credentials eff = 
  { signer :: Signer eff
  , clientId :: ClientId
  , network :: Network
  }

-- | A combinator used as part of OptionsBuilder, it set's Uport
-- | application credentials (used when signed requests are requried).
-- | Example:
-- |
-- | ```purescript
-- | connect
-- |   $ withAppName (AppName "MyApp")
-- |   >>> withCredentials myAppCredentials
-- | ```
withCredentials :: forall eff. Credentials eff -> Options eff -> Options eff
withCredentials c = _
  { signer = Just c.signer
  , clientId = Just c.clientId
  , network = Just c.network
  }



-- NOTE:
-- * The api of uport connect for this is a bit strange,
--   you can pass `mobileUriHandler` and `uriHandler` but
--   there is only `closeUriHandler`, there is also `isMobile`
--   option which is not documented, so we can wait when the api
--   is more stable or when someone actually needs this level 
--   of configuration.
-- * `topicFactory` we don't need it yet
-- * Credentials is created based on `signer`, `clientId` and `network` anyway,
--   so there is no need for seperate `credentials` field (if we need we can introduce it later)
-- * `infuraApiKey` is not used in uport-connect all so we omit it
-- * `rpcUrl` is not used in uport-connect so we omit it
type OptionsR eff =
  { signer :: Nullable (Signer eff)
  , clientId :: Nullable ClientId
  , network :: Nullable Network
  }


foreign import _newConnect :: forall eff. EffFn2
  (uport :: UPORT, exception :: EXCEPTION |eff)
  (Nullable AppName)
  (OptionsR eff)
  Connect
foreign import _getProvider :: forall eff. EffFn1
  (uport :: UPORT | eff)
  Connect
  Provider
