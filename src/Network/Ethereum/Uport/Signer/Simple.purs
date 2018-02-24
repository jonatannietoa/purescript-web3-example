module Network.Ethereum.Uport.Signer.Simple 
  ( PrivateKey
  , mkPrivateKey
  , simpleSigner
  ) where

import Control.Monad.Eff.Exception (EXCEPTION)
import Network.Ethereum.Uport.Signer (RawSigner, Signer, fromRawSigner)
import Prelude ((<<<))

-- | Type containing private key used passed to `SimpleSigner` function
-- | from `uport` module.
newtype PrivateKey = PrivateKey String

-- | Constructs `PrivateKey`
mkPrivateKey :: String -> PrivateKey
mkPrivateKey = PrivateKey

-- | Creates Signer from from PrivateKey using `SimpleSigner` function
-- | from `uport` module.
simpleSigner :: forall eff. PrivateKey -> Signer (exception :: EXCEPTION |eff)
simpleSigner = fromRawSigner <<< _simpleSigner

foreign import _simpleSigner :: forall eff. PrivateKey -> RawSigner (exception :: EXCEPTION |eff)
