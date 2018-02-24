module Network.Ethereum.Uport.Signer where

import Prelude

import Control.Monad.Aff (Aff, Error, runAff_)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, mkEffFn2, runEffFn2)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Unsafe.Coerce (unsafeCoerce)

-- | Signing function which will be used to sign JWT's in the credentials object.
-- | It could be constracted out of `mkSigner` or more low level function `fromRawSigner`.
foreign import data Signer :: # Effect -> Type

-- | Input to signing function
newtype SigningInput = SigningInput String

-- | Result of signing function.
newtype Signature = Signature String

-- | Takes a `Aff` based signing function and retusn `Signer`.
mkSigner :: forall eff. (SigningInput -> Aff eff Signature) -> Eff eff (Signer eff)
mkSigner affSigner = pure $ fromRawSigner rawSigner
  where
    rawSigner :: RawSigner eff
    rawSigner = mkEffFn2 rawSignerUncurried

    rawSignerUncurried :: SigningInput -> SignerCallback eff -> Eff eff Unit
    rawSignerUncurried signingInput signerCallback = runAff_ affHandler (affSigner signingInput)
      where
        affHandler :: Either Error Signature -> Eff eff Unit
        affHandler affRes = 
          let handle = runEffFn2 signerCallback
          in case affRes of
            Left err -> handle (toNullable $ Just err) (toNullable Nothing)
            Right signature -> handle (toNullable Nothing) (toNullable $ Just signature)


-- | Low level representation of `Signer`
type RawSigner eff = EffFn2 eff SigningInput (SignerCallback eff) Unit

type SignerCallback eff = EffFn2 eff (Nullable Error) (Nullable Signature) Unit

-- Creates `Signer` out of `RawSigner`
fromRawSigner :: forall eff. RawSigner eff -> Signer eff
fromRawSigner = unsafeCoerce

