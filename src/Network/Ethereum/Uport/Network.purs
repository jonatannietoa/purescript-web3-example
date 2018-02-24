module Network.Ethereum.Uport.Network where

import Unsafe.Coerce (unsafeCoerce)

-- | Type representing Etherium Network on which all
-- | operations will be performed.
foreign import data Network :: Type

-- | Represents Network configuration object.
type NetworkR =
  { id :: String
  , registry :: String
  , rpcUrl :: String
  }

-- | Constructs Network configuration object
-- |
-- | ```purescript
-- | mkNetwork
-- |   { id: "0x1"
-- |   , registry: "0xab5c8051b9a1df1aab0149f8b0630848b7ecabf6"
-- |   , rpcUrl: "https://mainnet.infura.io"
-- |   }
-- | ```
mkNetwork :: NetworkR -> Network
mkNetwork = unsafeCoerce


-- | Predefined Network configuration value for "mainnet"
foreign import mainnet :: Network

-- | Predefined Network configuration value for "ropsten"
foreign import ropsten :: Network

-- | Predefined Network configuration value for "kovan"
foreign import kovan :: Network

-- | Predefined Network configuration value for "rinkeby"
foreign import rinkeby :: Network

