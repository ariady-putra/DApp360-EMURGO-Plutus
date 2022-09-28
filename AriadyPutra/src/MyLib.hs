module MyLib where

import Control.Monad (void)

import Data.ByteString.Char8 qualified as B8
import Data.Map              qualified as M
import Data.Maybe            (catMaybes)

import Ledger               qualified
import Ledger               (from, before)
import Ledger               (txInfoValidRange)
import Ledger.Constraints   (mustIncludeDatum)
import Ledger.Constraints   (mustPayToTheScript)
import Ledger.Constraints   (mustSpendScriptOutput)
import Ledger.Constraints   (mustValidateIn)
import Ledger.Constraints   (otherScript)
import Ledger.Constraints   (typedValidatorLookups)
import Ledger.Constraints   (unspentOutputs)
import Ledger.Contexts      (ScriptContext (..))
import Ledger.Tx            (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value         (Value)

import Playground.Contract
import Plutus.Contract
import PlutusTx                   qualified
import PlutusTx.Builtins.Internal (BuiltinInteger)
import PlutusTx.Prelude
import Prelude                    qualified as Haskell

------------------------------------------------------------ DATATYPE DECLARATIONS ------------------------------------------------------------

data GameData
    = GameData
    { _gamePolicyID :: BuiltinByteString
    , _gameTitle    :: BuiltinByteString
    , _gamePrice    :: BuiltinInteger -- PlutusTx.Builtins.Internal
    }
    deriving Show
PlutusTx.unstableMakeIsData ''GameData
PlutusTx.makeLift ''GameData

data PublisherAction
    = MintGame
    { _mintGame :: GameData
    }
    | SetPrice
    { _oldGameData :: GameData
    , _newGameData :: GameData
    }
    | BurnGame
    { _existingGame :: GameData
    , _willBurnGame :: GameData
    }
    deriving Show
PlutusTx.unstableMakeIsData ''PublisherAction
PlutusTx.makeLift ''PublisherAction

data UserMarket
    = PrimaryMarket
    { _gamePrimary :: GameData
    }
    | SecondaryMarket
    { _gameSecondary :: GameData
    }
    deriving Show
PlutusTx.unstableMakeIsData ''UserMarket
PlutusTx.makeLift ''UserMarket

-- | Datum parameters
data MarketDatum
    = PlatformDatum
    { _platformPKH :: Ledger.PaymentPubKeyHash
    }
    | PublisherDatum
    { _publisherPKH    :: Ledger.PaymentPubKeyHash
    , _publisherAction :: PublisherAction
    }
    | UserDatum
    { _userPKH    :: Ledger.PaymentPubKeyHash
    , _userMarket :: UserMarket
    }
    deriving Show
PlutusTx.unstableMakeIsData ''MarketDatum

-- | Redeemer parameters
data MarketRedeemer
    = PlatformRedeemer
    { _platformRedeemPKH :: Ledger.PaymentPubKeyHash
    }
    | PublisherRedeemer
    { _publisherRedeemPKH    :: Ledger.PaymentPubKeyHash
    , _publisherRedeemAction :: PublisherAction
    }
    | UserRedeemer
    { _userRedeemPKH      :: Ledger.PaymentPubKeyHash
    , _redeemerUserMarket :: UserMarket
    }
    deriving Show
PlutusTx.unstableMakeIsData ''MarketRedeemer

------------------------------------------------------------ ON-CHAIN ------------------------------------------------------------

{-# INLINABLE validate #-}
validate :: MarketDatum -> MarketRedeemer -> ScriptContext -> Bool

-- Primary Market
-- TODO: Minting on-demand instead if possible
-- validate (PublisherDatum publisher (MintGame game)) undefined                 ctx = False -- Publisher mints game
validate undefined (PublisherRedeemer publisher (SetPrice old new))           ctx = False -- Publisher changes game price
validate undefined (PublisherRedeemer publisher (BurnGame existing willburn)) ctx = False -- Publisher burns game

validate undefined (UserRedeemer user (PrimaryMarket game))                   ctx = False -- User buys game
validate (UserDatum user (PrimaryMarket game)) undefined                      ctx = False -- User refunds game

validate undefined (PlatformRedeemer platform)                                ctx = False -- Platform processed refund (approve or reject, maybe off-chain instead?)

-- Secondary Market
validate (UserDatum user (SecondaryMarket game)) undefined                    ctx = False -- User puts game-pass-NFT for sale
validate undefined (UserRedeemer user (SecondaryMarket game))                 ctx = False -- User buys game-pass-NFT

-- any other scenarios are rejected
validate _ _ _ = False

