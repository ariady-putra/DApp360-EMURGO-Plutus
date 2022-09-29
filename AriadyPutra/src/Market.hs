module Market where

import Control.Monad (void)

import Data.ByteString.Char8 qualified as B8
import Data.Map              qualified as M
import Data.Maybe            (catMaybes)

import Ledger               qualified
import Ledger               (from, to)
import Ledger               (before, after)
import Ledger               (contains)
import Ledger               (getCardanoTxId)
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
import Ledger.Value         (CurrencySymbol, Value)
import Ledger.Value         qualified
import Ledger.Ada           qualified as Ada

import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude  qualified as Haskell
------------------------------------------------------------ DATATYPE DECLARATIONS ------------------------------------------------------------

data GameData
    = GameData
    { _franchise :: CurrencySymbol
    , _gameTitle :: TokenName
    , _gamePrice :: Value
    }
    deriving Show
PlutusTx.makeIsDataIndexed ''GameData
    [ ('GameData, 0)
    ]
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
PlutusTx.makeIsDataIndexed ''PublisherAction
    [ ('MintGame, 0)
    , ('SetPrice, 1)
    , ('BurnGame, 2)
    ] -- https://github.com/Plutonomicon/plutonomicon/blob/main/builtin-data.md#what-is-constr
PlutusTx.makeLift ''PublisherAction

data UserMarket
    = PrimaryMarket
    { _gamePrimary :: GameData
    }
    | SecondaryMarket
    { _gameSecondary :: GameData
    }
    deriving Show
PlutusTx.makeIsDataIndexed ''UserMarket
    [ ('PrimaryMarket  , 0)
    , ('SecondaryMarket, 1)
    ]
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
PlutusTx.makeIsDataIndexed ''MarketDatum
    [ ('PlatformDatum , 0)
    , ('PublisherDatum, 1)
    , ('UserDatum     , 2)
    ]

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
PlutusTx.makeIsDataIndexed ''MarketRedeemer
    [ ('PlatformRedeemer , 0)
    , ('PublisherRedeemer, 1)
    , ('UserRedeemer     , 2)
    ]
------------------------------------------------------------ ON-CHAIN ------------------------------------------------------------

{-# INLINABLE validate #-}
validate :: MarketDatum -> MarketRedeemer -> ScriptContext -> Bool

-- Primary Market
-- TODO: Minting on-demand instead if possible
validate (PublisherDatum publisher (MintGame game)) _                         ctx = True  -- Publisher mints game
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

data Market
instance Scripts.ValidatorTypes Market where
    type instance DatumType     Market = MarketDatum
    type instance RedeemerType  Market = MarketRedeemer

marketValidator :: Scripts.TypedValidator Market
marketValidator = Scripts.mkTypedValidator @Market
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [||   wrap   ||])
    where wrap = Scripts.wrapValidator
                @MarketDatum
                @MarketRedeemer
------------------------------------------------------------ LOGGING HELPERS ------------------------------------------------------------

-- | Log 2 Haskell.String as logDebug | logInfo | logWarn | logError
logAs :: (AsContractError x) =>
    (Haskell.String -> MarketContract x ()) -> Haskell.String -> Haskell.String ->
    MarketContract x ()
logAs doLog str = doLog . (str++)

-- | Log Haskell.String as logDebug | logInfo | logWarn | logError
logStrAs :: (AsContractError x) =>
    (Haskell.String -> MarketContract x ()) -> Haskell.String ->
    MarketContract x ()
logStrAs doLog = logAs doLog ""

-- | Log Haskell.String and Haskell.show s as logDebug | logInfo | logWarn | logError
logStrShowAs :: (Show s, AsContractError x) =>
    (Haskell.String -> MarketContract x ()) -> Haskell.String -> s ->
    MarketContract x ()
logStrShowAs doLog str = (logAs doLog str) . Haskell.show

-- | Log Haskell.show s as logDebug | logInfo | logWarn | logError
logShowAs :: (Show s, AsContractError x) =>
    (Haskell.String -> MarketContract x ()) -> s ->
    MarketContract x ()
logShowAs doLog = logStrShowAs doLog ""
------------------------------------------------------------ OFF-CHAIN ------------------------------------------------------------

type MarketContract = Contract () MarketSchema
type MarketPromise  = Promise  () MarketSchema

data PubMintGames
    = PubMintGames
    { _pubMintFranchise :: CurrencySymbol
    , _pubMintGameTitle :: TokenName
    , _pubMintGamePrice :: Value
    , _pubMintGameStock :: Integer
    }
    deriving (Generic, FromJSON, ToJSON, ToSchema)

min_req_utxo = 1_500_000 -- lovelace

pubMintGames :: (AsContractError x) => MarketPromise x ()
pubMintGames = endpoint @"Publisher: 1. Mint Games" $ \ params -> do
    pkh <- ownPaymentPubKeyHash
    let franchise = _pubMintFranchise params
        gameTitle = _pubMintGameTitle params
        gamePrice = _pubMintGamePrice params
        gameStock = _pubMintGameStock params
        gameDatum = GameData franchise gameTitle gamePrice
        action    = MintGame gameDatum
        you       = PublisherDatum pkh action
        nft       = Ledger.Value.singleton franchise gameTitle gameStock <> -- TODO: Should be NFT
                    Ada.lovelaceValueOf min_req_utxo
        txn       = you `mustPayToTheScript` nft
    cardanoTx <- submitTxConstraints marketValidator txn
    let txHash = getCardanoTxId cardanoTx
    void $ awaitTxConfirmed txHash
    logStrShowAs logInfo "Minted games txHash:" txHash

endpoints :: (AsContractError x) => MarketContract x ()
endpoints = selectList
            [ pubMintGames
            ]
type MarketSchema = Endpoint "Publisher: 1. Mint Games" PubMintGames
mkSchemaDefinitions ''MarketSchema

$(mkKnownCurrencies [])
