{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module TypedValidator where

import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins   as Builtins

import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts --Plutus.Script.Utils.V1.Scripts
import           Ledger.Ada          as Ada

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))

import           Plutus.Contract

import           Control.Monad       hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           GHC.Generics         (Generic)   
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--ON-CHAIN

{-# INLINABLE justRedeemer #-}
justRedeemer :: Integer -> Integer -> ScriptContext -> Bool
justRedeemer datum redeemer _ = traceIfFalse "Wrong redeemer!" (redeemer == datum)

data Typed                                            -- New type that encode the information about the Datum and the Redeemer
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = Integer                -- Type instances to define the type of Datum
    type instance RedeemerType Typed = Integer             -- Type instance to definte the type of Redeemer

typedValidator :: Scripts.TypedValidator Typed             -- typedValidator = TypedValidator {.. .. ..}
typedValidator = Scripts.mkTypedValidator @Typed 
                 $$(PlutusTx.compile [|| justRedeemer ||])
                 $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @Integer @Integer     -- mkUntypedValidator

validator :: Validator                                    -- validator = Validator {<script>}
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

--THE OFF-CHAIN

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab

data GiveParams = GP { datum :: Integer
                     , amount :: Integer} deriving (Generic, ToJSON, FromJSON, ToSchema)

type GiftSchema =
            Endpoint "give" GiveParams  
        .\/ Endpoint "grab" Integer


give :: AsContractError e => GiveParams -> Contract w a e ()
give (GP datum amount) = do
                    let tx = Constraints.mustPayToTheScript datum $ Ada.lovelaceValueOf amount
                    ledgerTx <- submitTxConstraints typedValidator tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
                    logInfo @String $ printf "made a gift of %d lovelace" amount 

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                     

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies []









