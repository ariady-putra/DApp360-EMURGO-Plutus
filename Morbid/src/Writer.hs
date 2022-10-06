module Writer
( writeValidator
, writeDatumRedeemer
)
where

{- ~/.local/share/applications/cabal-repl-morbid.desktop
[Desktop Entry]
Categories=Application;
Exec=/home/ariady/cabal-repl-morbid.sh
GenericName=cabal-repl-mmorbid
Icon=com.visualstudio.code
Name=Cabal REPL Morbid
Terminal=true
Type=Application
-}

{- ~/cabal-repl-morbid.sh
#!/bin/bash
cd ~/cardano/plutus-apps/DApp360-EMURGO-Plutus
sh cabal-repl-morbid.sh
-}

{- ~/cardano/plutus-apps/DApp360-EMURGO-Plutus/cabal-repl-morbid.sh
#!/bin/bash
HERE=$(pwd)
cd ~/plutus-apps
nix-shell --command "code $HERE/Morbid"
-}

{- cabal repl
0> :l Writer
1> writeValidator
-- Run cdp to generate the Plutus script address file
2> writeDatumRedeemer
-- Send Script Tx on cdp (localhost:41506)
-}

import Cardano.Api         qualified as API
import Cardano.Api.Shelley qualified as Shelley
import Codec.Serialise     qualified as Codec

import Data.Aeson            qualified as Aeson
import Data.ByteString.Lazy  qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Time.Clock.POSIX qualified as POSIX

import Ledger   qualified
import PlutusTx qualified
import Morbid   qualified

type FError  = Shelley.FileError ()
type EFError = Either FError ()

convertDataToScriptData :: PlutusTx.Data -> Shelley.ScriptData
convertDataToScriptData (PlutusTx.Constr i dt) = Shelley.ScriptDataConstructor i $ convertDataToScriptData <$> dt
convertDataToScriptData (PlutusTx.Map      dt) = Shelley.ScriptDataMap  [(convertDataToScriptData  k, convertDataToScriptData  v) | (k, v) <- dt]
convertDataToScriptData (PlutusTx.List     dt) = Shelley.ScriptDataList $ convertDataToScriptData <$> dt
convertDataToScriptData (PlutusTx.I         i) = Shelley.ScriptDataNumber i
convertDataToScriptData (PlutusTx.B         s) = Shelley.ScriptDataBytes  s

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file
    . Aeson.encode
    . Shelley.scriptDataToJson Shelley.ScriptDataJsonDetailedSchema
    . convertDataToScriptData
    . PlutusTx.toData

writeDatum :: IO ()
writeDatum = do
    now <- (*1_000) . truncate <$> POSIX.getPOSIXTime
    {- pkh =
        cardano-cli address key-hash --payment-verification-key-file wallet.vkey
    -}
    let pkh      = Ledger.PubKeyHash "7d4617ed7bcf517dfeb3a444cddd46fd3831ac9dad305af6da5eaa20"
        creator  = Ledger.PaymentPubKeyHash pkh
        password = Morbid.hashString "password"
        deadline = now + fromInteger 1_000_000 -- 1000 minutes
        datum    = Morbid.ChestDatum deadline creator password
    writeJSON "morbid.plutus.datum" datum

writeRedeemer :: IO ()
writeRedeemer = do
    {- pkh =
        cardano-cli address key-hash --payment-verification-key-file wallet.vkey
    -}
    let pkh      = Ledger.PubKeyHash "7d4617ed7bcf517dfeb3a444cddd46fd3831ac9dad305af6da5eaa20"
        redeemer = Ledger.PaymentPubKeyHash pkh
        password = Morbid.hashString "password"
    writeJSON "morbid.plutus.redeemer1createChest" $ Morbid.ActionCreateChest
    writeJSON "morbid.plutus.redeemer2addTreasure" $ Morbid.ActionAddTreasure
    writeJSON "morbid.plutus.redeemer3delayUnlock" $ Morbid.ActionDelayUnlock redeemer password
    writeJSON "morbid.plutus.redeemer4unlockChest" $ Morbid.ActionUnlockChest

writeDatumRedeemer :: IO ()
writeDatumRedeemer = writeDatum >> writeRedeemer

writeValidator :: IO EFError
writeValidator = Shelley.writeFileTextEnvelope
    @(Shelley.PlutusScript API.PlutusScriptV1) "morbid.plutus" Nothing
    . Shelley.PlutusScriptSerialised
    . SBS.toShort
    . LBS.toStrict
    . Codec.serialise
    . Ledger.unValidatorScript
    $ Morbid.contractValidator
