module Writer where

import Cardano.Api         qualified as API
import Cardano.Api.Shelley qualified as Shelley
import Codec.Serialise     qualified as Codec

import Data.ByteString.Lazy  qualified as LBS
import Data.ByteString.Short qualified as SBS

import Ledger qualified

import Morbid qualified

type FError  = Shelley.FileError ()
type EFError = Either FError ()

writeValidator :: IO EFError
writeValidator = Shelley.writeFileTextEnvelope
    @(Shelley.PlutusScript API.PlutusScriptV1) "morbid.plutus" Nothing
    . Shelley.PlutusScriptSerialised
    . SBS.toShort
    . LBS.toStrict
    . Codec.serialise
    . Ledger.unValidatorScript
    $ Morbid.contractValidator
