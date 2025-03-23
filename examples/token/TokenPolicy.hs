-- | Модуль определяет политику минтинга токена.
module TokenPolicy where

import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Value as Value
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude

-- | Параметры токена
data TokenParams = TokenParams
    { tokenName :: !BuiltinByteString
    , tokenSymbol :: !BuiltinByteString
    , tokenDecimals :: !Integer
    , tokenSupply :: !Integer
    }

PlutusTx.makeIsDataIndexed ''TokenParams [('TokenParams, 0)]

-- | Сценарий политики минтинга токена
tokenPolicy :: TokenParams -> Scripts.MintingPolicy
tokenPolicy params = Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \p -> Scripts.wrapMintingPolicy (mkPolicy p) ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params

-- | Реализация политики минтинга
mkPolicy :: TokenParams -> Scripts.MintingPolicy
mkPolicy params ctx = do
    -- Проверка, что количество минтинга не превышает максимальное количество токенов
    let mintedAmount = Contexts.valueProduced ctx
        tokenName' = tokenName params
        tokenSymbol' = tokenSymbol params
    if Value.assetClassValueOf mintedAmount (Value.assetClass tokenSymbol' tokenName') <= tokenSupply params
    then () -- Успешно
    else error "Exceeds maximum token supply"

-- | Получение хеша политики минтинга
tokenPolicyHash :: TokenParams -> Scripts.MintingPolicyHash
tokenPolicyHash = Scripts.mintingPolicyHash . tokenPolicy

-- | Создание нового токена с заданными параметрами
newToken :: BuiltinByteString -> BuiltinByteString -> Integer -> Integer -> TokenParams
newToken name symbol decimals supply = TokenParams
    { tokenName = name
    , tokenSymbol = symbol
    , tokenDecimals = decimals
    , tokenSupply = supply
    }