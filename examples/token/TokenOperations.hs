-- | Модуль определяет операции с токеном
module TokenOperations where

import qualified Plutus.V1.Ledger.Value as Value
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Tx as Tx
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import TokenPolicy

-- | Минтинг токенов
mintTokens :: TokenParams -> Integer -> Contexts.ScriptContext -> Tx.Tx
mintTokens params amount ctx = 
    let policyHash = tokenPolicyHash params
        assetClass = Value.assetClass (tokenSymbol params) (tokenName params)
        value = Value.assetClassValue assetClass amount
    in Constraints.mustMintValueWithRedeemer ctx () value policyHash

-- | Сжигание токенов
burnTokens :: TokenParams -> Integer -> Contexts.ScriptContext -> Tx.Tx
burnTokens params amount ctx =
    let policyHash = tokenPolicyHash params
        assetClass = Value.assetClass (tokenSymbol params) (tokenName params)
        value = Value.assetClassValue assetClass (negate amount)
    in Constraints.mustMintValueWithRedeemer ctx () value policyHash

-- | Передача токенов
transferTokens :: TokenParams -> Contexts.PubKeyHash -> Integer -> Contexts.ScriptContext -> Tx.Tx
transferTokens params recipient amount ctx =
    let assetClass = Value.assetClass (tokenSymbol params) (tokenName params)
        value = Value.assetClassValue assetClass amount
    in Constraints.mustPayToPubKey recipient value

-- | Проверка баланса токенов
checkTokenBalance :: TokenParams -> Contexts.PubKeyHash -> Contexts.ScriptContext -> Integer
checkTokenBalance params owner ctx =
    let assetClass = Value.assetClass (tokenSymbol params) (tokenName params)
        inputs = Contexts.txInfoInputs (Contexts.scriptContextTxInfo ctx)
        ownedValues = [v | i <- inputs, 
                        Just (Contexts.PubKeyCredential ownerHash) <- [Contexts.txOutAddress (Contexts.txInInfoResolved i)],
                        ownerHash == owner,
                        let v = Contexts.txOutValue (Contexts.txInInfoResolved i)]
    in foldr ((+) . Value.assetClassValueOf assetClass) 0 ownedValues