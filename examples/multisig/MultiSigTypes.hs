-- | Модуль определяет типы данных для многоподписного контракта
module MultiSigTypes where

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as Scripts

-- | Структура для хранения информации о доверенных участниках
data MultiSigParams = MultiSigParams
    { requiredSignatures :: !Integer        -- ^ Количество требуемых подписей
    , trustedParties :: ![Contexts.PubKeyHash] -- ^ Список доверенных ключей
    , timeLimit :: !Integer               -- ^ Временное ограничение (в слотах)
    }

PlutusTx.makeIsDataIndexed ''MultiSigParams [('MultiSigParams, 0)]

-- | Действия, которые могут быть выполнены с контрактом
data MultiSigAction
    = Spend                 -- ^ Потратить средства
    | AddParty Contexts.PubKeyHash    -- ^ Добавить нового участника
    | RemoveParty Contexts.PubKeyHash -- ^ Удалить участника
    | UpdateRequiredSigs Integer -- ^ Изменить количество требуемых подписей
    | ExtendTimeLimit Integer    -- ^ Продлить временное ограничение

PlutusTx.makeIsDataIndexed ''MultiSigAction
    [ ('Spend, 0)
    , ('AddParty, 1)
    , ('RemoveParty, 2)
    , ('UpdateRequiredSigs, 3)
    , ('ExtendTimeLimit, 4)
    ]

-- | Данные, храняемые на контракте
data MultiSigDatum = MultiSigDatum
    { msigParams :: !MultiSigParams -- ^ Параметры многоподписного контракта
    , msigCreationTime :: !Integer  -- ^ Время создания (в слотах)
    }

PlutusTx.makeIsDataIndexed ''MultiSigDatum [('MultiSigDatum, 0)]

-- | Искупитель (redeemer) для контракта
newtype MultiSigRedeemer = MultiSigRedeemer
    { msigAction :: MultiSigAction -- ^ Действие, которое нужно выполнить
    }

PlutusTx.makeIsDataIndexed ''MultiSigRedeemer [('MultiSigRedeemer, 0)]

-- | Экземпляр типизированного скрипта для многоподписного контракта
data MultiSig
instance Scripts.ValidatorTypes MultiSig where
    type instance DatumType MultiSig = MultiSigDatum
    type instance RedeemerType MultiSig = MultiSigRedeemer