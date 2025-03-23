-- | Главный модуль, демонстрирующий использование токена
module Main where

import TokenPolicy
import TokenOperations
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude

-- | Пример использования токена
main :: IO ()
main = do
    putStrLn "Creating a sample token on Cardano"
    
    -- Создание токена с параметрами
    let tokenParams = newToken "ExampleToken" "ETK" 6 1000000
    
    putStrLn $ "Token name: " ++ show (tokenName tokenParams)
    putStrLn $ "Token symbol: " ++ show (tokenSymbol tokenParams)
    putStrLn $ "Token decimals: " ++ show (tokenDecimals tokenParams)
    putStrLn $ "Token supply: " ++ show (tokenSupply tokenParams)
    
    putStrLn "\nToken policy hash:"
    print $ tokenPolicyHash tokenParams
    
    putStrLn "\nThis is a demonstration. In a real scenario, you would:"
    putStrLn "1. Submit the minting transaction to the blockchain"
    putStrLn "2. Distribute tokens to users"
    putStrLn "3. Enable token trading through contracts or exchanges"
    
    putStrLn "\nExample completed successfully!"