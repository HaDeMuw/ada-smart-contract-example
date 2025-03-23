module Main where

import System.IO
import Test.Tasty
import Test.Tasty.HUnit

import TokenPolicy

-- | Главная функция для запуска тестов
main :: IO ()
main = defaultMain tests

-- | Все тесты
tests :: TestTree
tests = testGroup "Тесты смарт-контрактов"
    [ tokenTests
    ]

-- | Тесты для работы с токенами
tokenTests :: TestTree
tokenTests = testGroup "Тесты токенов"
    [ testCase "Создание токена с правильными параметрами" $ do
        let token = newToken "TestToken" "TTK" 6 1000000
        tokenName token @?= "TestToken"
        tokenSymbol token @?= "TTK"
        tokenDecimals token @?= 6
        tokenSupply token @?= 1000000
    ]
