# DeFi пул (DeFi Pool)

Данный пример демонстрирует реализацию простого DeFi пула ликвидности на блокчейне Cardano с использованием языка Plutus.

## Описание

DeFi пул позволяет пользователям предоставлять ликвидность для торговли токенами, зарабатывая комиссии с каждой сделки. Реализованный пул использует принцип автоматического маркет-мейкера (AMM) с формулой постоянного произведения.

## Особенности

- Предоставление ликвидности (добавление токенов в пул)
- Вывод ликвидности (получение токенов из пула)
- Обмен токенов через пул
- Токены ликвидности (LP токены) для отслеживания доли в пуле
- Комиссии пула распределяются между поставщиками ликвидности

## Запуск примера

```bash
cabal run defi-pool
```

## Структура кода

- `LiquidityPoolValidator.hs` - Основная логика валидатора пула ликвидности
- `LiquidityPoolTypes.hs` - Типы данных для пула ликвидности
- `LiquidityPoolOperations.hs` - Операции с пулом ликвидности
- `Main.hs` - Демонстрация использования пула ликвидности

## Дополнительные ресурсы

- [Концепции DeFi](https://developers.cardano.org/docs/get-started/cardano-defi-ecosystem)
- [Автоматические маркет-мейкеры](https://docs.cardano.org/plutus/plutus-use-cases#defi)