# Este repositório é um jogo que fiz com um colega para a universidade (Laboratórios de Informática I) com a linguagem Haskell


Para jogar o jogo compile a Tarefa 5 (src/Tarefa5_2021li1g021.hs) e execute o ficheiro executável criado (./nome_do_ficheiro na terminal do Linux)

## Dependências

-[GHC e Cabal](https://www.haskell.org/downloads/) <p/>
-Gloss (libraria utilizada para a interface gráfica)

```bash
$ cabal update
$ cabal install --lib gloss
$ cabal install --lib gloss-juicy
```

## Testes

O projeto contém testes unitários escritos usando a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit). Os testes podem ser executados da seguinte forma.

```bash
$ ghci -i="src" -i="tests" tests/Tests.hs
>>> runTestsT1 -- Correr os testes tarefa 1
>>> runTestsT2 -- Correr os testes tarefa 2
>>> runTestsT3 -- Correr os testes tarefa 3
>>> runTestsT4 -- Correr os testes tarefa 4
>>> runAllTests -- Correr todos os testes
```

## Créditos

Luís Carlos Fragoso Figueiredo (eu)

Flávio David Rodrigues Sousa (meu colega)
