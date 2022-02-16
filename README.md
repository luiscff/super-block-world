# Este repositório é um jogo que fiz com um colega para a universidade (Laboratórios de Informática I) com a linguagem Haskell


Para jogar o jogo compile (recomendo o uso do ghc) a tarefa 5 do src e execute o ficheiro executável criado (./nome_do_ficheiro na terminal do Linux)

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
