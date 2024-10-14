# Prova funcional

- Esse repositorio contem as questoes da prova da cadeira de programacao funcional implementadas em haskell;
- As questoes estao em cada arquivo separado e os testes no arquivo `Test.hs`;

## Rodando os testes
- Para rodar os testes, basta usar o proprio interpretador de haskell `ghci` e carregar o modulo de testes e chamar a funcao `runTests`:

```
:l Tests
runTests
```

- A funcao deve mostrar para qual questao esse teste se tratava e o nome do teste para aquela questao, alem disso, sera mostrado se aquele teste passou ou nao com um `++ Passed!` ou `-- Fail` antes do titulo do teste.
