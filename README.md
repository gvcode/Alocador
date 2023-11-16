# Alocador
Projeto do otimizador de alocação de pessoas em reuniões.

## Arquivos
### alocador.py
O arquivo principal, onde leremos os dados (no momento, simulados), aplicaremos o método de alocação, e exportaremos o resultado.

### depend.py
Arquivo organizacional, onde definimos as dependências e funções utilitárias gerais (função de custo).

### test.py
Arquivo organizacional, onde podemos brincar e testar as funções.

### dados.xlsx
Os dados reais das preferências. Mas deixaremos para usá-lo no final do projeto.

### deals.py
A primeira tarefa do alocador é definir quais opções de horários (grupos) receberam quantas pessoas. Nem todos serão usados (recebem 0), e cada um pode receber 4, 5, ou 6 pessoas. Podemos definir várias abordagens, inclusive uma fixa (determinística), basicamente nós "resolvendo" essa tarefa pelo alocador na canetada.

### method_???.py
Esses arquivos definem o método de alocação em si. Por enquanto temos:

- method_random.py - que chuta alocações aleatórias (dado os tamanhos-base definidos por qualquer uma das opções em deals), e guarda o melhor chute. É a solução trivial, nosso trabalho é criar opções melhores que esta.


