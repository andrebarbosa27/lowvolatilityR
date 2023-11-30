# BACKTESTING DO EFEITO DE VOLATILIDADE EM PORTFÓLIOS NO MERCADO ACIONÁRIO BRASILEIRO ENTRE 2000 e 2020

## Descrição
Este repositório contém a implementação e análise do estudo "Backtesting do Efeito de Volatilidade em Portfólios no Mercado Acionário Brasileiro entre 2000 e 2020" de André Luiz Barbosa de Sousa. O projeto investiga estratégias de baixa volatilidade no mercado de ações brasileiro, utilizando dados de 2000 a 2020 para fornecer insights sobre o desempenho dessas estratégias em comparação com estratégias neutras e o índice Ibovespa.

## Tecnologias Utilizadas
## Tecnologias Utilizadas
- R
- dplyr: Para manipulação de dados
- tidyquant: Para trabalhar com dados financeiros
- lubridate: Para manipulação de datas
- zoo: Para trabalhar com dados temporais
- PerformanceAnalytics: Para análises de performance financeira
- ggplot2: Para visualização de dados

## Instalação e Uso
Para executar este projeto, é necessário ter o R instalado. Instale os pacotes necessários utilizando, por exemplo, `install.packages("dplyr")` para cada pacote. O script R `Low_Volatility.R` contém todo o processo de análise e backtesting.
Para executar este projeto,  
## Funcionalidades
- Análise de estratégias de investimento baseadas em baixa volatilidade.
- Comparação de desempenho com estratégias neutras e o índice Ibovespa.
- Backtesting de estratégias de portfólio para o mercado acionário brasileiro.

## Screenshots

### Comparação de Estratégias de Trade
![Comparação de Estratégias de Trade](https://github.com/andrebarbosa27/lowvolatilityR/blob/master/Compara%C3%A7%C3%A3o%20de%20Estrat%C3%A9gias%20de%20Trade.png)

### Retorno Cumulativo e Drawdowns
![Cumulative Returns e Drawdowns](https://github.com/andrebarbosa27/lowvolatilityR/blob/master/Retorno%20Acumulado%20e%20Drawdown.png)

### Retorno das Estratégias ao Longo do Tempo
![Retorno das Estratégias ao Longo do Tempo](https://github.com/andrebarbosa27/lowvolatility/blob/master/Retorno%20das%20estrat%C3%A9gias%20ao%20longo%20do%20tempo.png)

### Média Móvel de Retorno
![Rolling Mean](https://github.com/andrebarbosa27/lowvolatilityR/blob/master/Rolling%20Retorno.png)

### Desvio Padrão Móvel
![Rolling Standard Deviation](https://github.com/andrebarbosa27/lowvolatilityR/blob/master/Rolling%20Std.png)

## Geração e Exportação de Gráficos
O script R incluído neste repositório foi modificado para gerar e salvar automaticamente uma série de gráficos. Estes gráficos são salvos como arquivos PNG no diretório de trabalho após a execução do script.

### Instruções para Visualização dos Gráficos
- Após executar o script R, verifique o diretório de trabalho do script.
- Você encontrará vários arquivos de imagem (formato PNG) nomeados sequencialmente (por exemplo, `plot1.png`, `plot2.png`, etc.).
- Esses arquivos representam os gráficos gerados pelo script e podem ser abertos com qualquer visualizador de imagens padrão.

### Detalhes Específicos dos Gráficos
Cada gráfico salvo fornece insights visuais importantes relacionados ao estudo de estratégias de baixa volatilidade no mercado acionário brasileiro. Eles incluem análises temporais, comparações de desempenho e outras métricas relevantes para a pesquisa.

## Contribuições
Contribuições são bem-vindas! Se você tem sugestões ou melhorias, sinta-se à vontade para abrir uma issue ou enviar um pull request.

## Contato
Para mais informações, entre em contato com andr642010@hotmail.com.