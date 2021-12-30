Previsão de gasto de energia de eletrodomésticos

Este é um projeto da formação cientista de dados

Este projeto tem como objetivo criar um modelo que possa prever os gastos com eletricidade de eletrodomésticos baseando-se em diversas informações
Entre elas: Informações de sensores de temperatura e umidade, informações meteorológicas, consumo com iluminação, data, dia da semana, horário

O Script contém código para filtrar as variáveis preditoras de maneira a retirar as variáveis que não contribuem para a predição propriamente dita
uma vez definidas as variáveis que serão utilizadas para o modelo contrui um script auxiliar que foi carregado no código de maneira a não precisar reescrever o código
necessário para realizar as transformações realizadas nos dados, isso economiza algumas linhas de código

Também construi uma função que realiza o treinamento de diversos modelos com o uso do pacote caret e escolhe, através da métrica RMSE, qual é o melhor dos modelos retornando-o 
para que posssa ser salvo e usado posteriormente

Ao final ainda analiso o impacto do tratamente dos outliers no modelo, o que gerou um resultado positivo.


Licença:
Uso livre 