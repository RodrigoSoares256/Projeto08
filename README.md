Previs�o de gasto de energia de eletrodom�sticos

Este � um projeto da forma��o cientista de dados

Este projeto tem como objetivo criar um modelo que possa prever os gastos com eletricidade de eletrodom�sticos baseando-se em diversas informa��es
Entre elas: Informa��es de sensores de temperatura e umidade, informa��es meteorol�gicas, consumo com ilumina��o, data, dia da semana, hor�rio

O Script cont�m c�digo para filtrar as vari�veis preditoras de maneira a retirar as vari�veis que n�o contribuem para a predi��o propriamente dita
uma vez definidas as vari�veis que ser�o utilizadas para o modelo contrui um script auxiliar que foi carregado no c�digo de maneira a n�o precisar reescrever o c�digo
necess�rio para realizar as transforma��es realizadas nos dados, isso economiza algumas linhas de c�digo

Tamb�m construi uma fun��o que realiza o treinamento de diversos modelos com o uso do pacote caret e escolhe, atrav�s da m�trica RMSE, qual � o melhor dos modelos retornando-o 
para que posssa ser salvo e usado posteriormente

Ao final ainda analiso o impacto do tratamente dos outliers no modelo, o que gerou um resultado positivo.


Licen�a:
Uso livre 