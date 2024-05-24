função rodarJogo{
	Se (jogador1 venceu) {
		Chama função que atualiza pontuação; 
        Abre arquivo para escrita; 
        Atualiza o arquivo com os dados atualizados; 
    } Se não faça{
        Se (jogador2 venceu) {
            Chama função que atualiza pontuação; 
            Abre arquivo para escrita; 
            Atualiza o arquivo com os dados atualizados;
        } Se não faça{
            Se (todas as posições já foram marcadas){
                Houve um empate. Printar na linha de comando;
            } Se não faça {
                Se (vez == 0){
                    Jogador 1 deve jogar; 
                    Escolher uma opção; 
                    Verificar se a opção é válida.
                } Se não faça{
                    Jogador 2 deve jogar; 
                    Escolher uma opção; 
                    Verificar se a opção é válida
                }
            }
        }
    }
}