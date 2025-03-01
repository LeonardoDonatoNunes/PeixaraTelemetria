![logo_header](https://github.com/user-attachments/assets/92eec43f-b747-4995-829b-a3af2111a93d)

# PeixaraTelemetria
Repositório que contém os scripts para tratamento dos dados do projeto Peixara


#  Instalar o projeto
## Para clonar o repositório
Abra um terminal na pasta que queira baixar o projeto e execute
```
git clone https://github.com/LeonardoDonatoNunes/PeixaraTelemetria.git
```

## Dados
Criar uma pasta chamada `dados` dentro da pasta que será baixada e copiar para dentro todos os arquivos de dados do projeto

## Renviron
Um arquivo de variáveis de ambiente com o nome `.Renviron` deve ser criado na raiz do projeto contendo as credenciais do banco de dados com os seguintes nomes:
```
DB_HOST=host
DB_NAME=peixara_db
DB_USER=username
DB_PASSWORD=user_pwd
DB_PORT=port_num
```

## Como utilizar o projeto

### Atualizar repositório local
Sempre antes de iniciar alterações no projeto é preciso atualizar o repositório local com possíveis alterações no repositório remoto. Para isso, na linha de comando executar:
```
git pull
```

### Enviar alterações locais para o repositório remoto
Sempre que finalizar algum desenvolvimento é preciso enviar as alterações para o repositório remoto. Para isso seguir os passos abaixo:

1. __Adicionar as alterações__: Para adicionar as alterações excecutar: `git add .` 
2. __Comitar as alterações__: Para commitar as alterações excecutar: `git commit -m "Mensage com resumo geral das alterações"` 
3. __Enviar as alterações para o repositório remoto__: Excecutar: `git push origin main`


# Instruções do projeto
## Scripts
A pasta `scripts` contém todos os scritps que devem ser executados no projeto. Funções comuns a mais scripts estarão todas no arquivo `funcoes.R` que deve ser carregado em todos os outros scripts.
