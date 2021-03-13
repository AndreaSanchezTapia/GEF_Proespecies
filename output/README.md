[![hackmd-github-sync-badge](https://hackmd.io/a1llZOqCTw6DEZoUCJ1q2g/badge)](https://hackmd.io/a1llZOqCTw6DEZoUCJ1q2g)


## Colunas que devem ser criadas 

- [ ] Grupo
- [ ] Tira autores
- [ ] Split especie em gênero e epiteto
- [ ] Sinonimia: nome correto quando sinonimo
- [ ] Fonte_sinonimia: FB quando FB, checar os problemas abaixo


## Atualizar cat_ameaca


|           | UICN_UK  | brasil   | cncflora | estadoSP |  munSP   |
|-----------| -------- | -------- | -------- | -------- | -------- |
| Base      |          |          |          |          |          |
| Sinonimos |          |          |          |          |          |



## Erros e detalhes que têm que ser verificados

1. Nomes que a Flora do Brasil não acha: arquivos `output/05_not_found_by_FB.csv`
1. Nomes que a Flora do Brasil acha mas ainda considera sinônimos: arquivos `output/05_sinonimos_segundo_FB2020.csv`
1. Nomes que a Flora do Brasil acha e corrige mas detecta no Estado de SP


## Erros pontuais nas bases de dados

### Município SP
- [ ] _Spananthe_ Jacq. sem epiteto

### CNCFlora
- [x] _Cattleya lobata_ - endêmica do RJ, nao muda nada

### SIMA 
- [x] _Stenandrium diphyllum_ duplicado, duplo LC por cncflora

### Estado SP
- [ ] _Vismia martiana_ duplicada em Estado SP, com critérios conflitantes (CR, EN), famílias diferentes

