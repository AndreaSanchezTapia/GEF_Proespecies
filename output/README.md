[![hackmd-github-sync-badge](https://hackmd.io/a1llZOqCTw6DEZoUCJ1q2g/badge)](https://hackmd.io/a1llZOqCTw6DEZoUCJ1q2g)


## Pendências

- [x] coluna Grupo
- [x] Cria coluna fonte em todas as bases
- [x] Cria a coluna fontes para evitar linhas duplicadas: em quais das listas está a espécie
- [x] Tira autores dos nomes
- [x] Split espécie em gênero e epiteto
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
1. Nomes que a Flora do Brasil acha e corrige mas não detecta no Estado de SP


## Erros pontuais nas bases de dados

### Município SP
- [x] _Spananthe_ Jacq. sem epíteto

### CNCFlora
- [x] _Cattleya lobata_ - endêmica do RJ, nao muda nada

### SIMA 
- [x] _Stenandrium diphyllum_ duplicado, duplo LC por cncflora

### Estado SP
- [x] _Vismia martiana_ duplicada em Estado SP, com critérios conflitantes (CR, EN), famílias diferentes

# Patricia checando
Aeschynomene montevidensis var. microphylla- a especie foi avaliada como LC pelo cncflora em 2012.

Alternanthera malmeana R.E.Fr. - a espécie foi avaliada pelo CNCFlora como DD, mas segundo a FB 2020 não ocorre no BR.

Achimenes ichtyostoma - o gênero não ocorre na Flora do Brasil.

Aniba rosaeodora Ducke - erro de spelling. O erro está na lista do CNFlora. Nome correto e aceito na FB 202 = Aniba rosiodora Ducke.

Athyrium filix-femina (L.) Roth - é uma samambaia que segundo a Flora do Brasil, não ocorre no BR.

Austroblechnum lechleri (T. Moore) Gasper & V.A.O. Dittrich - é uma samambaia que segundo a Flora do Brasil, não ocorre no BR.

Begonia diaphones L.B. Sm. & Wassh. é um nome não resolvido segundo o The Plant List., não está na Flora do Brasil. Parece-me uma planta ornamental.

Begonia glabrescens - não existe em qualquer base de dados de nomes de espécies de flora.

Begonia klydophylla - não existe em qualquer base de dados de nomes de espécies de flora.

Begonia succulenta - não está em qualquer base de dados de nomes de espécies de flora. Importante ressaltar que achei o isótypo no Jstor e ocorre no Paraná.

Begonia tibagiensis - não está em qualquer base de dados. Assim como a espécie anterior é citada para o estado do Paraná.

Cedrela angustifolia -  segundo a Flora do Brasil, não ocorre no BR.

Chaetoclamys psammina - não está presente em qualquer base de dados

Dyckia commixta - não ocorre no BR, segundo a Flora do Brasil. Avaliada pelo CNClora como DD em 2012 com registros apenas no estado do Paraná.

Dyckia julianae - segundo a flora do Brasil é um híbrido. Dyckia x julianae

Echinodorus rhombifolia - não é encontrado em qualquer base de dados da flora.

Elaphoglossum jamesonii - não ocorre no BR.

Enhydra sessilis - não está na FLora do Brasil. Não ocorre em São Paulo. Há um artigo que divulga a ocorrência no RS.

Epidendrum polyanthum - não ocorre no BR.

Erythrophyllastrum andinum - espécie não está na Flora do Brasil 2020. Segundo CNCFlora ocorre no RS e PR. 

Evolvulus elegans var. confertifolius - ocorre no BR, nos estados de SP e PR segundo a FB.

Ficus velutina - não ocorre no BR.

Hibiscus linearis é sinônimo de Hibiscus striatus, este último ocorre no RS e SC apenas. Os nomes não estão na FB.

Hibiscus urticifolius - não está na FB 2020. Mas é um nome válido em outras bases de dados de plantas. Há um artigo de 2014 (https://www.researchgate.net/publication/273309217_Sinopse_de_Hibiscus_L_Malvoideae_Malvaceae_do_Estado_de_Sao_Paulo_Brasil_especies_nativas_e_cultivadas_ornamentais) que publica a presença da espécie e tem como material examinado há duas coletas para 1956.

Holocheilus schulzii - não está na Flora do Brasil, e segundo Tropicos ocorre no Paraguai e Argentina.

Hypericum cordiforme var. Kleinii - o nome correto e aceiro é Hypericum cordiforme. Este último está na FB mas não foi avaliado.

Ossaea australis - não está em qualquer base de dados de planta.

Paepalanthus melaleucus var. epilosus - não está em qualquer base de dados de planta. A espécie: Paepalanthus melaleucus está como nome correto e aceito na FB, mas não foi avaliado.

Parodia schumanniana - não ocorre no BR.

Peperomia cooperi - não ocorre no BR.

Peperomia sessilifolia - nome não resolvido (The Plant List) e não está na FB.

Poikilacanthus guillesii - é sinônimo de Justicia gilliesii (Nees) Benth. que não ocorre no BR.

Portulaca cryptopetala var. poellniztiana - é sinônimo de Portulaca cryptopetala segundo FB. A espécie Portulaca cryptopetala não foi avaliada.

Rhipsalis hileiabaiana subsp. hileiabaiana - é o nome atual de Rhipsalis baccifera subsp. hileiabaiana N.P.Taylor & Barthlott. O sinônimo foi avaliado como DD. Já a espécie não foi avaliada.

Schwenckia grandiflora var. hatschbachiana - não é encontrado em qualquer base de dados. A espécie Schwenckia grandiflora hoje é sinônimo de Schwenckia breviseta Casar.

Spananthe Jacq. - não é permitido avaliar o gênero. 

Syngonanthus caulescens var. epapillosus - o nome atual é Syngonanthus caulescens (Poir.) Ruhland. Este último foi avaliado em 2011 com LC. POr favor usar LC na categoria.

Trichilia lepidota var. schumanniana - é uma variedade de Trichilia lepidota. A espécie foi avaliadas com LC em 2011.

Zexmenia viguerioides - o gênero da espécie não ocorre no BR. 

05_sinonimos_segundo_FB2020.csv

Camaridium meleagris (Lindl.) M.A.Blanco - sinônimo - nome aceito e correto: Maxillaria meleagris Lindl. e a última não ocorre no BR.

Ditassa arianeae é sinônimo de Ditassa melantha Silveira. A útima não foi avaliada.

Ditassa lagoensis é sinônimo de Macroditassa lagoensis (E.Fourn.) Malme. Este último é sinônimo na FB, mas a Flora não Indica o nome aceito e correto. Macroditassa lagoensis (E. Fourn.) Malme é um nome correto para o The Plant List. Macroditassa lagoensis (E. Fourn.) Malme foi avaliado como LC pelo CNCFlora.

Matelea santosii Morillo & Fontella não está na FB e a flora não indica o nome aceito e correto para ajustar. Seguno o The plant list é um nome aceito.

Spigelia kleinii var. paranaensis é sinônimo de 
Spigelia kleinii L.B.Sm. foi avaliada como DD pelo CNCFlora. Usar Spigelia kleinii para DD.


OUTRO

Vismia martiana não ocorre em São Paulo conforme FB 20.



