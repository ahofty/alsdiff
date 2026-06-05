# Catálogo de saídas — `als_reorder.py` e `als_transplant.py`

> Objetivo: revisar a **UX/layout** de cada saída que as ferramentas produzem (não é teste de
> correção — disso cuidam os `selftest`). Cada cenário abaixo dispara um **formato de saída
> diferente**. Onde possível, usa dados reais; os casos de erro/incompatibilidade que os dados
> reais não disparam usam **fixture sintética** (claramente marcada).
>
> **Dados reais usados:**
> - **F1** = `00_Live_..._TesteREODER.prd.als` — set principal, 345 cenas, 29 blocos.
> - **F2** = `00_Live_XA_Remix_P3_TRANSPLANT_caughtup_heaven.als` — F1 + `caught up` + `heaven`
>   inseridos antes de `gruta` (saída do transplante de ontem, fire-test passou). 360 cenas, 31 blocos.
>
> `.als` gerados foram gravados em `/tmp/catalogo/` (descartáveis); aqui registramos só o **stdout**.
>
> **Como comentar:** escreva observações inline sob cada bloco, em linha começando com `>>> `.

---

## `als_reorder.py`

### R1 — `inspect` (lista de blocos) · real F1

```
$ als_reorder.py inspect F1.als
```
```
Arquivo: .../00_Live_..._TesteREODER.prd.als
Cenas: 345
ClipSlotList com 345 slots (reordenáveis em lockstep): 19
Prefixo de bloco: '>>'
Cenas de cabeçalho (fixas, antes do 1º bloco): 0
Blocos detectados: 29
  [  0..  7] ( 8 cenas,  7 c/ clips)  'staying alive'
  [  8.. 16] ( 9 cenas,  6 c/ clips)  'T01'
  [ 17.. 23] ( 7 cenas,  5 c/ clips)  'T02'
  [ 24.. 30] ( 7 cenas,  6 c/ clips)  'space jam'
  [ 31.. 36] ( 6 cenas,  4 c/ clips)  'E2'
  [ 37.. 42] ( 6 cenas,  4 c/ clips)  'a disaster'
  [ 43.. 57] (15 cenas, 14 c/ clips)  'pale pale moon'
  [ 58.. 65] ( 8 cenas,  6 c/ clips)  'breath in'
  [ 66.. 71] ( 6 cenas,  4 c/ clips)  'OW'
  [ 72.. 77] ( 6 cenas,  4 c/ clips)  'H1'
  [ 78.. 83] ( 6 cenas,  4 c/ clips)  'OZ'
  [ 84.. 95] (12 cenas, 10 c/ clips)  'dirty discotechno'
  [ 96..102] ( 7 cenas,  5 c/ clips)  'E4'
  [103..110] ( 8 cenas,  5 c/ clips)  '#1 crush'
  [111..123] (13 cenas, 12 c/ clips)  'the jungle drum'
  [124..131] ( 8 cenas,  6 c/ clips)  'you make me feel'
  [132..137] ( 6 cenas,  5 c/ clips)  'gonna make you sweat'
  [138..142] ( 5 cenas,  3 c/ clips)  'WW'
  [143..148] ( 6 cenas,  4 c/ clips)  'payback'
  [149..154] ( 6 cenas,  4 c/ clips)  'combate'
  [155..159] ( 5 cenas,  3 c/ clips)  'E5'
  [160..165] ( 6 cenas,  5 c/ clips)  'ZZ clap your spoons'
  [166..170] ( 5 cenas,  4 c/ clips)  'stand up'
  [171..175] ( 5 cenas,  4 c/ clips)  'stop this flame'
  [176..181] ( 6 cenas,  4 c/ clips)  'we are family'
  [182..197] (16 cenas,  5 c/ clips)  'gruta'
  [198..204] ( 7 cenas,  6 c/ clips)  'justify my love'
  [205..209] ( 5 cenas,  3 c/ clips)  'moaners'
  [210..344] (135 cenas, 72 c/ clips)  'falta organizar'

Jumps de clip ativos (FollowActionA/B==9): use 'reorder' para remapear.
```
> saída pra console ok. acho que podemos gerar um arquivo ("nome_do_projeto.playlist") com as músicas em ordem pra que eu possa reordenar num editor de texto e usar como input do comando reorder. esse arquivo pode conter apenas os nomes das musicas, uma em cada linha.

---

### R2 — `reorder` sucesso · real F1
Move `falta organizar` para o topo. **Nota de UX:** `reorder` exige a **ordem completa** dos 29
blocos (é uma permutação total), não só o que muda.

```
$ als_reorder.py reorder F1.als --order "falta organizar,staying alive,T01,...,moaners"
```
```
OK: 345 cenas, 345 reposicionadas.
Saída: /tmp/catalogo/r2_out.als
```
> saída pra console ok. mas vale alterar o comando para ler um arquivo de playlist gerado pelo comando inspect. e pode assumir que o nome do arquivo segue o padrao "nome_do_projeto.playlist" pra nao precisar passar como argumento. se a ordem das musicas pasadas pelo arquivo ou pelo argumento --order for a mesma ordem do projeto não precisa fazer nada. 

---

### R3 — `reorder` erro de permutação · real F1
Ordem incompleta (`"T01,T02"`).

```
$ als_reorder.py reorder F1.als --order "T01,T02"
```
```
Erro: a ordem deve ser uma permutação dos blocos.
  Faltando: ['#1 crush', 'E2', 'E4', 'E5', 'H1', 'OW', 'OZ', 'WW', 'ZZ clap your spoons', 'a disaster', 'breath in', 'combate', 'dirty discotechno', 'falta organizar', 'gonna make you sweat', 'gruta', 'justify my love', 'moaners', 'pale pale moon', 'payback', 'space jam', 'stand up', 'staying alive', 'stop this flame', 'the jungle drum', 'we are family', 'you make me feel']
  Blocos no set: ['staying alive', 'T01', 'T02', 'space jam', 'E2', 'a disaster', 'pale pale moon', 'breath in', 'OW', 'H1', 'OZ', 'dirty discotechno', 'E4', '#1 crush', 'the jungle drum', 'you make me feel', 'gonna make you sweat', 'WW', 'payback', 'combate', 'E5', 'ZZ clap your spoons', 'stand up', 'stop this flame', 'we are family', 'gruta', 'justify my love', 'moaners', 'falta organizar']
```
(exit 1)
> saída pra console ok.

---

### R4 — `subset --keep` (reordena na ordem dada) · real F1

```
$ als_reorder.py subset F1.als --keep "space jam,T01"
```
```
OK: 2 blocos mantidos, 16 cenas (de 345). Cabeçalho (0 cenas) removido.
Blocos: space jam, T01
Saída: /tmp/catalogo/r4_out.als
```
> saída pra console ok. pode ler um arquivo de playlist igual o reorder.

---

### R5 — `subset --drop` (mantém o resto na ordem original) · real F1

```
$ als_reorder.py subset F1.als --drop "falta organizar"
```
```
OK: 28 blocos mantidos, 210 cenas (de 345). Cabeçalho (0 cenas) removido.
Blocos: staying alive, T01, T02, space jam, E2, a disaster, pale pale moon, breath in, OW, H1, OZ, dirty discotechno, E4, #1 crush, the jungle drum, you make me feel, gonna make you sweat, WW, payback, combate, E5, ZZ clap your spoons, stand up, stop this flame, we are family, gruta, justify my love, moaners
Saída: /tmp/catalogo/r5_out.als
```
> saída pra console ok. e só pra deixar claro, esse comando nao faz sentido ler o arquivo de playlist. 

---

### R6 — `subset` ABORT por jump pendurado · real F1
Manter só `dirty discotechno`: seus clips têm `JumpIndexB` saltando para a cena 101 (no bloco
`E4`, que seria removido). Caso real registrado no roadmap.

```
$ als_reorder.py subset F1.als --keep "dirty discotechno"
```
```
ERRO: há 'jump to scene' apontando para cena REMOVIDA. Isso não deveria existir
(pulos devem ficar dentro da mesma música). Abortei SEM gravar nada.

  cena 87 ('') [CSL#11] JumpIndexB -> cena 101 ('') [REMOVIDA]
  cena 89 ('') [CSL#11] JumpIndexB -> cena 101 ('') [REMOVIDA]
  cena 90 ('') [CSL#11] JumpIndexB -> cena 101 ('') [REMOVIDA]
```
(exit 1)
> saída pra console ok.

---

### R7 — `subset` erro de bloco desconhecido · real F1

```
$ als_reorder.py subset F1.als --keep "naoexiste"
```
```
Erro: blocos desconhecidos no --keep: ['naoexiste']
Blocos no set: ['staying alive', 'T01', 'T02', 'space jam', 'E2', 'a disaster', 'pale pale moon', 'breath in', 'OW', 'H1', 'OZ', 'dirty discotechno', 'E4', '#1 crush', 'the jungle drum', 'you make me feel', 'gonna make you sweat', 'WW', 'payback', 'combate', 'E5', 'ZZ clap your spoons', 'stand up', 'stop this flame', 'we are family', 'gruta', 'justify my love', 'moaners', 'falta organizar']
```
(exit 1)
> saída pra console ok.

---

### R8 — `subset` erro `--keep` + `--drop` juntos (argparse) · real F1

```
$ als_reorder.py subset F1.als --keep "T01" --drop "T02"
```
```
usage: als_reorder.py subset [-h] (--keep KEEP | --drop DROP)
                             [--output OUTPUT] [--prefix PREFIX]
                             input
als_reorder.py subset: error: argument --drop: not allowed with argument --keep
```
(exit 2)
> saída pra console ok.

---

## `als_transplant.py`

### T1 — `compare` caso real (F1 vs F2)
F1 e F2 vêm da mesma linhagem → tracks/devices idênticos; a diferença está só nas músicas
(`caught up`/`heaven` só em F2).

```
$ als_transplant.py compare F1.als F2.als
```
```
A: 00_Live_XA_Remix_Housy_VS2_ZUNIDOvPOSv3.2.MasSegui.TesteREODER.prd.als
B: 00_Live_XA_Remix_P3_TRANSPLANT_caughtup_heaven.als
Tracks em A: 13 | em B: 13
  ✓ Projetos idênticos em tracks e cadeias de device — seguro nos 2 sentidos.

Músicas (blocos '>>') — A: 29 | B: 31
  Em comum (29): staying alive, T01, T02, space jam, E2, a disaster, pale pale moon, breath in, OW, H1, OZ, dirty discotechno, E4, #1 crush, the jungle drum, you make me feel, gonna make you sweat, WW, payback, combate, E5, ZZ clap your spoons, stand up, stop this flame, we are family, gruta, justify my love, moaners, falta organizar
  Só em A (0): —
  Só em B (2): caught up, heaven
```
> saída pra console ok por equanto, acho que preciso usar mais pra indicar o que poderia melhorar.

---

### T2 — `compare` só uma direção segura · **sintético**
B = A + device `Saturator` extra no fim da chain de `DRUMS`. Mover de A→B é seguro (cadeia de A
está contida na de B); B→A não.

```
$ als_transplant.py compare t2_A.als t2_B.als
```
```
A: t2_A.als
B: t2_B.als
Tracks em A: 3 | em B: 3

Direção A→B (mover clips de A para B): SEGURO ✓
    ✓ todas as tracks de A existem em B e suas cadeias estão contidas

Direção B→A (mover clips de B para A): INSEGURO ✗
    ✗ [MidiTrack] DRUMS: B tem devices que A não tem: Saturator

RESULTADO: Só A→B é seguro

Músicas (blocos '>>') — A: 3 | B: 3
  Em comum (2): intro, verse
  Só em A (1): chorus
  Só em B (1): bridge
```
> saída pra console ok por equanto, acho que preciso usar mais pra indicar o que poderia melhorar.

---

### T3 — `compare` incompatível nos 2 sentidos · **sintético**
Cada lado tem um device que o outro não tem (`Compressor2` em A, `Roar` em B).

```
$ als_transplant.py compare t3_A.als t3_B.als
```
```
A: t3_A.als
B: t3_B.als
Tracks em A: 2 | em B: 2

Direção A→B (mover clips de A para B): INSEGURO ✗
    ✗ [MidiTrack] DRUMS: A tem devices que B não tem: Compressor2

Direção B→A (mover clips de B para A): INSEGURO ✗
    ✗ [MidiTrack] DRUMS: B tem devices que A não tem: Roar

RESULTADO: INCOMPATÍVEL nos dois sentidos ✗

Músicas (blocos '>>') — A: 2 | B: 2
  Em comum (2): intro, verse
  Só em A (0): —
  Só em B (0): —
```
(exit 1)
> saída pra console ok por equanto, acho que preciso usar mais pra indicar o que poderia melhorar.

---

### T4 — `transplant` SUCESSO · real (F2 → F1)
Reproduz a saída de ontem: move `caught up,heaven` de F2 para F1, antes de `gruta`.

```
$ als_transplant.py transplant F2.als F1.als --move "caught up,heaven" --before "gruta"
```
```
OK: movidos 2 blocos (15 cenas) de SRC p/ DST, antes de 'gruta'.
Blocos: caught up, heaven
ClipSlotLists casadas: 19
Scene Ids novos: (1012, 1026)
PointeeId usados pelos clips movidos: 12 (identidade: 12 | remapeados: 0)
Saída: /tmp/catalogo/t4_out.als
```
(exit 0) — confere com o roadmap: 15 cenas movidas, 19 CSLs em lockstep, 12 pointees resolvidos
por **identidade** (mesma linhagem F1/F2). Quando há remap de id, cada um aparece numa linha
`remap OLD -> NEW` abaixo (aqui não houve).
> saída pra console ok por equanto, acho que preciso usar mais pra indicar o que poderia melhorar.

---

### T5 — `transplant` ABORT por jump pendurado · **sintético**
Bloco movido (`aaa`) tem um clip com "jump to scene" para fora dos blocos movidos (cena no
bloco `bbb`).

```
$ als_transplant.py transplant t5_src.als t5_dst.als --move "aaa" --before "target"
```
```
1 clip(s) movido(s) têm 'jump to scene' para FORA dos blocos movidos (pulo ficaria pendurado). Abortei sem gravar:
  cena 1 (aaa) -> cena 4 (bbb)
Corrija esse follow action na origem (ou peça --disable-dangling, a implementar).
```
(exit 1)
> saída pra console ok.

---

### T6 — `transplant` ABORT por PointeeId não resolvido · **sintético**
Clip movido usa `PointeeId 777` (parâmetro de um device `Eq8` na origem); no destino a track
casada tem device diferente (`Roar`), então o alvo não existe e não há remap seguro.

```
$ als_transplant.py transplant t6_src.als t6_dst.als --move "aaa" --before "target"
```
```
Não consegui mapear com SEGURANÇA 1 PointeeId usados por clips movidos (parâmetro que mudou no destino). Abortei sem gravar:
  777  contexto-origem=('DeviceChain', 'Devices', 'Eq8', 'UserName', 'P')  (mesmo-id-no-destino=None)
```
(exit 1)
> saída pra console ok por equanto, acho que preciso usar mais pra indicar o que poderia melhorar.

---

### T7 — `transplant` erro de bloco inexistente · real
Mover um bloco que não existe na origem.

```
$ als_transplant.py transplant F2.als F1.als --move "xyz" --before "gruta"
```
```
bloco 'xyz' não existe na origem
```
(exit 1)
> saída pra console ok por equanto, acho que preciso usar mais pra indicar o que poderia melhorar.

---

## Observações de UX já notadas (para discussão)

- **R2 (reorder):** exige listar **todos** os 29 blocos na ordem nova, mesmo movendo só um.
  Talvez valha um modo "mover bloco X para antes/depois de Y" (como o `--before` do transplant).
  → **✅ resolvido pelo fluxo de playlist** (abaixo): você edita um arquivo de texto em vez de
  passar a ordem inteira na linha de comando.
- **R6 / T5 / T6 (aborts):** nomes de cena vazios aparecem como `('')`. No R6 dá pra mostrar a
  qual **bloco** a cena pertence (como o T5 já faz: `cena 1 (aaa)`), o que seria mais legível.
  → ainda pendente (não mexido nesta rodada).
- **Saída de sucesso (R4/R5):** lista os blocos mas não o intervalo de cenas resultante; o
  transplant (T4) é mais verboso. Vale alinhar o nível de detalhe entre as ferramentas?
  → ainda pendente.
- **Exit codes:** consistentes (0 ok / 1 erro de domínio / 2 erro de argparse).

---

## Novidades implementadas — fluxo de playlist (`als_reorder.py`)

Atende suas notas em R1/R2/R4/R5. Resumo: `inspect` gera um `<projeto>.playlist` (um nome de
música por linha, na ordem atual); `reorder` e `subset` leem esse arquivo.

🐞 **Bug achado e corrigido no caminho:** a 1ª versão usava `#` como comentário na playlist, mas
o bloco real **`#1 crush`** começa com `#` e era **descartado silenciosamente** (a playlist saía
com 28 de 29 blocos). Decisão: a playlist contém **só nomes** (sem sintaxe de comentário, que
colidiria com nomes reais); só linhas em branco são ignoradas.

### N1 — `inspect` gera a playlist (não sobrescreve a sua) · real F1
```
$ als_reorder.py inspect proj.als
...
Playlist gerada (29 músicas, uma por linha): /tmp/catalogo/proj.playlist
  Reordene as linhas e rode 'reorder', ou apague linhas e rode 'subset'.

$ als_reorder.py inspect proj.als          # 2ª vez: respeita sua edição
Playlist já existe, mantive a sua (não sobrescrevi): /tmp/catalogo/proj.playlist
  Edite-a e use com 'reorder'/'subset', ou passe --force para regenerar.
```

### N2 — `reorder` lê a playlist (auto-descoberta) + **no-op** quando a ordem não mudou · real F1
Precedência da ordem: `--order` › `--playlist CAMINHO` › auto `<projeto>.playlist`.
```
$ als_reorder.py reorder proj.als          # playlist na ordem do projeto
Ordem já igual à do projeto (fonte: playlist .../proj.playlist); nada a fazer, não gravei nada.

$ als_reorder.py reorder proj.als          # após mover 'falta organizar' p/ o topo na playlist
OK: 345 cenas, 345 reposicionadas (fonte: playlist .../proj.playlist).
Saída: /tmp/catalogo/reord_out.als
```

### N3 — `subset` lê a playlist como lista a MANTER (inclui `#1 crush`) · real F1
`--drop` continua explícito e **não** lê playlist (conforme sua nota no R5).
```
$ als_reorder.py subset proj.als --playlist sub.playlist   # sub.playlist = space jam / #1 crush / T01
OK: 3 blocos mantidos, 24 cenas (de 345). Cabeçalho (0 cenas) removido.
Blocos: space jam, #1 crush, T01
```

### N4 — erros novos com mensagem acionável · real F1
```
$ als_reorder.py reorder proj.als          # sem --order e sem playlist ao lado
Nenhuma ordem informada e a playlist não existe:
  .../proj.playlist
Rode 'als_reorder.py inspect ".../proj.als"' para gerá-la (e então reordene as linhas), ou passe --order.
```
>>> (seus comentários sobre o fluxo de playlist aqui)
