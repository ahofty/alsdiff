# Roadmap: subset, transplante e merge de projetos `.als`

> Documento de **planejamento**. Continua o trabalho de `als_reorder.py` (ver
> `docs/plano-reordenar-cenas.md`, já validado). Aqui desenhamos três operações novas
> sobre a Session View, os riscos que **quebrariam o formato** do arquivo, a estratégia de
> validação e a escolha de ferramenta (Python vs OCaml).
>
> **Foco:** problemas 1 e 2. O 3 (merge) fica esboçado, sem profundidade.
>
> Convenção herdada: bloco = sequência de cenas iniciada por uma cena cujo nome tem o
> prefixo `>>`; o bloco vai do `>>` até antes do próximo (ou fim); separador em branco
> viaja com o bloco; cenas antes do 1º `>>` são cabeçalho fixo.

---

## 0. Fatos medidos no set real (fundamentam os riscos)

Set: `00_Live_..._TesteREODER.prd.als` — Live 12.3.8, 345 cenas, ~280 MB XML.

| Fato | Valor / implicação |
|---|---|
| `<NextPointeeId Value="658863"/>` no topo | **Alocador global de ids LOM.** Inserir objetos de outro projeto exige ids novos a partir daqui. |
| `<OverwriteProtectionNumber>` | Número de proteção; **não** é checksum de cenas. |
| Ponteiro de cena (`CurrentScene`/`SelectedScene`/`HighlightedSceneIndex`) | **Não existe** → remover/reordenar cenas não deixa ponteiro pendurado. ✅ |
| Checksum/contador de cenas | **Não existe** (só os 345 `<Scene>`; `ScenesListWrapper` é wrapper vazio). ✅ |
| Tracks | 9 principais (`1 BEAT`…`8 SAMPLE`, `RESAMPLE`) + 4 returns (`A-LONG DELAY`…`D-VS2fx`) + 1 `MainTrack` + 1 `PreHearTrack`. Identificáveis por `<Name><EffectiveName>`/`<UserName>`. |
| ClipSlotLists em lockstep (345 filhos) | **19**: cada track principal expõe **2** (a da track + a do `MainSequencer`); só 1 return (`D-VS2fx`) tem CSL de 345. Todas reordenadas/recortadas juntas (já validado). |
| `<ClipEnvelope>` dentro de clips de sessão | **676** (661 com `<EnvelopeTarget><PointeeId>`). **Automação interna do clip aponta para um parâmetro de device por id global** (ex.: PointeeId 15842 → `<ControllerTargets.0 Id="15842">`). ⚠️ Este é o calcanhar do transplante. |
| `<SavedPlayingSlot>` por track (×9, no MainSequencer) | **Índice ABSOLUTO de cena do clip que tocava ao salvar** (+ `SavedPlayingOffset`). 🔴 Se apontar p/ cena removida/realocada, o **engine de áudio crasha ao abrir** (EXC_BAD_ACCESS). **Tem que ser remapeado** em TODA operação (reorder/subset). `-2` = nada tocando (sentinela seguro). **Descoberto via crash do Live, não estava no plano inicial.** |

---

## 1. Problema 1 — **Subset** (projeto enxuto com só algumas músicas)

**Objetivo:** a partir do projeto principal, gerar um `.als` novo contendo apenas os blocos
escolhidos (SEM o cabeçalho fixo). Abre mais rápido, menos risco de o Live travar.

### 1.1. Abordagem (extensão direta do que já temos)
1. `detect_blocks` (já existe) lista os blocos.
2. Usuário diz quais **manter** (ou quais **remover**). Cabeçalho nunca fica.
3. Monta o conjunto de índices de cena mantidos, em ordem.
4. **Remove** (cirurgia de texto) os `<Scene>` não mantidos de `<Scenes>` **e**, em lockstep,
   os `<ClipSlot>` correspondentes em **cada** uma das 19 ClipSlotLists.
5. Permite combinar com reorder (mesma máquina de permutação, só que a permutação agora é
   uma **seleção+ordenação** em vez de bijeção).
6. Grava arquivo novo; nunca toca o original.

### 1.2. Riscos que quebrariam o formato (além dos que você citou)
- 🔴 **Jumps de clip pendurados (o risco real).** Clip mantido com follow action `FA==9`
  ("jump to scene") apontando para uma cena **removida**. Opções: (a) **desabilitar** essa
  follow action do clip (mais seguro — nada de salto errado silencioso); (b) remapear se o
  alvo foi mantido; (c) recusar e listar. **Proposta:** remapear os que sobrevivem;
  para os que apontam para cena removida, **desabilitar e reportar a lista**.
- 🟢 **Ponteiro de cena / checksum:** medido que **não existem** → nada a corrigir.
- 🟢 **Renumeração de ids:** desnecessária. `Scene Id`/`ClipSlot Id`/`PointeeId` são
  high-water marks globais; **remover** ids nunca colide. `NextPointeeId` fica como está
  (continua sendo um teto válido).
- 🟡 **Referência externa a um clip removido.** Medido que a automação de track aponta para
  **parâmetros de device** (que ficam), não para clips de sessão; clips de sessão não
  parecem ser referenciados de fora. **Mitigação:** passada de validação — coletar os ids
  definidos dentro dos spans removidos e conferir que nenhum é referenciado fora deles.
- 🟡 **Zero cenas / zero blocos:** guarda contra subset vazio.
- 🟢 **Tempo/compasso por cena:** viajam dentro de `<Scene>`, intactos.

### 1.3. Validação
- `inspect` na saída: nº de cenas e blocos batem com a seleção.
- Multiset de cenas/clipslots da saída == subconjunto correspondente do original.
- Nenhum jump fora de `[0, novo_nº_cenas)`.
- `alsdiff original subset`: deve mostrar **apenas remoções** de cenas/clips (casa por id).
- **Teste de fogo:** abrir no Live (sem recovery) e tocar os blocos mantidos.

### 1.4. Complexidade: **baixa.** É `als_reorder.py` + remoção. Fica em Python.

### 1.5. Status: **FEITO e VALIDADO ✅** (subcomando `subset`)
- `als_reorder.py subset IN --keep "A,B"` (reordena na ordem dada) ou `--drop "X,Y"`
  (preserva ordem original); cabeçalho sempre removido; aborta se clip mantido salta p/ cena
  removida. `selftest` cobre seleção, ordem keep/drop, abort e remap de jump sobrevivente.
- Validado no set real: cenas mantidas **byte-idênticas** ao subconjunto original; clipslots
  idênticos (jump-normalizado); jumps no range. **Achado:** há jumps que **cruzam blocos**
  no set real (87/89/90 de `dirty discotechno` → 101 de `E4`) — o abort barra corretamente
  quando o bloco-alvo não é mantido, e o remap acerta quando ambos ficam (91→7, 101→17).
- ⚠️ `alsdiff` **não** é validador confiável p/ subset: ele alinha clips **por posição**,
  então deleção aparece como "Modified/Removed". A checagem byte-a-byte é a referência.
- 🔴 **BUG encontrado via crash do Live e CORRIGIDO:** o subset crashava o Live (audio
  thread, EXC_BAD_ACCESS em 0x20) porque `SavedPlayingSlot` (índice absoluto da cena que
  tocava ao salvar, ×9) continuava apontando para cena removida. O reorder não crashava só
  porque preservava a contagem (o índice continuava válido, embora errado). Fix: remapear
  `SavedPlayingSlot` (mantida→novo índice; removida→`-2`, zerando offset) em reorder E subset.
- Pendente: fire-test no Ableton (arquivo `..._SUBSET_dd_E4_v2.als` gerado, com SPS sãos).

---

## 2. Problema 2 — **Transplante** (mover músicas de A e inserir antes de uma música em B)

**Objetivo:** pegar 1+ blocos do projeto A e inseri-los em B, imediatamente antes de um
bloco-alvo de B. A novidade que você citou (diferença de qtde/ordem de tracks) é **uma**
das dificuldades; há outras mais profundas que quebram o formato.

### 2.1. Abordagem
1. Detectar blocos em A e em B.
2. **Casar as tracks A↔B** (ver 2.2). Se não casar 1:1, **abortar e apontar a diferença**.
3. Para cada track casada, recortar de A os `<ClipSlot>` das cenas dos blocos movidos e
   **inserir** na posição-alvo da ClipSlotList correspondente de B (nas **duas** CSLs da
   track — track + MainSequencer).
4. Inserir os `<Scene>` dos blocos movidos em `<Scenes>` de B, na posição-alvo.
5. **Renumerar ids** do material vindo de A (ver 2.3) e **remapear PointeeIds** das
   automações internas dos clips (ver 2.4). Bumpar `NextPointeeId` de B.
6. Remapear jumps `FA==9` das cenas movidas para as novas posições em B.
7. Gravar B novo.

### 2.2. Casamento de tracks A↔B (a dificuldade que você citou)
- Casar por **(tipo, EffectiveName)** das tracks que possuem CSL de lockstep, na ordem.
  Incluir returns (sends dependem deles).
- **Recusar e reportar** se: contagem difere; algum nome de A não existe em B; tipos
  divergem; ordem relevante difere. Saída do tipo "track `7 LEAD` existe em A mas não em B".
- Sutileza: cada track tem **2** CSLs de 345 — casar track→(CSL_track, CSL_seq) dos dois
  lados e mover nas duas.

### 2.3. ⚠️ Colisão e renumeração de ids LOM (risco que quebra o formato, **não citado**)
- A e B têm **espaços de id independentes**. Os `Id=`/`PointeeId`/`AutomationTarget Id`
  do material de A **colidem** com os de B ao serem inseridos.
- É preciso **realocar** os ids *globais* do material importado para valores novos
  começando em `B.NextPointeeId`, e **bumpar** `B.NextPointeeId`.
- Sutileza perigosa: **nem todo `Id=` é global.** Muitos são ordinais locais (ex.:
  `<FloatEvent Id="…">`, `<AutomationEnvelope Id="0">`) e **não** devem ser tocados.
  Os globais que participam de referências cruzadas são `LomId`, `PointeeId` e os
  **alvos** apontados (`*Target Id=…`/`ControllerTargets.N Id=…`). Errar essa distinção =
  arquivo corrompido. Exige um mapa "id antigo→novo" aplicado **consistentemente** só nos
  campos certos.

### 2.4. ⚠️ Remap dos `PointeeId` das automações internas dos clips (o pior risco)
- Os 661 `ClipEnvelope` com `PointeeId` apontam para **parâmetros de device** da track de
  origem (em A). Ao cair na track casada de B, esse id **não existe** (ou pior, existe e é
  outro parâmetro). Sem remap → automação quebrada / Live em recovery.
- Remap correto exige enumerar, na track casada, a **lista ordenada de alvos de parâmetro**
  (device chain) em A e em B e mapear posição a posição. **Só é seguro se as cadeias de
  device das tracks casadas forem estruturalmente idênticas.** Se diferirem → abortar.

### 2.5. Outros riscos de formato no transplante
- 🟡 **Sends/returns:** clip pode automar um send; B precisa ter os mesmos returns
  (coberto se casarmos returns em 2.2).
- 🟡 **Amostras/arquivos referenciados** por AudioClips de A (caminhos relativos ao Project
  de A). Em B, o sample pode não estar na pasta → "Media files missing". **Mitigação:**
  detectar `SampleRef`/paths e avisar (ou copiar os samples para o Project de B — provável
  fase posterior).
- 🟡 **Versão do schema** A vs B (`MajorVersion`/`MinorVersion`/`Creator`). Misturar XML de
  versões diferentes de Live é arriscado. **Mitigação:** exigir versões compatíveis; avisar.
- 🟢 **Cenas/checksum/ponteiro:** mesmas conclusões do problema 1 (não existem).

### 2.6. Validação
- Pré-condições (track match + device-chain match + versão) checadas **antes** de escrever;
  abortar com relatório claro se falhar (seu requisito).
- Pós: nenhum id duplicado em B; todo `PointeeId` importado resolve para um alvo existente
  em B; jumps no range; `alsdiff B_original B_novo` mostra **apenas inserções** dos blocos.
- **Teste de fogo:** abrir B no Live, tocar os blocos transplantados, conferir que a
  automação dos clips responde (filtro/volume/etc.) e que não há "missing media".

### 2.7. Complexidade: **alta.** 2.3 e 2.4 são o cerne. Ver §4 (premissa que simplifica).

---

## 3. Problema 3 — **Merge** (esboço, sem profundidade)

Caso geral de unir dois sets. É o problema 2 levado ao limite: além de transplantar clips,
teria de **unir tracks que existem só em um lado**, resolver devices distintos, e a
renumeração/remap de ids vira global. Provavelmente também exige unir returns, locators e
configurações de projeto. **Recomendação:** tratar só depois que 1 e 2 estiverem sólidos;
muito provavelmente exige o modelo tipado (ver §4). Fica registrado no roadmap.

---

## 4. Ferramenta: continuar em Python ou ir para OCaml?

**Achado decisivo sobre o `alsdiff` (OCaml):** ele é **read-only** e seu parser é **lossy**
— extrai só os campos que o diff usa (xmlm) e **descarta o resto**; **não existe writer**
nem reserialização fiel. Operar pelo modelo dele e gravar de volta **perderia dados** e
não seria byte-fiel — exatamente o motivo que nos levou à cirurgia de texto.

| | Python (cirurgia de texto) | OCaml (`alsdiff`) hoje |
|---|---|---|
| Escrever `.als` fiel | ✅ já temos (byte-idêntico em identidade) | ❌ sem writer; parse lossy |
| Problema 1 (subset) | ✅ trivial | ❌ exigiria construir serializador |
| Problema 2 (analisar/validar: match de track, cadeia de device, alvos de param) | 🟡 dá, parseando subárvores | ✅ modelo tipado ajudaria **na análise** |
| Mutação + renumeração + remap fiel | ✅ controlável por texto | ❌ precisaria de writer fiel |

**Recomendação:**
- **Problemas 1 e 2: continuar em Python.** A exigência de saída byte-fiel + ausência de
  writer no OCaml torna o port hoje um mau negócio (teríamos que escrever um serializador
  fiel — o trabalho mais arriscado de todos).
- **OCaml entra como validador externo**, não como motor: rodar `alsdiff` (e o `--mode
  json`) para conferir que a mutação só produziu as mudanças esperadas. Já usamos isso.
- **Eventual port para OCaml** (alinhado ao goal de MR upstream) só compensa se/quando: (a)
  o algoritmo estiver provado em Python, e (b) decidirmos investir num **writer fiel** no
  alsdiff (preservando nós `Xml.t` originais e mexendo só nos campos certos). Aí o modelo
  tipado paga no problema 2/3 (enumerar device chains, alvos de parâmetro, ids). Antes
  disso, não. **Obs.:** alsdiff é um *diff* tool; adicionar comandos de **mutação** é uma
  expansão de escopo que o mantenedor upstream pode não querer — vale alinhar antes (§5 Q5).

---

## 5. Perguntas para você (responda aqui que eu sigo)

**Q1 — Subset: manter ou remover?** Prefere passar a lista de blocos a **manter** (`--keep
"T01,space jam"`) ou a **remover** (`--drop "gruta,falta organizar"`)? (Posso aceitar os
dois.)

> _sua resposta:_ os dois, default keep

**Q2 — Subset + reorder juntos?** No subset, quer poder já **reordenar** os blocos mantidos
na mesma operação, ou subset preserva a ordem original e reorder é separado?

> _sua resposta:_ se não for em modo --drop, respeita a ordem das musicas enviadas como argumento

**Q3 — Jumps pendurados (subset):** ao manter um clip cujo "jump to scene" aponta para uma
cena removida, o comportamento padrão deve ser **desabilitar aquele jump** (seguro) e
reportar? Ou prefere **abortar** e te deixar decidir bloco a bloco?

> _sua resposta:_ abortar, esse caso nao deve existir. pulos devem apontar para clips que pertencem a mesma musica

**Q4 — Transplante: A e B vêm do MESMO template?** Crucial. Seus sets são todos derivados
do mesmo projeto-base (mesmas 9 tracks + 4 returns, **mesmas cadeias de device**)? Se sim,
o remap de PointeeId (§2.4) fica seguro e posicional. Se as cadeias de device puderem
diferir entre A e B, o transplante de clips **com automação interna** é intrinsecamente
arriscado — aí eu restrinjo a primeira versão a exigir cadeias idênticas e abortar quando
diferirem. Qual é a realidade dos seus projetos?

> _sua resposta:_ A e B devem vir do mesmo template, mas vale validar e não confiar nessa regra. sempre posso ter mudado algo sem querer.

**Q5 — Samples (transplante):** os blocos que você moveria de A para B costumam ter
**AudioClips** (samples em disco), ou são majoritariamente MIDI? Se houver samples, quer que
a ferramenta **copie os arquivos** para o Project de B, ou só **avise** quais faltam?

> _sua resposta:_ os arquivos A e B são sets dentro do mesmo projeto e conseguem enxergar os samples entre si

**Q6 — Posição de inserção (transplante):** "inserir antes da música X em B" — confirma que
a âncora é o **nome do bloco** (`--before "gruta"`)? E os blocos movidos de A entram **na
ordem** em que você listar?

> _sua resposta:_ sim e sim.

**Q7 — Upstream:** vale eu sondar se o mantenedor do `alsdiff` topa comandos de **mutação**
no projeto (subset/transplant), ou preferimos manter como ferramenta Python à parte por
enquanto?

> _sua resposta:_ não precisa sondar o mantenedor do alsdiff, acho que já estamos divergindo demais da proposta original dele. mas vale investigar essa ferramenta que a ableton acabou de anunciar: https://www.ableton.com/en/live/extensions verifica qual a capacidade dela e se nos ajudaria de alguma forma. 

---

## 7. Investigação Q7 — Ableton Extensions SDK (anunciada 2026-06-02)

A Ableton lançou hoje, em **beta público**, a **Extensions SDK**: ferramentas em
**JavaScript/TypeScript sobre Node.js** que rodam **dentro do Live** e podem **ler e
reescrever a estrutura do Set** (tracks, clips, cenas, parâmetros, automação).

**Fatos relevantes (do material público; a API método-a-método está atrás do download
beta no Centercode):**
- Modela `Song`/`Scene`/`Track`/`ClipSlot`/`Clip`/devices/parâmetros/automação. "A Clip
  Slot is used to create and delete clips."
- **Node.js completo** — "todas as APIs core e pacotes NPM acessíveis" → tem acesso a
  **filesystem** (logo, um Extension poderia ler outro `.als` do disco).
- Dispara **one-shot** via clique-direito no Set aberto; aplica mudanças e para.
  **Não** roda programaticamente/headless nem no startup. **Sem** integração com M4L.
- Edita **o Set aberto** (não há multi-set nativo).
- Requer **Live 12 Suite Beta (12.4.5+)**. Estamos no 12.3.8.
- ⚠️ Incertezas que dependem da doc do SDK (download beta): se o `Scene` tem **reorder/move**
  direto (a LOM clássica do M4L só criava/deletava/duplicava cenas) e o escopo exato do `fs`.

**Por que isso importa para nós (a grande vantagem):** editar pelo **modelo de objetos do
próprio Live** faz o Live cuidar da **alocação de ids** e da **integridade dos ponteiros de
automação** automaticamente — **elimina nossos dois piores riscos** (§2.3 renumeração de
ids e §2.4 remap de PointeeId) e zera a questão de **fidelidade byte-a-byte** (quem grava é
o Live).

**Avaliação por problema:**
- **Reorder (feito) / Subset (P1):** set único. Bom encaixe **se** o `Scene` permitir
  reorder/delete. Subset = deletar cenas indesejadas com o Live mantendo tudo consistente.
- **Transplante (P2):** é onde o SDK mais ajudaria — a inserção em B passa pela API → Live
  resolve ids e pontees. Como tem `fs`, um Extension rodando em B poderia **ler A do disco**;
  porém o SDK só modela o **set aberto**, então ainda precisaríamos parsear A por fora (nosso
  parser) ou usar um fluxo "exporta blocos de A → importa em B".

**Custos / trade-offs vs. o nosso Python:**
- Exige **upgrade para Suite Beta 12.4.5** — beta instável, num projeto de 280 MB, contra o
  objetivo de "não travar". O Python roda **offline, sobre o arquivo, sem abrir o Live**
  (mais rápido p/ sets gigantes, batch-friendly, determinístico).
- Reescrever em JS/TS; API beta pode mudar; one-shot (sem CLI/headless).

**Recomendação:**
1. **Manter o caminho Python** (offline, file-based) como motor atual — já provado no reorder,
   trivial no subset.
2. **A Extensions SDK é o lar natural de longo prazo do transplante/merge**, por terceirizar
   ao Live as partes mais perigosas (ids, pontees, fidelidade). Vale um **spike** quando
   tivermos acesso à doc/beta do SDK.
3. **Bloqueio para avaliar a fundo:** a referência de API está atrás do download beta. Para
   ir além, preciso que você entre no programa beta e compartilhe a doc do SDK (ou instale),
   aí avalio com precisão `Scene.reorder`/move e o escopo de `fs`.

> **Decisão pendente sua:** seguimos a **Fase A (subset) em Python agora** (desbloqueado), e
> em paralelo você decide se quer entrar na beta do Suite p/ explorarmos o SDK no transplante?

---

## 6. Sequência sugerida de execução

1. **Fase A — Subset (Python).** Baixo risco; entrega rápida; reaproveita a engine atual.
2. **Fase B — Transplante (Python), com pré-condições rígidas.** Primeiro o **validador**
   (match de track + cadeia de device + versão) que **recusa e aponta** divergências; só
   então a mutação (insert + renumeração de ids + remap de PointeeId + remap de jumps).
   Começar exigindo template idêntico (resposta da Q4).
3. **Fase C — Merge.** Só depois; reavaliar Python vs OCaml à luz do que aprendermos em B.
