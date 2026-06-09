# Plano: reordenar blocos de música (cenas) na Session View via XML do `.als`

> Documento de **discussão / planejamento**. Nada foi implementado ainda.
> Objetivo: validar a viabilidade de reordenar arbitrariamente os "blocos de música"
> de um Live Set na Session View manipulando o XML, e desenhar os passos + riscos.

---

## 1. Problema (nas suas palavras)

O Live Set organiza músicas em **blocos contíguos de cenas**:

- cena 1–5 = música A, cena 6 em branco (separador)
- cena 7–15 = música B, cena 16 em branco
- … e assim por diante.

Reordenar músicas (ex.: de `A B C D E` para `B A E D C`) é trabalhoso à mão, o que
desencoraja experimentar versões alternativas do set. Queremos uma ferramenta que receba
a ordem desejada de blocos e produza um **novo** `.als` com os blocos reordenados.

### Estrutura de um bloco (sua descrição)

Um bloco de música é, de cima para baixo:

1. uma **linha (cena) com clips STOP** (para parar a música anterior);
2. uma **cena totalmente em branco**;
3. **algumas cenas com clips** (nem toda track tem clip em toda cena);
4. uma **cena totalmente em branco** demarcando o fim da música.

> ⚠️ **A confirmar (ver §7, Perguntas):** preciso fixar a regra exata de fronteira de
> bloco — o que pertence ao bloco e o que é separador. Disso depende a auto‑detecção.

---

## 2. O que eu já consegui observar no seu arquivo

Arquivo analisado:
`/Users/amello/Music/LIVESet/LIVE Project/00_Live_XA_Remix_Housy_VS2_ZUNIDOvPOSv3.1.testeNAoSeguirMAsSEguiTEmNovidadeNoTopo.als`

| Fato | Valor |
|---|---|
| Formato | `.als` = **gzip** de um XML único |
| Tamanho | ~23 MB comprimido → ~4,7 milhões de linhas de XML |
| Versão | Ableton **Live 12.3.1** (`MajorVersion="5" MinorVersion="12.0_12300"`) |
| Bloco `<Scenes>` | linhas ~4.730.483 a ~4.738.373 do XML |
| Nº de cenas | **343** (`<Scene Id="0">` … `Id="342"`) |
| Tracks com session clips | ~42 `ClipSlotList` (audio/midi/grupos/returns) |
| `ClipSlot` no total | ~6.517 |

### Anatomia de uma `<Scene>` (real, sanitizada)

```xml
<Scene Id="533">
    <FollowAction>
        <FollowTime Value="4" />
        <IsLinked Value="true" />
        <LoopIterations Value="1" />
        <FollowActionA Value="4" />        <!-- tipo da ação A -->
        <FollowActionB Value="0" />
        <FollowChanceA Value="100" />
        <FollowChanceB Value="0" />
        <JumpIndexA Value="0" />           <!-- 0 ou 1 (NÃO é índice absoluto) -->
        <JumpIndexB Value="0" />
        <FollowActionEnabled Value="false" />   <!-- 🟢 desativada nas cenas -->
    </FollowAction>
    <Name Value="" />                      <!-- nome da cena (vazio aqui) -->
    <Annotation Value="" />
    <Color Value="-1" />
    <Tempo Value="120" />
    <IsTempoEnabled Value="false" />
    <TimeSignatureId Value="201" />
    <IsTimeSignatureEnabled Value="false" />
    <LomId Value="0" />
    <ClipSlotsListWrapper LomId="0" />
</Scene>
```

> Observação importante: o `Scene Id` aqui é **533** (não a posição 0). Confirma que o
> `Id` é um **identificador LOM global**, não a posição na lista — a ordem é dada pela
> **posição no XML**. Mesma lógica vale para `ClipSlot Id`.

**Reavaliação do "complicador dos jumps" (boa notícia parcial):** medições no arquivo:

- **Todas as 343 cenas** têm `FollowActionEnabled="false"` e `JumpIndexA ∈ {0,1}` →
  **as follow actions de CENA estão desligadas e não usam salto absoluto.** Ou seja,
  reordenar cenas **não** quebra jump de cena. ✅
- Porém, no arquivo inteiro há **14.613** ocorrências de `JumpIndexA`, das quais só
  **343** estão nas cenas. **As outras ~14.270 estão dentro de CLIPS.** 🔴
  → **O complicador real são as follow actions de CLIP**, exatamente como você previu —
  só que no nível do clip, não da cena.
- **A confirmar (Fase 0):** se o `JumpIndex*` de clip referencia um **índice absoluto de
  cena** (precisaria remapear por `P`) ou se é relativo/interno ao clip (não precisa).
  A maioria desses 14k clips tem follow action provavelmente **desabilitada** também
  (default do Live 12) — então o nº de jumps *ativos que apontam para cena* pode ser
  pequeno. **Medir quantos têm `FollowActionEnabled="true"` é a verificação‑chave.**

---

## 3. Como a reordenação funciona, conceitualmente

Toda a operação é **uma permutação `P` das posições de cena** (de "posição antiga" para
"posição nova"). O ponto central de correção é: **`P` precisa ser aplicada de forma
consistente em vários lugares**, não só na lista de cenas.

### 3.1. As cenas são posicionais e paralelas às tracks

Na Session View, "cena N" não é um objeto que carrega os clips. Os clips moram **dentro
de cada track**, numa lista ordenada de `ClipSlot` (a `ClipSlotList` da track). O
**N‑ésimo `ClipSlot` de cada track** pertence à **N‑ésima cena**. A cena em si só
carrega metadados (nome, cor, tempo, follow action).

Logo, reordenar cenas = reordenar, **em paralelo (lockstep)**:

1. os elementos `<Scene>` dentro de `<Scenes>`; **e**
2. os elementos `<ClipSlot>` dentro de **cada** `ClipSlotList` de **cada** track,
   com a **mesma** permutação `P`.

Se a permutação não for idêntica em todas as listas, os clips "deslizam" para cenas
erradas. Esse é o invariante mais importante da ferramenta.

```
            cena0  cena1  cena2  cena3 ...
Track Drums [ A ][ B ][   ][ C ] ...   ┐
Track Bass  [   ][ B ][ b ][   ] ...   ├─ todas reordenadas pela MESMA P
Track Synth [ a ][   ][ s ][ C ] ...   ┘
<Scenes>    [S0 ][S1 ][S2 ][S3 ] ...   ┘ (idem)
```

### 3.2. Referências a índices absolutos que precisam ser **remapeadas** por `P`

Tudo que guarda um número de cena precisa virar `P(número)`:

- 🟢 **`JumpIndex*` de CENA** — medido: desativado em todas as 343 cenas, valores 0/1.
  Na prática provavelmente **não precisa** remapear; por segurança, podemos remapear
  mesmo assim (custo zero) ou só zerar.
- 🔴 **`JumpIndex*` de CLIP** (follow actions de clip) — ~14.270 ocorrências. **Este é o
  ponto crítico.** A confirmar se é índice **absoluto de cena**; se for, remapear por `P`
  **apenas nos clips com `FollowActionEnabled="true"`** (os desativados são default e
  inertes, mas remapear todos também é seguro e mais simples).
- 🟡 **Ponteiro de cena selecionada/atual** do Live Set — os nomes `CurrentSceneIndex`/
  `SelectedScene` **não existem** neste arquivo. **A confirmar nome real** (provável algo
  em `ViewStates`/seleção da Session); se existir, remapear ou zerar.
- 🟡 **`ClipSlot Id`** — observei um `ClipSlot Id="533"` (número alto), o que sugere que
  o `Id` é um **identificador LOM global**, não a posição. Se for global, **movemos o
  `ClipSlot` inteiro** (Id + conteúdo) e a ordem é dada pela posição no XML — sem mexer
  no Id. **A confirmar:** se o Live exige `ClipSlot Id == posição`; em caso afirmativo,
  renumerar após reordenar.

### 3.3. O que **não** é afetado (a confiar / confirmar)

- **Locators / Cue Points / Arrangement**: referenciam *tempo* na Arrangement View, não
  índice de cena. Reordenar a Session **não** deve mexer neles. (A confirmar que não há
  acoplamento.)
- **Conteúdo dos clips** (notas MIDI, samples, automação): viaja junto com o `ClipSlot`,
  intacto.
- **Tempo / fórmula de compasso por cena**: estão *dentro* de `<Scene>`, então viajam
  junto com a cena automaticamente. ✅

---

## 4. Modelo de "bloco" e interface (opção escolhida: por nome)

Você escolheu **especificar a ordem por nome de bloco** (opção 1) e topou **nomear a
primeira cena de cada bloco** no Ableton. Plano:

1. Você nomeia a 1ª cena de cada música (ex.: `>> VAGALUME`, `>> ZUNIDO`, …). Sugiro um
   prefixo/convenção (ex.: começar com `>>`) para a ferramenta reconhecer "início de
   bloco" sem ambiguidade.
2. A ferramenta varre as cenas em ordem e **fatia em blocos**: um bloco vai do início
   nomeado até imediatamente antes do próximo início (ou fim da lista), respeitando a sua
   convenção de separadores (stop‑row + cena em branco).
3. Você passa a nova ordem como lista de nomes: `["ZUNIDO", "VAGALUME", ...]`.
4. A ferramenta monta a permutação `P` concatenando os blocos na nova ordem (cada bloco
   carrega **todas** as suas cenas internas, incluindo seu separador), e aplica `P`
   conforme a §3.

> Detalhe a decidir (§7): se o **separador em branco** pertence ao fim do bloco anterior
> (viaja com ele) ou é regenerado entre blocos. Mais simples e previsível: **cada bloco
> inclui seu próprio separador final**, então reordenar blocos preserva 1 separador entre
> cada música automaticamente.

---

## 5. Plano de execução (faseado — opção 3: Python primeiro, depois OCaml)

### Fase 0 — Validação read‑only (CONCLUÍDA ✅)
- [x] Confirmar que `.als` é gzip de XML único.
- [x] Localizar `<Scenes>`, contar cenas (343), ver anatomia de `<Scene>`.
- [x] Confirmar existência de `JumpIndexA/B` nas cenas → **desativadas, valores 0/1**.
- [x] Descobrir onde estão os jumps de verdade → **~14.270 em clips, 343 em cenas**.
- [x] Confirmar que `Scene Id`/`ClipSlot Id` é LOM global (ordem = posição no XML).
- [x] **Clips com `FollowActionEnabled="true"`: 7.231** (de 14.270).
- [x] **`JumpIndex*` de clip É índice absoluto de cena** (valores 2..168, range 0..342),
      e só conta quando **`FollowActionA/B == 9`** (= "Jump to scene"). → **remapear**.
- [x] Ponteiro de cena selecionada/atual: **não existe** (sem `SelectedScene`/
      `CurrentSceneIndex`). Nada a remapear aqui. ✅
- [x] **`ClipSlotList` com 343 slots: 19** (+4 vazias de returns). Regra: reordenar TODA
      `ClipSlotList` com nº de filhos diretos == nº de cenas.
- [x] ⚠️ **ElementTree NÃO faz round-trip fiel** (reescreve aspas `"`→`&quot;`,
      `&#x0A;`→`&#10;`, +5KB sem mudar nada). → a ferramenta usa **cirurgia de texto**.

> Estrutura do `<ClipSlot>` é **aninhada**: slot externo contém um interno com `<HasStop>`
> e (quando há clip) `<Value><MidiClip|AudioClip>`. Scanner é depth-aware. O 1º
> `<ClipSlotList>` do texto não tem 343 — por isso selecionamos por contagem, não posição.

### Fase 1 — Protótipo em Python — **`scripts/als_reorder.py`** (FEITO e VALIDADO ✅)

Implementado com stdlib pura (sem lxml), cirurgia de texto, scanner XML depth-aware **e
quote-aware**. Subcomandos: `inspect`, `reorder --order "B,A,E"`, `selftest`.

- [x] **`selftest` PASSA** numa fixture sintética: detecção de blocos por prefixo `>>`,
      permutação, reorder de cenas + ClipSlots em lockstep, e remap de jump (clip não vaza
      para outras cenas).
- [x] **`inspect` no set real** lê os 280MB: 343 cenas, 19 ClipSlotLists, 0 blocos
      (esperado — o set ainda não tem nomes `>>`), 343 cenas de cabeçalho.
- [x] **Bug corrigido:** o próprio prefixo `>>` quebrava o parser naive (o `>` dentro de
      `Value=">>A"` era confundido com fim de tag). Scanner agora é **quote-aware**.
- [x] **Enum de jump corrigido:** "Jump to scene" é `FollowActionA/B == 9` (não 8).
- [x] **Preservação byte-a-byte:** permutação **identidade** no set real de 280MB →
      saída **byte-idêntica** ao original (mata o risco R4). ✅
- [x] **Reorder real validado:** 5 blocos injetados (`>>B0..>>B4`), ordem `B2,B0,B4,B1,B3`
      → 343 cenas / 19 CSL / ordem correta; **multiset de cenas idêntico**; o `len_delta`
      de poucos bytes é **100% explicado** pelo remap dos jumps (nº de dígitos; ex.: cena
      `126→6`, `91→214`); `predicted == observed`.
- [ ] **Pendente (com você):** rodar `reorder` num set REAL já nomeado com `>>`, validar
      com `alsdiff` (diff só deve mostrar reorder + remaps) e **reabrindo no Ableton**.

**Critérios de aceite da Fase 1**
- Live abre o arquivo sem "recovery"/erro.
- Cada música toca igual, só que na nova ordem de blocos.
- Follow actions/"jumps" continuam apontando para a cena certa (dentro do bloco certo).
- `alsdiff` não acusa mudanças inesperadas (devices, automação, etc. intactos).

> **Status: `scripts/als_reorder.py` está pronta para uso.** Falta só o teste de fogo:
> você gerar um `.als` com a 1ª cena de cada bloco nomeada `>> NOME`, rodar `inspect` e
> `reorder`, e abrir o resultado no Live.

### Fase 2 — Subcomando em OCaml no `alsdiff` (para MR upstream)
Depois de validado, portar a lógica para um subcomando, p.ex. `alsdiff reorder-scenes`
(nome a definir), reusando o parser/escritor XML e os tipos do projeto.

- Reusar leitura/escrita gzip+XML já existentes no `bin/`/`lib`.
- Modelar a permutação e o remapeamento sobre os tipos `Scene`/`ClipSlot` (atenção aos
  padrões do `CLAUDE.md`: derives PPX, `[@id.id]`, MainTrack singleton — embora cenas não
  envolvam MainTrack).
- Testes em `test/` espelhando `lib/live/*`, usando um set pequeno de fixtures (criar um
  `.xml` de teste com poucas cenas e 2–3 tracks + follow actions com jump).
- Rodar `opam exec -- dune runtest` (serializado; nunca dois `dune` concorrentes).

---

## 6. Riscos e pontos de atenção

| # | Risco | Mitigação |
|---|---|---|
| R1 | **Remapeamento de `JumpIndex*` de CLIP** incompleto → jumps apontam pra música errada (cena: desativado, baixo risco) | Confirmar semântica do jump de clip; aplicar `P` a todos os índices de cena em clips; testar com clip que salta pra fora do bloco movido |
| R2 | **Lockstep quebrado** entre `<Scenes>` e as `ClipSlotList` → clips deslizam | Invariante central: mesma `P` em todas as listas; assert de tamanho 343 em cada lista |
| R3 | **`ClipSlot Id` posicional** (se Live exigir Id==posição) | Confirmar; se preciso, renumerar Ids após reordenar |
| R4 | **Re‑serialização altera XML demais** (Live recusa/recupera) | Usar parser que preserva a árvore; diffar com `alsdiff`; nunca sobrescrever original |
| R5 | **Follow actions de clip** com salto absoluto não tratadas | Confirmar na Fase 0; incluir no remapeamento |
| R6 | **Ponteiro de cena selecionada** apontando pra índice antigo | Remapear ou resetar para 0 |
| R7 | **Convenção de bloco ambígua** (o que é separador) | Fixar regra com você (§7) antes de codar a detecção |
| R8 | **Arquivo gigante** (4,7M linhas) | Stream/lxml; performance ok pra uso offline |
| R9 | **Corrupção/perda** | Sempre gerar arquivo novo + backup; validar reabrindo no Live |

---

## 7. Perguntas rápidas já respondidas

1. **Fronteira de bloco / separador:** a sequência exata entre duas músicas é
   *cena‑STOP* **seguida de** *cena‑em‑branco*? Ou a cena‑STOP é a **primeira linha do
   próximo bloco** (para o anterior) e o separador é só **uma** cena em branco? Me
   descreva o "sanduíche" exato de cenas entre o fim da música X e o início da Y.

   Acho que a resposta da proxima pergunta deixa essa aqui um pouco desncecessária, mas a organizacao atual é:  
   - musica A:
     - cena STOP
     - cena em branco
     - cena com clip
     - cena com clip
     - cena em branco
   - musica B:
     - cena STOP
     - cena em branco
     - cena com clip
     - cena com clip
     - cena com clip
     - cena em branco
   Isso no inicio do projeto. Já para o final tem muita coisa desorganizadada ainda.

2. **Convenção de nome:** topa um **prefixo fixo** no nome da 1ª cena de cada bloco
   (ex.: `>> NOME`) pra eu detectar início de bloco sem heurística frágil? Algum nome de
   cena que NÃO seja início de bloco (que eu deva ignorar)?

   Acho válido adotar essa convenção sim, com o prefixo inclusive.

3. **O que move junto:** o separador em branco deve **viajar com o bloco** (cada música
   leva seu separador final) — concorda? Assim sempre sobra exatamente 1 separador entre
   músicas após reordenar.

   Sim, concordo.

4. **Antes/depois do primeiro bloco:** existem cenas "de cabeçalho" no topo do set (intro,
   testes — o nome do arquivo sugere "novidade no topo") que devem ficar **fixas** e fora
   da reordenação?

   vamos adotar o formato da pergunta 2 de forma rigida. o arquivo atual ainda nao tem isso, mas pode assumir como uma premissa e vou gerar um arquivo nesse formato em breve. atualizo aqui com o nome do arquivo quando fizer.

---

## 8. Conclusão de viabilidade

**É viável.** A Session View no `.als` é totalmente representada no XML; reordenar blocos
é uma **permutação de cenas aplicada em lockstep às `ClipSlotList` de todas as tracks**,
mais o **remapeamento dos índices de cena** que aparecem nas follow actions. Medições
refinaram o quadro: o salto de **cena** está desativado (baixo risco), e o complicador
real, como você previu, são as **follow actions de CLIP** (~14k ocorrências) — falta só
confirmar se referenciam índice absoluto de cena e quantas estão ativas. O conteúdo dos
clips viaja intacto dentro dos `ClipSlot`. Os principais cuidados são (a) manter o
lockstep, (b) remapear os índices de cena nos clips ativos, e (c) re‑serializar sem
perturbar o resto do XML — e para isso temos o `alsdiff` como verificador natural.

Próximo passo quando você acordar: responder as 4 perguntas do §7 e fechar a Fase 0
(as 5 verificações pendentes) antes de eu escrever o protótipo Python.
