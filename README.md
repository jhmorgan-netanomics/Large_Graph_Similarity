# Large_Graph_Similarity

Large_Graph_Similarity (LGS) reduces two networks to a similarity score between 0 and 1, where 1 means "no measurable difference". A network compared with itself scores exactly 1, whatever order its nodes and links are listed in.

## Layout

| Path | Contents |
|---|---|
| `julia_env/src/Large_Graph_Similarity.jl` | The measures, constructors, feature builders, `network_comparator`, and `reference_comparator` |
| `julia_env/src/CLI.jl` | The command-line interface |
| `julia_env/Self_Identity_Tests.jl` | Regression tests: self-identity, undefined measures, two-mode networks, node measure comparison, network type, core decompositions, angular similarity, reference comparison |
| `julia_env/Benchmark_Replication_Tests.jl` | Each measure against results saved from ORA, igraph, scoredec, Pajek, Python, and R |
| `julia_env/old_tests/` | Development scripts that predate version 0.2.0 and still refer to the raw and asinh scores |
| `julia_env/build_app_*.jl` | PackageCompiler build scripts |
| `Test_Data/` | Test networks |
| `LGS_Pending_Fixes_28Oct2026.md` | Record of the October 2026 findings and their repair |

## Running the tests

From this folder:

```bash
julia -t auto --project=julia_env julia_env/Self_Identity_Tests.jl
julia -t auto --project=julia_env julia_env/Benchmark_Replication_Tests.jl
```

The first script includes a reference comparison on observed networks: Balikatan 2022 as the reference against Pac Rim 2023 (`Pac Rim Day 1.xml`) and Pac Sentry 2025 (`PAO_Day_6.xml`), each file's Agent x Agent - All Communication network. A file that is absent from `Test_Data` is reported as broken in the test summary.

The second script compares each measure on Balikatan's All Communication network with the benchmark files in `Test_Data` and writes `Test_Data/benchmark_replication_results.csv`. Set `LGS_SOURCE` to another copy of `Large_Graph_Similarity.jl` to run the same benchmarks against it.

Rebuild the app after any change to `src`; the compiled app does not pick up source changes on its own.

## One-mode networks

`network_comparator(edges_1, nodes_1, edges_2, nodes_2; directed, weighted, ...)` compares two one-mode networks. It returns one score (`overall_similarity`), the aligned features, the contribution of each feature type, and three tables to read alongside the score:

- `node_measure_diagnostics` gives, for each node measure, the divergence that entered the score, the divergence expected from node sampling alone with its 95% level, and the scaled Wasserstein-1 distance.
- `undefined_features` lists every feature that is undefined (NaN) in one or both networks. Those features are left out of the score. One that is undefined in only one network is a difference the score does not contain.
- `comparison_info` names the feature set, the bin width, and the version that were used, with the network type that was declared and the type found in each network's links.

- `edge_agreement` holds the node overlap and link counts behind the angular similarity of the links (see below).
- `weight_scale` shows whether two weighted networks differ in their heaviest link or in their distribution of weights (see below).

The CLI writes one file of each kind: `<name_1>_<name_2>_similarity_scores.csv` (the score, with the angular similarity of the links), `_comparison.csv`, `_type_contributions.csv`, `_node_measure_diagnostics.csv`, `_edge_agreement.csv`, `_weight_scale.csv`, `_undefined_features.csv`, and `_comparison_info.csv`.

### Angular similarity of the links: a second reading

Beside its own score, the comparison reports the angular similarity of the two networks' link weights, `1 - 2 acos(c) / pi`, where `c` is the cosine between the two weight vectors, on the weights and on link presence (`edge_angular_similarity`, `edge_angular_similarity_binary`). It is not part of the score.

The two readings answer different questions. The angular similarity asks whether the same links are present at the same weights. The score asks whether the two networks have the same structure. When the links largely agree and the score is low, the type contributions, the feature table, and the node measure diagnostics show which measures a small change in links has moved. The two are not on the same scale, so a gap between them is a prompt to read those tables, not an error in either.

The angular similarity compares link by link, so it means something only when the nodes of the two networks are the same entities: each pair of nodes must have been able to be tied in both. This is checked.

- **Matching.** Nodes are matched on the node key (see *Node key* below): their ids by default, or their names when the caller says so.
- **Overlap.** The node overlap is the number of nodes in both networks divided by the number in either, counting every node in a node table or on a link.
- **Threshold.** The angular similarity is computed only when the overlap is at least `min_node_overlap` (default 0.75; CLI `--min-node-overlap`). Below it the value is `NaN`, and `edge_agreement` gives the overlap and the reason.
- **Unequal node sets.** The node sets need not be identical. A node present in one network only is treated as having no links in the other, so its links count as links found in one network only.
- **Switching it off.** `match_nodes_on = nothing` (CLI `--match-nodes-on none`) skips it, for networks that are not on the same nodes.

### Node key: ids by default, names when asked

Most comparisons are structural: the two networks need not share a single node, and nothing has to be said about node identity. The key matters in one situation, when the same network is compared under different ids, for example after ids were changed to meet an external standard. Something must then be constant between the two node lists, and the caller says what it is.

- **Default.** Nodes are identified by id. Nothing is guessed.
- **Names.** `match_nodes_on = :name` (CLI `--match-nodes-on name`) identifies nodes by name: the node list's `name` or `label` column, whichever both lists have. Any other column can be named in the same way.
- **Not found.** If the field is not in both node lists, that is reported and ids are used. `comparison_info` records the key that was used (`node_key`).
- **What the key does.** Both networks are put onto the key before anything is measured. The same network under two id schemes is then the same input: it scores exactly 1, and the angular similarity of its links is 1. Node measures are returned under the ids that were given.
- **Limits.** Two nodes of one network with the same name are an error, because their links would be merged. A node without a name keeps its id. If neither ids nor names are constant, there is no key; the score is still computed, and the angular similarity is left out.
- **Reference mode.** The reference fixes the key (`reference_analysis(...; match_nodes_on)`). A network that lacks the field is identified by id and still compared.

### Weight scale: one heavy link, or the distribution of weights

Normalized degree is divided by the network's largest link weight. That puts networks with different weight scales on one scale, and it is ORA's scaled degree. The largest weight is set by one link, so when that link changes, every node's normalized degree changes with it, and the score moves by more than one link would suggest. The comparison reports this so that the two cases can be told apart. None of it enters the score.

`weight_scale` has one row per statistic, with each network's value and the ratio of the larger to the smaller:

| Statistic | Reading |
|---|---|
| `largest_weight` | The divisor. Directed networks: over all links. Undirected networks: over links between two different nodes |
| `second_largest_weight`, `links_at_largest_weight` | Whether the largest weight stands alone |
| `percentile_95`, `median`, `mean`, `links` | Where the rest of the weights lie |
| `sqrt_jsd_weights` | Divergence of the two weight distributions, weights as they are |
| `sqrt_jsd_weights_over_largest` | The same after each network's weights are divided by its largest weight |

The two divergences are computed as for a node measure (geometric axis, same bin width). When the largest weights differ, the lower percentiles agree, and the first divergence is small while the second is large, one heavy link differs and the distributions do not. When the first divergence is itself large, the distributions of weights differ.

`node_measure_diagnostics` marks the measures concerned in the column `divided_by_largest_weight`: `in_degree_normalized` and `out_degree_normalized` for directed weighted networks, `total_degree_normalized` for undirected weighted networks, none for two-mode networks. `comparison_info` states the finding in words (`weight_scale_note`). The table is empty for networks compared as binary.

### Network type is tested, not assumed

Before a feature set is chosen, each network's links are tested for direction and for weight, whatever was declared and however the network was read (edge lists or ORA XML).

| Declared | Found in both networks | Compared as |
|---|---|---|
| Directed | Symmetric: every link between two different nodes has a reverse link of the same weight | Undirected, one link per pair at that weight |
| Weighted | Every weight is 1 | Binary |
| Directed or weighted | Only one network is symmetric, or only one has all weights 1 | As declared, because both networks are measured on one feature set |
| Undirected or binary | Links run one way, or weights vary | As declared: setting direction or weight aside is a choice |

A symmetric network has no direction to measure. On the directed feature set its in and out measures are copies of one another, so one difference is counted several times. ORA writes co-occurrence networks as directed with both arcs; they are found symmetric and compared as undirected.

`comparison_info` records `declared_type`, `found_type_1`, `found_type_2`, and a `type_note` saying what was changed or why the declaration stands. The app's single-network analysis runs the same tests. The constructors, called directly, do what they are told.

### How node measures are compared

Each node measure (degree, reach, clustering, PageRank, and so on) contributes one feature: the distance between the two networks' distributions of that measure. Every measure is compared the same way.

1. **Scale.** The typical value `s` is the median nonzero magnitude of the two distributions pooled, with the two networks weighted equally.
2. **Geometric axis.** Each node value `x` is placed at `asinh(x / s)`. The axis is linear near zero and logarithmic in the tails, so a heavy tail does not empty the bins that hold most nodes.
3. **Bins of fixed width.** The range runs from the joint minimum to the joint maximum, always includes zero, and is divided into bins of width `bin_width` (default 0.125; CLI `--bin-width`). A wider distribution gets more bins of the same width.
4. **Linear binning.** Each node is shared between the two nearest bin centres, so a small change in a value moves a small amount of mass.
5. **Divergence.** The feature is the square root of the Jensen-Shannon divergence between the two binned distributions.

The k-core and s-core decompositions are compared in the same way, one feature per decomposition (`jsd_k_core_in`, `jsd_s_core_undirected`, and so on; `jsd_source_k_core` and `jsd_target_k_core` for two-mode networks). A node's core number is a node-level measure, and under linear binning neighbouring core levels share mass. Versions before 0.3.0 used one feature per core level, which treated a move from level 12 to level 13 as two unrelated differences.

Only the triad census carries a group weight. The community features (the share of nodes in each community) are left at one feature each: they sum to 1, so their combined size is bounded, and they supply little of the distance.

Scores computed with different bin widths are not comparable. Scores from versions 0.2.0 and 0.3.0 are not comparable with each other. Versions before 0.2.0 returned two scores (`overall_similarity_raw`, `overall_similarity_asinh`) from equal-width histograms with 20 bins; those scores are not comparable with the present one.

### The weighted triad census in the score

For weighted networks the triad census is taken over a grid of weight thresholds chosen for each network. Each triad type contributes three features: the area under its density curve (`<triad>_AUMC_density`), its peak density (`<triad>_peak_density`), and the position of its peak within the grid (`<triad>_peak_position`), from 0 at the lowest threshold to 1 at the highest, on the log scale. A position does not depend on the unit of the link weights. It is undefined for the empty triad (003) and for a type that never occurs, and is compared only where the type occurs in both networks. The census table keeps the peak threshold itself (`peak_tau`) for interpretation.

In a sparse network most triad types are densest at the lowest threshold, so their position is 0 in both networks and the feature adds nothing to the distance. That is intended: the position counts where a type peaks in the interior of the grid, and is silent otherwise.

**Sampling noise.** Two networks that differ only by chance still show a nonzero divergence on every node measure, and more so when they are small. `node_measure_diagnostics` gives two reference levels for each measure: `expected_noise`, the mean divergence between two samples of these sizes drawn from one distribution, and `noise_95`, the level such a divergence stays under 95 times in 100. A `js_divergence` below `noise_95` is not evidence of a difference. Neither level enters the score.

- *How they are computed.* With hard bins the noise follows a chi-square distribution on the number of occupied bins less one. Linear binning lowers it by a smoothing ratio that depends on where values sit within their bins: two thirds when they are spread evenly, 1 at bin centres, one half at bin edges. The ratio is taken as two thirds a priori and informed by the measure's own values, so that small networks lean on the prior and larger ones on their data. A small-sample term (Williams' correction, held to at most 1.25) is applied.
- *How well they hold.* Against random splits of 946 node measures from 30 network pairs (47 to 3,200 nodes), the estimate over the true noise has median 1.03 and lies between 0.94 and 1.13 in 90 of 100 cases. The formula used before version 0.4.2 was about 50% too high.
- *What they do not cover.* Nodes are treated as independent draws. A change in one link can move many nodes together, and that variation is not included. PageRank, the k-core and s-core numbers, and degree normalized by the largest weight vary several times more between two networks of one process than these levels say; for those measures the levels are a lower bound.
- *The proper route.* The uncertainty of a score, or of a measure, under a stated assumption about how a network was observed belongs to the Network_Credible_Intervals project, which deforms and reconstructs real networks and returns an interval for any measure. Its validation is in progress. An LGS score can be passed to it as a metric, with `compare_to_reference` keeping the reference fixed.

## Two-mode (bipartite) networks

Two two-mode networks, such as two Agent x Hashtag networks, are compared with `bipartite = true` (CLI: `--bipartite true`, or automatically for an ORA network whose source and target nodesets differ).

**Declaring the modes.** The node table needs a column naming each node's mode (`mode_column`, CLI `--mode-column`). Without it, senders are taken as one mode and receivers as the other, which works only when no node both sends and receives and no node is isolated. A link within a mode, or an id listed in both modes, is an error.

**Direction is kept.** The source mode is measured by what it sends and the target mode by what it receives. Links running back from the target mode to the source mode are allowed and are measured separately.

**Feature set.** Only measures defined for two modes are used, and every node-level distribution is compared mode against mode:

- sizes of each mode, weak components, link weight statistics;
- density as links / (n_source x n_target), reciprocity when links run both ways;
- degree assortativity as the correlation between a sender's out-degree and its receiver's in-degree;
- four-cycle clustering (Robins and Alexander) and pairwise clustering (Latapy);
- per mode: degree and strength, two-step reach, k-core, PageRank, SALSA hub or authority.

Transitivity, local clustering, the triad census, strong components, and self-loop statistics are not used.

**Comparisons that are refused.**

1. A two-mode network against a one-mode network.
2. Two-mode networks whose mode names differ (override with `match_modes = false`).
3. Two-mode networks that each run one way, in opposite directions, such as Agent x Tweet against Tweet x Agent.

**Scores are not comparable across kinds.** A two-mode score rests on a different feature set from a one-mode score.

### Projection

To compare a two-mode network with a one-mode network, project it first: `project_bipartite(edges, nodes; onto = "Agent")`, or `--project-onto Agent` on the command line. The projection is undirected and is compared as a one-mode network.

- Ties are weighted by Newman's method by default (`weighting = :newman`); `:count` and `:jaccard` are available.
- A projection is far more clustered and dense than an observed network, because every node of the dropped mode becomes a clique. Treat a projected-against-observed score with caution.
- A node of the dropped mode with k links creates k(k-1)/2 ties. Projections above `max_pairs` are refused.
- The mode matters. Agent x Tweet - Sender projected onto agents is empty, because no two agents send the same tweet.

## Comparing a reference network with many networks

To compare one network with many others (reconstructions of it, or simulated networks that should resemble it), analyse the reference once and compare each network with the saved analysis. Only the other network is measured in each comparison. The result is the same, to the last digit, as `network_comparator` gives for that pair.

```julia
networks = [("model_a", edges_a, nodes_a), ("model_b", edges_b, nothing),
            ("model_c", () -> read_my_network("model_c"))]      # read when its turn comes
batch = reference_comparator(reference_edges, reference_nodes, networks;
                             directed = true, weighted = true, seed = 11, reference_dir = "reference")
batch.summary        # one row per network
```

From the command line, network 1 is the reference and a table lists the others:

```bash
Large_Graph_Similarity --mode reference --edgelist-1 target_edges.csv --nodelist-1 target_nodes.csv --name-1 target \
    --network-table networks.csv --directed true --weighted true --seed 11 --output-dir run
```

`networks.csv` has the columns `name`, `edgelist`, and `nodelist`. Leave `nodelist` empty, or leave the column out, for a network given by its links alone. A path that is not absolute is read relative to the table's folder. The reference may also be read from ORA XML (`--ora-xml-1`, `--network-name-1`).

**Only networks of the reference's type are compared.** The reference's type is found from its links, as in a comparison of two networks: declared directed and symmetric, it is undirected; declared weighted with every weight 1, it is binary. Every other network is tested in the same way and must come out as the same type. This is stricter than `network_comparator`, which keeps the declaration when only one of two networks is symmetric or binary. A run of many comparisons should not contain pairs that are not comparable by definition, so here they are refused and recorded:

- a network whose direction or weight differs from the reference's, for example a binary reconstruction of a weighted reference;
- a one-mode network against a two-mode reference, or the reverse;
- a two-mode network whose mode names differ from the reference's (unless `match_modes = false`), or whose links run the other way.

A refused network has `status` "refused" in the summary, with the reason. A network that cannot be read or measured has `status` "failed". Neither stops the run.

**What is saved.** With `reference_dir` (CLI `--reference-dir`, default `<output-dir>/reference`) the reference's analysis is written to disk: `reference_settings.csv`, `reference_global_stats.csv`, `reference_triad_census.csv`, `reference_node_measures.csv`, `reference_edges.csv`, and `reference_nodes.csv`. A later run reuses it when the reference's lists, the declaration, the community detection settings, the seed, and the LGS version are all the same. Otherwise it is recomputed and saved again, and `reference_status` says what differed. A reference analysed without a seed is never reused, because its community detection could not be repeated. `save_reference`, `load_reference`, `reference_analysis`, and `compare_to_reference` do the steps one at a time.

**Seed.** The seed is used for the reference and for every network, as `network_comparator` uses it for both of a pair. With the seed and the lists, any single comparison of the run can be repeated with `network_comparator`.

**Angular similarity.** It is reported for a network on the reference's nodes (overlap at least `min_node_overlap`) and is `NaN` for the others, as in a comparison of two networks. Simulated networks on their own nodes have none.

**Output.** The summary has one row per network: `name`, `status`, `reason`, `similarity`, `distance`, `edge_angular_similarity`, `edge_angular_similarity_binary`, `node_overlap`, `feature_set`, `nodes`, `top_type`, `top_features`, `undefined_one_side`, and `weight_scale_note`. The CLI writes it as `reference_comparison_summary.csv`, with `reference_comparison_info.csv` (the reference's type, whether its statistics were computed or reused, and the counts compared, refused, and failed). Each compared network gets a folder of its own holding its statistics and the same comparison tables as a comparison of two networks.

## Future work

- **Bipartite modularity (Barber, 2007).** Community detection is left out of the two-mode feature set, because standard modularity expects links within a mode, which a two-mode network cannot have. Adding Barber's null model to `leiden_community_detection` and `champ_community_detection` would restore the community features (partition sizes, modularity, in-group ratios, modularity vitality) for two-mode networks.

## References

- Endres, D. M., & Schindelin, J. E. (2003). A new metric for probability distributions. *IEEE Transactions on Information Theory, 49*(7), 1858-1860.
- Barber, M. J. (2007). Modularity and community detection in bipartite networks. *Physical Review E, 76*, 066102.
- Latapy, M., Magnien, C., & Del Vecchio, N. (2008). Basic notions for the analysis of large two-mode networks. *Social Networks, 30*, 31-48.
- Newman, M. E. J. (2001). Scientific collaboration networks. II. Shortest paths, weighted networks, and centrality. *Physical Review E, 64*, 016132.
- Robins, G., & Alexander, M. (2004). Small worlds among interlocking directors. *Computational & Mathematical Organization Theory, 10*, 69-94.
