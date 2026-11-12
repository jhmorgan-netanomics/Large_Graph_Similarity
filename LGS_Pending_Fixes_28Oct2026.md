# Large_Graph_Similarity: Pending Fixes

Recorded 28 October 2026 and expanded 29 October 2026. The findings come from Step 6 (network comparison) of ION_Comparator, which compares the Island Shield ION bundle's networks with the final export's.

## Status, 11 November 2026

All six findings are addressed in `julia_env/src/Large_Graph_Similarity.jl` and `julia_env/src/CLI.jl`. Findings 6, 1, 2, 3, and 5 are repaired, and two-mode networks (finding 4) have their own feature set. The compiled app in `julia_env/build` has not been rebuilt, so it still carries the old behaviour until `build_app_linux.jl` is run.

The repairs to findings 6, 1, 2, 3, and 5 passed in the project environment (Julia 1.11.2) on 11 November. The two-mode work that followed was checked with Julia 1.12.7 only, and its measures and projections were compared with NetworkX on Balikatan's Agent x Hashtag network. `julia_env/Self_Identity_Tests.jl` holds the regression tests (115 in all) and should be run again in the project environment:

```bash
julia -t auto --project=julia_env julia_env/Self_Identity_Tests.jl
```

| No. | Status | What changed |
|---|---|---|
| 6 | Repaired | One parser, `_parse_link_stats`, reads each statistic by name into a vector initialised with NaN. All four feature builders use it. Within-pair scaling ignores a NaN side. |
| 2 | Repaired for row order | The four constructors sort nodes by id and edges by source, target, weight before measuring. `leiden_community_detection` indexes nodes by sorted id. A network under two different id schemes still differs (0.91 on Balikatan); see the note below. |
| 1 | Repaired | PageRank starts from the uniform vector unless an `rng` is passed. Default tolerance is 1e-10. |
| 3 | Repaired | A value within 1e-9 of a bin width of a bin edge was moved onto that edge before binning. Superseded in version 0.2.0 by linear binning, under which a value near an edge is shared between the two bins (findings 8 and 9). |
| 5 | Repaired | A feature that is NaN in either network is left out of the distance. `network_comparator` returns an `undefined_features` table naming each one and its side, and the CLI writes it as `<name_1>_<name_2>_undefined_features.csv`. |
| 4 | Two-mode feature set added | `network_comparator(...; bipartite = true)` measures two-mode networks with `bipartite_constructor` and `bipartite_feature_builder`. Direction is kept and every node-level distribution is compared mode against mode. A two-mode network against a one-mode network is refused; `project_bipartite` (CLI `--project-onto`) makes the projection. Community measures are left out pending Barber's bipartite modularity. See `README.md`. |

Measured on Balikatan All Communication, directed and weighted, resolution 1.0:

| Comparison | Before | After |
|---|---|---|
| Against itself | 0.9918 | 1.0 |
| Without self-loops, against itself | 0.9918, with uninitialised values in the self-loop features | 1.0 |
| Against itself in reversed order | 0.9062 | 1.0 |
| Against Pac Rim Day 1 | 0.2616 | 0.2631 |

Scores between different networks move in the third decimal place, because the Leiden partition of a network whose nodes were not already listed in id order is now a different one.

Open points:

- **Two-mode PageRank uses its own iteration.** On the undirected form of Agent x Hashtag, `pagerank_stitched` reported `converged = false` at both 1e-6 and 1e-10. The two-mode constructor therefore uses a standard L1 power iteration, which agrees with NetworkX to 1e-11. The one-mode constructors do not check `converged`.
- **Id schemes.** Sorting by id cannot make two id schemes agree, because the sorted order itself differs. ION_Comparator's shared node identity still covers this. Closing it in LGS needs an ordering built from the structure of the network, not from its labels.
- **One-sided undefined measures are reported, not scored.** A measure defined in one network and undefined in the other is listed in `undefined_features` and adds nothing to the distance. Giving it a penalty would need a chosen magnitude.
- **The histogram dropped the largest value.** `fit(Histogram, ...)` uses left-closed bins, so every node whose value equalled the joint maximum fell outside the last bin and was not counted. Repaired in version 0.2.0; see findings 8 and 9 below.
- **`Comparator_Tests.jl` tests its own copies.** That script redefines the constructors, feature builders, and `network_comparator` after loading the module, so its calls run those older copies, which still contain finding 6. Its two comparator calls now read the result by name.

## Findings from the app test, 12 November 2026

`ION_Comparator/test/LGS_App_Tests.jl` runs the rebuilt app on the 30 network pairs of the 29 October run. Every network compared with itself scores exactly 1, as do the 7 identical pairs. The similarity does not yet move with the angular similarity of the edge weights (rank correlation 0.67). Three defects explain it. None is among the six findings below.

| No. | Finding | Status |
|---|---|---|
| 7 | SALSA centrality gave every hub the same score, and every authority the same score | Repaired 12 November |
| 8 | The Jensen-Shannon histogram drops every value equal to the joint maximum | Repaired 12 November (version 0.2.0) |
| 9 | The histogram's bins span only the joint range, so two near-constant distributions that differ slightly come out almost maximally different | Repaired 12 November (version 0.2.0) |
| 10 | The weighted triad census features `<triad>_peak_tau` report the ends of the threshold grid on the raw weight scale | Open |

**Finding 7.** `salsa_centrality` iterated `y = D_out^-1 A D_in^-1 A' x`. That matrix is row-stochastic, so the uniform starting vector is already a fixed point. The function reproduced `salsa_power` in `R_env/Local_Weighted_Clustering_Comparisons.R` to 1.7e-17, which has the same form. The package that R function was checked against, `centiserve::salsa` (version 1.0.0), returns `eigen(M)$values`, the eigenvalues of the hub matrix, so it could not serve as a reference. SALSA scores are the stationary distributions of the hub and authority chains (Lempel and Moran, 2001), which have a closed form (Boldi and Vigna, 2014): within a component, a node's share of in-degree (authority) or out-degree (hub), times the component's share of the nodes. `salsa_centrality` now computes that directly. On Balikatan it agrees with an independent power iteration to 1.7e-13 and with the stationary distribution computed in R to 1.6e-12.

**Findings 8 and 9: the node measure comparison (version 0.2.0).** Every node measure is now compared in one way: values are placed on a geometric axis, `asinh(x / s)`, where `s` is the median nonzero magnitude of the two distributions pooled with the networks weighted equally; the range, which always includes zero, is divided into bins of fixed width (0.125 by default; `bin_width`, CLI `--bin-width`); each node is shared between the two nearest bin centres; and the feature is the square root of the Jensen-Shannon divergence. This replaces the 20 equal-width bins, the edge snapping of finding 3, and the separate raw and asinh scores. `network_comparator` returns one score, `overall_similarity`, and the CLI writes one set of files. A `node_measure_diagnostics` table reports, per measure, the divergence, the divergence expected from sampling alone, and the scaled Wasserstein-1 distance; none of these enters the score.

Measured on the 30 pairs of the 29 October run (source run in process, before the rebuild):

| | 29 October app | After findings 1 to 7 | Version 0.2.0 |
|---|---|---|---|
| Rank correlation with the weighted angular similarity | 0.65 | 0.78 | 0.93 |
| Rank correlation with the binary angular similarity | | 0.82 | 0.92 |
| Networks scoring exactly 1 against themselves | | 60 of 60 | 60 of 60 |

**Benchmarks.** `julia_env/Benchmark_Replication_Tests.jl` compares each measure on Balikatan's All Communication network with the results saved from ORA, igraph, scoredec, Pajek, Python, and R. The revised source meets all 33 comparisons. Run against the source as it stood on 28 October, 27 of the 33 statistics are identical to the last digit. The six that differ: SALSA (finding 7, changed by design); PageRank (finding 1; the largest difference from ORA moves in the ninth decimal place); and the Leiden partition and the two modularity vitality rank correlations against ORA, which rest on a random search. Three differences from the benchmark packages predate these repairs and are unchanged; they are differences of definition and are written into the test: normalized in- and out-degree leave self-loops out and divide by the nodes that have links, where ORA counts self-loops and divides by all nodes; Barrat's coefficient combines a reciprocated pair of links by sum, where the benchmark uses the maximum (30 of 1,245 nodes differ); and igraph's k-core counts parallel links and self-loops. The adjusted Rand index of the Leiden partition against ORA's was 0.86 to 0.88 here for both the original and the revised source; the value recorded in November 2025 was 0.958.

**Finding 10 (open).** The weighted triad census is computed over a grid of weight thresholds chosen for each network. Its `<triad>_peak_tau` feature is the threshold at which a triad type is densest. In all 19 weighted one-mode networks of the 29 October run that feature takes two values per network: the lower end of the grid for the types that occur and for the types that never occur, and the upper end of the grid for the empty triad (003), whose density can only rise as links are removed. The 16 features therefore report the ends of the threshold grid. They are on the raw scale of the link weights, where every other triad feature is a density. For day 4 Hashtag x Hashtag - Co-Occurrence the upper end is 20.19 in one network and 13.52 in the other, and that single feature supplies 74% of the squared distance (similarity 0.47; 0.64 without the peak thresholds). Leaving the 16 features out raises the rank correlation with the weighted angular similarity from 0.93 to 0.95. `003_AUMC_density` has the same dependence in a milder form: it is close to the width of the grid in log units. No change has been made.

The sections below are the record as written on 28 and 29 October; line numbers refer to the file before the repairs.

Line numbers refer to `julia_env/src/Large_Graph_Similarity.jl` as of 28 October 2026 (625,916 bytes).

## Why these matter

LGS reduces two networks to one similarity score between 0 and 1, where 1 means "no measurable difference". That promise rests on one property: **a network compared with itself must score 1.**

ION_Comparator tested this property directly. It handed the app pairs of networks that were edge-for-edge identical, and in several cases byte-identical files. The app did not return 1. Depending on the network and the run, it scored them anywhere from 0.41 to 0.9999.

When identical inputs can score 0.41, a low score between two different networks cannot be read as evidence that they differ. The six findings below explain where the shortfall comes from. They are ordered by how much each one distorts the score:

| No. | Finding | Typical effect on an identical pair | Affects |
|---|---|---|---|
| 6 | Link statistics read uninitialised memory | Down to 0.41; changes from run to run | Any network with no self-loops |
| 2 | Community detection depends on node order | 0.79–0.85 | Any network whose node order differs |
| 1 | PageRank starts from an unseeded random vector | 0.97–0.9999 | Every network |
| 3 | Histogram JSD amplifies tiny differences | Magnifies finding 1 | Every network |
| 5 | Undefined measures make the score NaN | NaN | Measures undefined on either side |
| 4 | Two-mode networks are measured as one-mode | Measures that do not apply are scored | Every bipartite network |

---

## 6. Link statistics hold uninitialised memory when a statistic is NaN

**Finding.** For a network with no self-loops, the features `self_loops_mean` and `self_loops_std` are not defined. LGS fills them with whatever happens to be in memory: values such as 3.0e-294, 6.7e-308, 1.0, or -1.0. These values differ between the two copies of a network and between runs. Within-pair scaling then turns them into the largest difference a scaled feature can show.

**How it was detected.**
- On 29 October, day0 Tweet x Tweet - Retweeted-By was given to the app as two byte-identical edge and node lists (same md5).
- It scored **0.4142**. The same network had scored 0.9993 on 27 October.
- The type contributions put the whole loss in Link Measure: type similarity 0.4142, distance √2. Every other type scored 1, apart from Influence at 0.9993, which is finding 1.
- The aligned features showed `self_loops_mean` and `self_loops_std` at 1.0 for the final export and 0.0 for ION. Yet the app's own `*_global_stats.csv` reports `self_loops: (count = 0, min = NaN, max = NaN, mean = NaN, std = NaN, sum = 0.0)` for both.
- Checking all 30 networks of the run, every network without self-loops showed garbage in these two features. Every network with self-loops showed the correct values.

**Cause.**
1. The four feature builders write each link group's statistics as a string and then parse the string back:
   - `symmetric_binary_feature_builder`, about line 13143;
   - `symmetric_weighted_feature_builder`, about line 13769;
   - `directed_binary_feature_builder`, about line 14425;
   - `directed_weighted_feature_builder`, about line 15145.
2. The parse uses `eachmatch(r"=\s*([+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)", vstr)`. This regex matches digits only, so every `NaN` is skipped. For a network without self-loops, `numbers` becomes `[0.0, 0.0]`, the count and the sum.
3. The guard that follows was meant to pad the missing values with NaN, but it fills nothing:

   ```julia
   resize!(numbers, 6)
   numbers[length(numbers)+1:6] .= NaN   # length is already 6, so the range 7:6 is empty
   ```

   `resize!` extends the vector without initialising the new slots. Slots 3 to 6 keep whatever was in memory.
4. The values are then assigned by position, so the sum lands in `min` and garbage lands in `max`, `mean`, `std`, and `sum`.
5. `_global_metric_normalizer` (line 15639) drops min, max, and sum. It scales mean and std within the pair by `max(abs(v1), abs(v2), eps())`. Garbage of 3e-294 against 0 therefore becomes 1 against 0, and garbage against garbage becomes any ratio at all.

`link_statistics` (about line 11744) returns NaN for min, max, mean, and std only when a group is empty. It uses the population standard deviation, so a single self-loop gives a std of 0, not NaN. The bug is therefore triggered exactly when a network has no self-loops, or, in principle, no non-self links.

**Why it is a problem.**
- It is the largest of the six distortions, and it is not repeatable: the score depends on the contents of uninitialised memory.
- It strikes the networks that matter most for social-media comparison. Tweet x Tweet, Agent x Hashtag, and Agent x Tweet networks never have self-loops, and Agent x Agent networks often do not.
- It does not show up in a test on a network that has self-loops (see *Regression tests* below).

**Fix.**
- Parse each statistic by name rather than by position, and accept NaN and Inf. For example, use `r"(\w+)\s*=\s*([^,)]+)"` with `tryparse(Float64, ...)`, defaulting to NaN.
- Replace `resize!` with an explicit `fill(NaN, 6)` that is then overwritten by name.
- Better still, stop round-tripping through a string: pass the numbers from `link_statistics` straight to the feature builders.
- Let fix 5 drop a self-loop mean or std that is NaN on both sides.
- Apply the fix in all four builders; the code is duplicated.

**Workaround in ION_Comparator.** Step 6 rebuilds the Link Measure features from the app's own `final_global_stats.csv` and `ion_global_stats.csv`, reading each statistic by name, before it recomputes the score (`_link_stats` and `_link_features` in `src/network_inspection.jl`). Checked against the 29 October output:
- valid features are reproduced exactly (day0 Agent x Agent - All Communication stays at 0.5844);
- the identical Retweeted-By network rises from 0.4142 to 0.9993. The remaining 0.0007 is finding 1.

**Was it introduced recently?** Probably not. The same parser, with the same `resize!` guard, appears in all four feature builders, which suggests it has been there since they were written. The git history can settle this. From the `Large_Graph_Similarity` folder:

```bash
git log -S 'resize!(numbers, 6)' --oneline -- julia_env/src/Large_Graph_Similarity.jl
```

The oldest commit listed is the one that introduced the guard.

## 1. PageRank is not reproducible between runs

**Finding.** Two byte-identical networks receive PageRank values that differ by up to about 1e-7. The histogram comparison (finding 3) turns that into a small but nonzero distance.

**How it was detected.**
- On 28 October, day0 Agent x Agent - Retweeted-By was given to the app as two byte-identical edge and node lists. This network has self-loops, so finding 6 does not apply.
- It scored 0.9934 rather than 1. Only one of 326 features differed: `jsd_raw_page_rank`, at +0.0033 and -0.0033.
- All 1,149 nodes' `page_rank` values differed between the two copies, by at most 9.9e-8.
- Every edge-identical network with self-loops scored between 0.969 and 0.9999, and Influence was the largest contributor in each case.

**Cause.** `pagerank_local_ora_matrix` (line 3626) starts its power iteration from `x = rand(rng, n)` (line 3700). Its `rng` defaults to `Random.default_rng()`, so the `--seed` value never reaches it. The iteration stops at a sup-norm tolerance of 1e-6, so two runs converge to values about 1e-7 apart. It is called through `pagerank_stitched` (line 3874).

**Why it is a problem.** A tolerance of 1e-6 is fine for ranking nodes, but the comparator compares whole distributions. Seed noise therefore becomes a floor below which no pair of networks can be told apart from identical ones. It also makes every score unrepeatable at the third decimal place.

**Fix.**
- Pass a seeded generator, `MersenneTwister(seed)`, from the comparator through `pagerank_stitched` into `pagerank_local_ora_matrix`, or start from a uniform vector, `x = fill(1/n, n)`.
- Consider `tol = 1e-10`.

## 2. Community detection depends on node order

**Finding.** Two copies of one network, the same except that the nodes are listed in a different order, receive different community partitions. Every measure built on the partition then differs.

**How it was detected.**
- Day0 Retweeted-By compared with itself, the second copy's node and edge lists in reverse order, scored 0.853. Day4 scored 0.790. Degree Measures was the largest contributor on both days.
- Earlier, the same edge-identical network under two id schemes (ION's renumbered ids against the scenario's) scored 0.83. Only community-based features differed:
  - modularity (0.6410 against 0.6396);
  - `Community_k` sizes;
  - in-group degree ratios;
  - modularity vitality.

**Cause.** Leiden's result depends on the order in which nodes are indexed, even with a fixed seed. Every measure built on the partition inherits this.

**Why it is a problem.** Node order carries no meaning. Two exports of the same data, or one network under two id schemes, should compare as identical, yet they lose up to about 0.2 of similarity.

**Fix.** Sort the nodes by id (canonical order) before building the graph and running Leiden. A network compared with itself in reversed order should then score 1.

**Workaround in ION_Comparator.** Both sides are written under one shared node identity in sorted order, so unchanged networks reach the app as identical files. Changed networks still carry this noise in their community-based features.

## 3. Histogram JSD amplifies tiny differences

**Finding.** Node-level distributions are compared through a 20-bin histogram Jensen–Shannon divergence, reported as ±√JSD/2. A change of 1e-7 in one node's value can move it across a bin edge, and the square root then enlarges the effect. For example, a JSD of 4.4e-5 becomes a distance of 0.0066.

**How it was detected.** The same pair of runs as finding 1: PageRank values 1e-7 apart produced `jsd_raw_page_rank` of ±0.0033.

**Cause.** `_js_divergence_computation` (line 15488) bins both distributions into 20 common bins, and a value sitting on a bin edge changes bins under any perturbation.

**Why it is a problem.** It turns numerical noise into a visible distance. Once finding 1 is fixed it is mostly harmless, but it will magnify any other source of floating-point noise.

**Fix (optional once finding 1 is fixed).** Round values to about 1e-9 before binning, or use a tie-robust estimator.

## 5. Undefined measures turn the overall score into NaN

**Finding.** When a feature is NaN on either side, the weighted Euclidean distance becomes NaN, and so does `overall_similarity_*`.

**How it was detected.** In day4 Tweet x Tweet - Quoted-By, degree assortativity was defined in the final export and undefined in ION, and the app's overall score came back NaN.

**Cause.** Degree assortativity is NaN when one endpoint degree sequence has zero variance:
- directed binary networks (Newman, target in-degree minus 1);
- weighted networks under Farine in-degree mode, where agents have in-degree 0.

NaN minus NaN, or a NaN on one side only, then propagates through the sum.

**Why it is a problem.** A single undefined measure discards the information in every other measure. It also hides the difference that matters: a measure defined on one side and undefined on the other is itself a finding.

**Fix.**
- Skip a feature that is NaN on both sides.
- Report a feature that is NaN on one side as a difference, with the side, rather than letting it become NaN in the distance.

ION_Comparator does both when it recomputes the score.

## 4. Two-mode (bipartite) networks are measured as one-mode

**Finding.** LGS has no notion of a two-mode network. Agent x Hashtag, Agent x Tweet - Sender, Tweet x Agent - Mentions, and Tweet x Hashtag link two different kinds of node, but they are measured as if every node could link to every other.

**How it was detected.** Reading the code. `network_comparator`, its constructors, and the CLI take no bipartite argument. Only `freeman_degree_normalization` accepts `bipartite` and `types` (line 2087), and every caller passes `bipartite = false`.

**Why it is a problem.** Several measures are meaningless or misleading on a two-mode network:
- degree assortativity;
- transitivity and the local clustering coefficient, since a two-mode network has no triangles;
- reciprocity;
- the Triad Census and Local Structure types.

Others are mis-scaled:
- density should be m / (n₁ · n₂), not m / (n(n−1));
- degree centralization should be normalized within each mode.

Comparing two two-mode networks today mixes meaningful and meaningless features in one score.

**Fix (deferred).** LGS was designed for one-mode networks, but two-mode networks are common in social-media data and deserve proper support:
1. Add a `bipartite::Bool` argument, with node `types`, to `network_comparator`, the constructors, and the CLI (for example `--bipartite true --mode-column type`). Infer the modes from ORA's source and target node sets where possible.
2. Drop the measures that do not apply (listed above).
3. Rescale the ones that do: two-mode density, and degree and centralization normalized within each mode, using the existing `freeman_degree_normalization(...; bipartite = true, types)`.
4. Add two-mode measures in place of the dropped ones:
   - bipartite clustering (Robins and Alexander's four-cycle coefficient, or Latapy's pairwise coefficient);
   - degree distributions for each mode separately;
   - optionally, measures of the one-mode projections, labelled as such.

**Workaround in ION_Comparator.** It drops degree assortativity, transitivity, local clustering, reciprocity, and the Triad Census and Local Structure types from the score it recomputes for two-mode networks.

---

## Regression tests to add with the repairs

The core property is that a network compared with itself scores exactly 1. The tests should check it on networks that exercise each finding.

**1. Balikatan All Communication against itself.** In `Comparator_Tests.jl`, after `balikatan_arcs` and `balikatan_nodes` are loaded, add:

```julia
balikatan_self = network_comparator(balikatan_arcs, balikatan_nodes, balikatan_arcs, balikatan_nodes;
                                    directed = true, weighted = true, resolution = 1.0)
@assert balikatan_self.overall_similarity_raw == 1.0   "raw: $(balikatan_self.overall_similarity_raw)"
@assert balikatan_self.overall_similarity_asinh == 1.0 "asinh: $(balikatan_self.overall_similarity_asinh)"
```

`network_comparator` returns a named tuple of 13 fields (line 16409), so the test reads the scores by name.

The existing comparator tests unpack it by position into 10 names (Balikatan against Pac Rim) or 4 names (against Pac Sentry). The tuple has since grown. As a result, `pac_sentry_overall_similarity` now receives `node_measures_1`, and `pac_rim_type_contributions` receives the asinh distance. Those tests should read the fields by name too.

Expected: 1 once finding 1 is fixed. Before that fix, about 0.99, because PageRank is unseeded.

This test cannot catch finding 6. Balikatan's All Communication network has **41 self-loops** (of 3,146 arcs in `Test_Data/Balikatan_2022_All_Communication_Arcs.csv`), so its self-loop statistics are defined and the faulty guard never fires. A Balikatan self-comparison that passes is therefore consistent with finding 6 having been present all along.

I did not find a Balikatan self-comparison in the current `Comparator_Tests.jl`, `CLI_Tests.jl`, or `Graph_Comparison_Tests.jl`. Those files compare Balikatan with Pac Rim and with Pac Sentry. If the identity test existed earlier, the git history will show it:

```bash
git log -S 'balikatan_arcs, balikatan_nodes, balikatan_arcs' --oneline
```

**2. Balikatan without self-loops against itself.** This test catches finding 6.

```julia
balikatan_noloops = filter(r -> r[1] != r[2], balikatan_arcs)
noloops_self = network_comparator(balikatan_noloops, balikatan_nodes, balikatan_noloops, balikatan_nodes;
                                  directed = true, weighted = true, resolution = 1.0)
@assert noloops_self.overall_similarity_raw == 1.0 "raw: $(noloops_self.overall_similarity_raw)"
```

Expected: 1. Before the fix, anything from about 0.4 to 1, varying by run.

**3. Reversed order against itself.** Use `balikatan_arcs` against the same edges with rows reversed, and nodes reversed. This catches finding 2. Expected: 1.

**4. Two copies through the CLI.** Write the same edge and node CSVs twice and run `--mode comparison`. This checks that the CLI path, which ION_Comparator uses, behaves like the in-process call. Expected: `overall_similarity_raw` = 1 and `overall_similarity_asinh` = 1.

**5. A one-sided undefined measure.** Use a directed binary network in which all targets share one in-degree, against a copy with one extra edge. This checks fix 5. Expected: a finite score, with degree assortativity reported as undefined on one side.

**6. A two-mode network against itself (after fix 4).** Use Balikatan's Agent x Hashtag against itself. Expected: 1, with no Triad Census, Local Structure, or transitivity features in the comparison.

## How to verify after a rebuild

1. Run the regression tests above in `Comparator_Tests.jl`.
2. From ION_Comparator, run `julia -t auto --project=. test/Network_Inspection_Tests.jl` and check:
   - Criterion 2, "the two methods agree", should score exactly 1 for the edge-identical networks. At that point raise `LGS_NOISE_FLOOR` in the test file to `1 - 1e-9`.
   - Criterion 4, "order sensitivity", should report raw 1.0 on both days.
3. Remove the Link Measure workaround from ION_Comparator, or keep it as a cross-check. The app's scores and the recomputed ones should then agree exactly.
