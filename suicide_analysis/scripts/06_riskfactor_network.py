"""
'Risk-factor network': a second Gaussian-graphical-model network, this time
among MDD cases only (n=3000), linking a lifetime suicidality-severity node
(0=no ideation .. 3=attempt) to the major psychosocial domains examined in the
regression models (illness severity/recurrence, comorbidity, neuroticism,
childhood sexual abuse, family history, parental bonding, social support,
stressful life events, hospitalisation). Shows which domains are most directly
("bridge") connected to suicidal behaviour once all others are partialled out.
"""
import pandas as pd
import numpy as np
import networkx as nx
from sklearn.covariance import GraphicalLasso
import os, pickle

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))
cases = df[df.case == 1].copy()

cases["suicidality_severity"] = (
    cases["si_lifetime"].fillna(0) + cases["splan_lifetime"].fillna(0) + cases["sattempt_lifetime"].fillna(0)
)

nodes = {
    "suicidality_severity": "Suicidality severity (0-3)",
    "dsm_symptom_count": "Depressive symptom count",
    "n_episodes": "Number of episodes",
    "age_onset": "Age of onset",
    "melancholia_bin": "Melancholic subtype",
    "gad_bin": "Comorbid GAD",
    "panic_bin": "Comorbid panic",
    "psy_inpatient": "Ever hospitalised",
    "Nscore_": "Neuroticism",
    "csa_any": "Childhood sexual abuse",
    "fh_dep_ratio": "Family history: depression",
    "fh_man_ratio": "Family history: mania",
    "pbi_mother_care": "Maternal care",
    "pbi_mother_overprotect": "Maternal overprotection",
    "social_support": "Social support",
    "social_conflict": "Social conflict",
    "SLEscore_": "Lifetime stressful events",
}
items = list(nodes.keys())
d = cases[items].dropna()
X = ((d - d.mean()) / d.std()).values
print("Risk-factor network analytic n =", len(d))

def ebic_glasso(X, ebic_gamma=0.5):
    n, p = X.shape
    S = np.corrcoef(X, rowvar=False)
    alphas = np.linspace(0.02, 0.6, 25)
    best = None
    for a in alphas:
        try:
            gl = GraphicalLasso(alpha=a, max_iter=500, tol=1e-3)
            gl.fit(X)
            prec = gl.precision_
            k = np.sum(np.abs(np.triu(prec, 1)) > 1e-5)
            sign, logdet = np.linalg.slogdet(prec)
            loglik = 0.5 * n * (logdet - np.sum(S * prec))
            bic = -2 * loglik + k * np.log(n)
            ebic = bic + 4 * k * ebic_gamma * np.log(p)
            if best is None or ebic < best[0]:
                best = (ebic, a, prec, k)
        except Exception:
            continue
    return best

ebic, alpha, prec, k = ebic_glasso(X)
diag = np.sqrt(np.outer(np.diag(prec), np.diag(prec)))
pcor = -prec / diag
np.fill_diagonal(pcor, 0)
pcor_df = pd.DataFrame(pcor, index=items, columns=items)
pcor_df.to_csv(os.path.join(OUT, "riskfactor_network_pcor.csv"))
print(f"alpha={alpha:.3f}, edges={k}")

G = nx.Graph()
for it in items:
    G.add_node(it)
for i, a in enumerate(items):
    for j, b in enumerate(items):
        if i < j and abs(pcor[i, j]) > 1e-4:
            G.add_edge(a, b, weight=pcor[i, j])

strength = {n: sum(abs(ed['weight']) for _, _, ed in G.edges(n, data=True)) for n in G.nodes}
cent_df = pd.DataFrame({
    "item": items, "label": [nodes[i] for i in items],
    "strength": [strength.get(i, 0) for i in items],
}).sort_values("strength", ascending=False)
cent_df.to_csv(os.path.join(OUT, "riskfactor_network_centrality.csv"), index=False)
print(cent_df.to_string(index=False))

su_edges = []
for a, b, ed in G.edges(data=True):
    if a == "suicidality_severity" or b == "suicidality_severity":
        other = b if a == "suicidality_severity" else a
        su_edges.append((nodes[other], ed['weight']))
su_edges = sorted(su_edges, key=lambda x: -abs(x[1]))
su_df = pd.DataFrame(su_edges, columns=["neighbor", "partial_cor"])
su_df.to_csv(os.path.join(OUT, "riskfactor_network_suicidality_edges.csv"), index=False)
print("\nDirect neighbors of suicidality-severity node:")
print(su_df.to_string(index=False))

with open(os.path.join(OUT, "riskfactor_network.pkl"), "wb") as f:
    pickle.dump(dict(graph=G, centrality=cent_df, n=len(d), alpha=alpha, nodes=nodes, su_edges=su_df), f)

print("\nDone.")
