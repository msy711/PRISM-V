"""
Network analysis (Gaussian graphical model / partial-correlation network) of the
16-item current symptom checklist (SCL), which includes suicidal ideation as one
node and is measured in BOTH the MDD and control groups. Estimates regularized
partial-correlation networks (graphical lasso, EBIC-selected) for (a) full sample,
(b) MDD only, (c) control only, computes node centrality (strength, betweenness,
closeness, expected influence), and profiles which symptoms are most directly
("bridge") connected to the suicidal-ideation node in each group.
"""
import pandas as pd
import numpy as np
import networkx as nx
from sklearn.covariance import GraphicalLasso
import os
import json

OUT = "/home/user/PRISM-V/suicide_analysis/outputs"
FIG = "/home/user/PRISM-V/suicide_analysis/figures"
df = pd.read_pickle(os.path.join(OUT, "subject_level_with_factors.pkl"))

scl_items = ['scl.no.pleasure','scl.no.interest','scl.hopeless','scl.effortful','scl.worthless',
             'scl.poor.appetite','scl.fall.asleep','scl.restless.sleep','scl.low.energy',
             'scl.suicide.thought','scl.cry','scl.trapped','scl.blame','scl.lonely','scl.blue','scl.worry']

short_label = {
    'scl.no.pleasure':'No pleasure','scl.no.interest':'No interest','scl.hopeless':'Hopeless',
    'scl.effortful':'Everything effortful','scl.worthless':'Worthless','scl.poor.appetite':'Poor appetite',
    'scl.fall.asleep':'Trouble falling asleep','scl.restless.sleep':'Restless sleep','scl.low.energy':'Low energy',
    'scl.suicide.thought':'SUICIDAL IDEATION','scl.cry':'Crying','scl.trapped':'Feeling trapped',
    'scl.blame':'Self-blame','scl.lonely':'Lonely','scl.blue':'Feeling blue','scl.worry':'Worry'
}

def ebic_glasso(X, gammas_alpha=None, ebic_gamma=0.5):
    """Simple EBIC-selected graphical lasso over a grid of alpha values."""
    n, p = X.shape
    S = np.corrcoef(X, rowvar=False)
    alphas = np.linspace(0.02, 0.6, 25)
    best = None
    for a in alphas:
        try:
            gl = GraphicalLasso(alpha=a, max_iter=500, tol=1e-3)
            gl.fit(X)
            prec = gl.precision_
            # count free params (nonzero off-diagonal / 2)
            k = np.sum(np.abs(np.triu(prec, 1)) > 1e-5)
            # gaussian log-likelihood
            sign, logdet = np.linalg.slogdet(prec)
            loglik = 0.5 * n * (logdet - np.sum(S * prec))
            bic = -2 * loglik + k * np.log(n)
            ebic = bic + 4 * k * ebic_gamma * np.log(p)
            if best is None or ebic < best[0]:
                best = (ebic, a, prec, k)
        except Exception:
            continue
    return best  # (ebic, alpha, precision, k)

def build_network(data, items, label):
    d = data[items].dropna()
    X = (d - d.mean()) / d.std()
    X = X.values
    ebic, alpha, prec, k = ebic_glasso(X)
    diag = np.sqrt(np.outer(np.diag(prec), np.diag(prec)))
    pcor = -prec / diag
    np.fill_diagonal(pcor, 0)
    pcor_df = pd.DataFrame(pcor, index=items, columns=items)
    pcor_df.to_csv(os.path.join(OUT, f"network_pcor_{label}.csv"))

    G = nx.Graph()
    for it in items:
        G.add_node(it)
    for i, a in enumerate(items):
        for j, b in enumerate(items):
            if i < j and abs(pcor[i, j]) > 1e-4:
                G.add_edge(a, b, weight=pcor[i, j])

    strength = {n: sum(abs(d['weight']) for _, _, d in G.edges(n, data=True)) for n in G.nodes}
    ei = {n: sum(d['weight'] for _, _, d in G.edges(n, data=True)) for n in G.nodes}  # expected influence
    dist_G = nx.Graph()
    for a, b, edata in G.edges(data=True):
        dist_G.add_edge(a, b, weight=1.0 / (abs(edata['weight']) + 1e-6))
    btw = nx.betweenness_centrality(dist_G, weight='weight')
    clo = nx.closeness_centrality(dist_G, distance='weight')

    cent_df = pd.DataFrame({
        "item": items,
        "label": [short_label[i] for i in items],
        "strength": [strength.get(i, 0) for i in items],
        "expected_influence": [ei.get(i, 0) for i in items],
        "betweenness": [btw.get(i, 0) for i in items],
        "closeness": [clo.get(i, 0) for i in items],
    }).sort_values("strength", ascending=False)
    cent_df.to_csv(os.path.join(OUT, f"network_centrality_{label}.csv"), index=False)

    # edges directly touching suicide node, ranked
    su_edges = []
    for a, b, edata in G.edges(data=True):
        if a == 'scl.suicide.thought' or b == 'scl.suicide.thought':
            other = b if a == 'scl.suicide.thought' else a
            su_edges.append((short_label[other], edata['weight']))
    su_edges = sorted(su_edges, key=lambda x: -abs(x[1]))
    su_df = pd.DataFrame(su_edges, columns=["neighbor", "partial_cor"])
    su_df.to_csv(os.path.join(OUT, f"network_suicide_edges_{label}.csv"), index=False)

    print(f"\n=== {label}: n={len(d)}, alpha={alpha:.3f}, edges={G.number_of_edges()} ===")
    print("Top centrality (strength):")
    print(cent_df.head(6).to_string(index=False))
    print("Direct neighbors of suicidal-ideation node:")
    print(su_df.to_string(index=False))

    return dict(label=label, n=len(d), alpha=float(alpha), n_edges=G.number_of_edges(),
                pcor=pcor_df, centrality=cent_df, suicide_edges=su_df, graph=G)

results = {}
results['full'] = build_network(df, scl_items, "full")
results['MDD'] = build_network(df[df.case == 1], scl_items, "MDD")
results['Control'] = build_network(df[df.case == 0], scl_items, "Control")

# summary comparison of suicide-node centrality across networks
summary = []
for label, r in results.items():
    row = r['centrality'].set_index('item').loc['scl.suicide.thought']
    summary.append(dict(network=label, n=r['n'], **row.drop('label').to_dict()))
summary_df = pd.DataFrame(summary)
summary_df.to_csv(os.path.join(OUT, "network_suicide_node_summary.csv"), index=False)
print("\n=== Suicidal-ideation node centrality across networks ===")
print(summary_df.to_string(index=False))

# save graphs for plotting later
import pickle
with open(os.path.join(OUT, "networks.pkl"), "wb") as f:
    pickle.dump({k: dict(graph=v['graph'], centrality=v['centrality'], n=v['n'], alpha=v['alpha'])
                 for k, v in results.items()}, f)

print("\nDone.")
