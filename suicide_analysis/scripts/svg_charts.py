"""Reusable, theme-aware inline-SVG chart builders (dataviz-skill palette tokens).
All colors reference CSS custom properties defined in the report's <style> block,
so charts adapt automatically between light and dark themes.
"""
import numpy as np

FONT = "system-ui,-apple-system,'Segoe UI',sans-serif"


def _fmt(x, nd=1):
    return f"{x:.{nd}f}"


def bar_chart(data, w=640, h=340, margin=(30, 20, 60, 190), unit="%", color_var="--series-1",
              value_fmt=None, max_val=None, title=None):
    """Horizontal bar chart. data: list of (label, value) tuples."""
    top, right, bottom, left = margin
    plot_w = w - left - right
    plot_h = h - top - bottom
    n = len(data)
    bar_h = plot_h / n * 0.6
    gap = plot_h / n
    maxv = max_val if max_val else max(v for _, v in data) * 1.15
    value_fmt = value_fmt or (lambda v: f"{v:.1f}{unit}")
    svg = [f'<svg viewBox="0 0 {w} {h}" width="100%" role="img" aria-label="{title or "bar chart"}" xmlns="http://www.w3.org/2000/svg" font-family="{FONT}">']
    # gridlines
    for gx in np.linspace(0, maxv, 5):
        x = left + gx / maxv * plot_w
        svg.append(f'<line x1="{x:.1f}" y1="{top}" x2="{x:.1f}" y2="{top+plot_h}" stroke="var(--gridline)" stroke-width="1"/>')
        svg.append(f'<text x="{x:.1f}" y="{top+plot_h+18}" text-anchor="middle" font-size="11" fill="var(--text-muted)">{value_fmt(gx)}</text>')
    for i, (label, v) in enumerate(data):
        y = top + i * gap + (gap - bar_h) / 2
        bw = v / maxv * plot_w
        svg.append(f'<text x="{left-10}" y="{y+bar_h/2+4}" text-anchor="end" font-size="12.5" fill="var(--text-primary)">{label}</text>')
        svg.append(f'<rect x="{left}" y="{y:.1f}" width="{bw:.1f}" height="{bar_h:.1f}" rx="3" fill="var({color_var})"/>')
        svg.append(f'<text x="{left+bw+8:.1f}" y="{y+bar_h/2+4}" font-size="12.5" font-weight="600" fill="var(--text-primary)">{value_fmt(v)}</text>')
    svg.append(f'<line x1="{left}" y1="{top+plot_h}" x2="{left+plot_w}" y2="{top+plot_h}" stroke="var(--axis)" stroke-width="1.5"/>')
    svg.append("</svg>")
    return "\n".join(svg)


def grouped_bar_chart(categories, series, w=640, h=320, margin=(24, 24, 50, 60), unit="%",
                       colors=("--series-1", "--series-2"), value_fmt=None, title=None):
    """categories: list of str. series: list of (name, [values]) length == len(categories)."""
    top, right, bottom, left = margin
    plot_w = w - left - right
    plot_h = h - top - bottom
    n = len(categories)
    ns = len(series)
    maxv = max(max(vals) for _, vals in series) * 1.2
    value_fmt = value_fmt or (lambda v: f"{v:.0f}{unit}")
    group_w = plot_w / n
    bar_w = group_w / (ns + 1)
    svg = [f'<svg viewBox="0 0 {w} {h}" width="100%" role="img" aria-label="{title or "bar chart"}" xmlns="http://www.w3.org/2000/svg" font-family="{FONT}">']
    for gy in np.linspace(0, maxv, 5):
        y = top + plot_h - gy / maxv * plot_h
        svg.append(f'<line x1="{left}" y1="{y:.1f}" x2="{left+plot_w}" y2="{y:.1f}" stroke="var(--gridline)" stroke-width="1"/>')
        svg.append(f'<text x="{left-8}" y="{y+4:.1f}" text-anchor="end" font-size="11" fill="var(--text-muted)">{value_fmt(gy)}</text>')
    for gi, cat in enumerate(categories):
        gx = left + gi * group_w
        for si, (name, vals) in enumerate(series):
            v = vals[gi]
            bh = v / maxv * plot_h
            bx = gx + (si + 0.5) * bar_w
            by = top + plot_h - bh
            svg.append(f'<rect x="{bx:.1f}" y="{by:.1f}" width="{bar_w*0.82:.1f}" height="{bh:.1f}" rx="3" fill="var({colors[si]})"/>')
            svg.append(f'<text x="{bx+bar_w*0.41:.1f}" y="{by-6:.1f}" text-anchor="middle" font-size="11" font-weight="600" fill="var(--text-primary)">{value_fmt(v)}</text>')
        svg.append(f'<text x="{gx+group_w/2:.1f}" y="{top+plot_h+20}" text-anchor="middle" font-size="12" fill="var(--text-primary)">{cat}</text>')
    svg.append(f'<line x1="{left}" y1="{top+plot_h}" x2="{left+plot_w}" y2="{top+plot_h}" stroke="var(--axis)" stroke-width="1.5"/>')
    # legend
    lx = left
    ly = top - 10
    for si, (name, _) in enumerate(series):
        svg.append(f'<rect x="{lx:.1f}" y="{ly-9}" width="10" height="10" rx="2" fill="var({colors[si]})"/>')
        svg.append(f'<text x="{lx+15:.1f}" y="{ly:.1f}" font-size="11.5" fill="var(--text-secondary)">{name}</text>')
        lx += 18 + len(name) * 7 + 18
    svg.append("</svg>")
    return "\n".join(svg)


def forest_plot(rows, w=680, h=None, margin=(30, 40, 40, 210), xmax=None, title=None):
    """rows: list of dict(label, or_, lo, hi, sig(bool), direction('risk'|'protect'|'ns'))"""
    top, right, bottom, left = margin
    n = len(rows)
    row_h = 26
    plot_h = n * row_h
    h = h or (top + plot_h + bottom)
    plot_w = w - left - right
    xmax = xmax or max(r['hi'] for r in rows) * 1.15
    xmin = 0.3
    def xpos(v):
        # log scale
        lo_l, hi_l = np.log(xmin), np.log(xmax)
        v = max(v, xmin * 1.001)
        return left + (np.log(v) - lo_l) / (hi_l - lo_l) * plot_w
    svg = [f'<svg viewBox="0 0 {w} {h}" width="100%" role="img" aria-label="{title or "forest plot"}" xmlns="http://www.w3.org/2000/svg" font-family="{FONT}">']
    # x gridlines at 0.5,1,2,4,8
    for gv in [0.5, 1, 2, 4, 8]:
        if gv < xmin or gv > xmax:
            continue
        x = xpos(gv)
        svg.append(f'<line x1="{x:.1f}" y1="{top}" x2="{x:.1f}" y2="{top+plot_h}" stroke="var(--gridline)" stroke-width="1" stroke-dasharray="{"2,2" if gv!=1 else "0"}"/>')
        svg.append(f'<text x="{x:.1f}" y="{top+plot_h+18}" text-anchor="middle" font-size="11" fill="var(--text-muted)">{gv}</text>')
    svg.append(f'<text x="{left+plot_w/2:.1f}" y="{top+plot_h+34}" text-anchor="middle" font-size="11.5" fill="var(--text-secondary)">Odds ratio (log scale, 95% CI)</text>')
    for i, r in enumerate(rows):
        y = top + i * row_h + row_h / 2
        svg.append(f'<text x="{left-10}" y="{y+4:.1f}" text-anchor="end" font-size="12" fill="var(--text-primary)">{r["label"]}</text>')
        x1, x2, xc = xpos(max(r['lo'], xmin*1.001)), xpos(min(r['hi'], xmax)), xpos(r['or_'])
        color = "var(--status-critical)" if r['direction'] == 'risk' else ("var(--status-good)" if r['direction'] == 'protect' else "var(--text-muted)")
        svg.append(f'<line x1="{x1:.1f}" y1="{y:.1f}" x2="{x2:.1f}" y2="{y:.1f}" stroke="{color}" stroke-width="2"/>')
        r_dot = 5 if r['sig'] else 3.5
        svg.append(f'<circle cx="{xc:.1f}" cy="{y:.1f}" r="{r_dot}" fill="{color}"/>')
        svg.append(f'<text x="{left+plot_w+8:.1f}" y="{y+4:.1f}" font-size="11" fill="var(--text-secondary)">{r["or_"]:.2f} ({r["lo"]:.2f}–{r["hi"]:.2f})</text>')
    svg.append(f'<line x1="{left}" y1="{top}" x2="{left}" y2="{top+plot_h}" stroke="var(--axis)" stroke-width="1"/>')
    svg.append("</svg>")
    return "\n".join(svg)


def km_curves(curves, w=560, h=340, margin=(20, 24, 46, 50), xlab="Years since illness onset",
              ylab="Attempt-free probability", title=None, colors=("--series-2", "--series-1")):
    """curves: list of dict(name, times[], surv[], color_idx)"""
    top, right, bottom, left = margin
    plot_w = w - left - right
    plot_h = h - top - bottom
    xmax = max(max(c['times']) for c in curves)
    svg = [f'<svg viewBox="0 0 {w} {h}" width="100%" role="img" aria-label="{title or "KM curve"}" xmlns="http://www.w3.org/2000/svg" font-family="{FONT}">']
    for gy in np.linspace(0, 1, 6):
        y = top + plot_h - gy * plot_h
        svg.append(f'<line x1="{left}" y1="{y:.1f}" x2="{left+plot_w}" y2="{y:.1f}" stroke="var(--gridline)" stroke-width="1"/>')
        svg.append(f'<text x="{left-8}" y="{y+4:.1f}" text-anchor="end" font-size="10.5" fill="var(--text-muted)">{gy:.1f}</text>')
    for gx in np.linspace(0, xmax, 6):
        x = left + gx / xmax * plot_w
        svg.append(f'<text x="{x:.1f}" y="{top+plot_h+16}" text-anchor="middle" font-size="10.5" fill="var(--text-muted)">{gx:.0f}</text>')
    for ci, c in enumerate(curves):
        pts = []
        for t, s in zip(c['times'], c['surv']):
            x = left + min(t, xmax) / xmax * plot_w
            y = top + plot_h - s * plot_h
            pts.append((x, y))
        path = f"M {pts[0][0]:.1f} {pts[0][1]:.1f} "
        for k in range(1, len(pts)):
            path += f"L {pts[k][0]:.1f} {pts[k-1][1]:.1f} L {pts[k][0]:.1f} {pts[k][1]:.1f} "
        col = colors[ci % len(colors)]
        svg.append(f'<path d="{path}" fill="none" stroke="var({col})" stroke-width="2.2"/>')
        lx, ly = pts[-1]
        svg.append(f'<circle cx="{lx:.1f}" cy="{ly:.1f}" r="3" fill="var({col})"/>')
    # legend
    lx0 = left + 6
    for ci, c in enumerate(curves):
        col = colors[ci % len(colors)]
        svg.append(f'<rect x="{lx0:.1f}" y="{top+2}" width="10" height="10" rx="2" fill="var({col})"/>')
        svg.append(f'<text x="{lx0+15:.1f}" y="{top+11}" font-size="11" fill="var(--text-secondary)">{c["name"]}</text>')
        lx0 += 18 + len(c['name']) * 6.3 + 16
    svg.append(f'<line x1="{left}" y1="{top+plot_h}" x2="{left+plot_w}" y2="{top+plot_h}" stroke="var(--axis)" stroke-width="1.3"/>')
    svg.append(f'<text x="{left+plot_w/2:.1f}" y="{h-4}" text-anchor="middle" font-size="11" fill="var(--text-secondary)">{xlab}</text>')
    svg.append("</svg>")
    return "\n".join(svg)


def network_svg(G, pos, highlight_node, labels, w=560, h=460, node_color="--series-1",
                 highlight_color="--status-critical", title=None, node_size_map=None):
    xs = [p[0] for p in pos.values()]; ys = [p[1] for p in pos.values()]
    pad = 55
    minx, maxx, miny, maxy = min(xs), max(xs), min(ys), max(ys)
    def sx(x): return pad + (x - minx) / (maxx - minx + 1e-9) * (w - 2 * pad)
    def sy(y): return pad + (y - miny) / (maxy - miny + 1e-9) * (h - 2 * pad)
    svg = [f'<svg viewBox="0 0 {w} {h}" width="100%" role="img" aria-label="{title or "network"}" xmlns="http://www.w3.org/2000/svg" font-family="{FONT}">']
    maxw = max(abs(d['weight']) for _, _, d in G.edges(data=True)) if G.number_of_edges() else 1
    for a, b, d in G.edges(data=True):
        x1, y1, x2, y2 = sx(pos[a][0]), sy(pos[a][1]), sx(pos[b][0]), sy(pos[b][1])
        wgt = abs(d['weight'])
        sw = 0.6 + (wgt / maxw) * 3.2
        op = 0.25 + (wgt / maxw) * 0.55
        touch = (a == highlight_node or b == highlight_node)
        col = f"var({highlight_color})" if touch else "var(--text-secondary)"
        svg.append(f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{col}" stroke-width="{sw:.2f}" opacity="{op:.2f}"/>')
    for n in G.nodes:
        x, y = sx(pos[n][0]), sy(pos[n][1])
        r = 9 if n != highlight_node else 13
        col = f"var({highlight_color})" if n == highlight_node else f"var({node_color})"
        svg.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="{col}" stroke="var(--surface-1)" stroke-width="2"/>')
        lbl = labels.get(n, n)
        dy = -16 if y < h/2 else 20
        svg.append(f'<text x="{x:.1f}" y="{y+dy:.1f}" text-anchor="middle" font-size="10.3" fill="var(--text-primary)" font-weight="{"700" if n==highlight_node else "400"}">{lbl}</text>')
    svg.append("</svg>")
    return "\n".join(svg)
