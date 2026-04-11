import numpy as np
from graph import draw_graph

R = 2.5   # half-side of outer square

vertex_pos = {
    "a": np.array([-R,  R]),
    "b": np.array([ R,  R]),
    "c": np.array([ R, -R]),
    "d": np.array([-R, -R]),
    "e": np.array([ 0.,  0.]),
}

vertex_labels = {
    "a": "a",
    "b": "b",
    "c": "c",
    "d": "d",
    "e": "ε",
}

edges = (
    [("a", "e"), ("e", "a")]
  + [("b", "e"), ("e", "b")] * 3
  + [("c", "e"), ("e", "c")] * 2
  + [("d", "e"), ("e", "d")]
)

draw_graph(
    vertex_pos    = vertex_pos,
    edges         = edges,
    filename      = "graph-cat.svg",
    vertex_labels = vertex_labels,
)
