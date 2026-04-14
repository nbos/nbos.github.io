from joints import draw_joints

draw_joints(
    n_boxes=7,
    groupings=[
        frozenset({0, 1}),
        frozenset({1, 2}),
        frozenset({4, 5}),
        frozenset({5, 6}),
    ],
    labels=["b", "c", "c", "d", "a", "b", "b"],
    margin_x=0.05,
    lw=2.0,
    output_path="string-fig-chunked.svg",
)
