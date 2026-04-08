from joints import draw_joints

draw_joints(
    n_boxes=9,
    groupings=[
        frozenset({1, 2}),
        frozenset({3, 4}),
        frozenset({5, 6}),
    ],
    output_path="joints-neq.svg",
)
