from joints import draw_joints

draw_joints(
    n_boxes=9,
    groupings=[
        frozenset({0, 1}),
        frozenset({1, 2}),
        frozenset({2, 3}),
        frozenset({3, 4}),
        frozenset({4, 5}),
        frozenset({5, 6}),
        frozenset({6, 7}),
    ],
    output_path="joints-mut.svg",
)
