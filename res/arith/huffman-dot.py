import heapq
from collections import defaultdict

class HuffmanNode:
    def __init__(self, char=None, freq=0, left=None, right=None):
        self.char = char
        self.freq = freq
        self.left = left
        self.right = right
        self.node_id = None  # Will be set when generating DOT
    
    def __lt__(self, other):
        # For heapq comparison - compare by frequency
        if self.freq != other.freq:
            return self.freq < other.freq
        # If frequencies are equal, prioritize leaf nodes (with characters)
        if self.char is None and other.char is not None:
            return False
        if self.char is not None and other.char is None:
            return True
        # If both are leaf nodes or both are internal nodes, compare by character
        return (self.char or '') < (other.char or '')

def build_huffman_tree(alphabet):
    """Build Huffman tree from alphabet frequencies"""
    # Create heap with all characters
    heap = []
    for char, freq in alphabet:
        node = HuffmanNode(char, freq)
        heapq.heappush(heap, node)
    
    # Build tree by combining nodes
    while len(heap) > 1:
        # Get two nodes with lowest frequencies
        left = heapq.heappop(heap)
        right = heapq.heappop(heap)
        
        # Create new internal node
        merged = HuffmanNode(
            char=None,
            freq=left.freq + right.freq,
            left=left,
            right=right
        )
        
        heapq.heappush(heap, merged)
    
    return heap[0]  # Root of the tree

def generate_codes(root):
    """Generate Huffman codes by traversing the tree"""
    if not root:
        return {}
    
    codes = {}
    
    def traverse(node, code=""):
        if node.char is not None:  # Leaf node
            codes[node.char] = code if code else "0"  # Handle single character case
        else:  # Internal node
            if node.left:
                traverse(node.left, code + "0")
            if node.right:
                traverse(node.right, code + "1")
    
    traverse(root)
    return codes

def generate_dot_graph(root, title="Huffman Tree"):
    """Generate DOT graph representation of the Huffman tree"""
    if not root:
        return ""
    
    dot_lines = []
    dot_lines.append(f'digraph "{title}" {{')
    dot_lines.append('    rankdir=TB;')
    dot_lines.append('    node [shape=circle, style=filled];')
    dot_lines.append('')
    
    # Assign unique IDs to nodes
    node_counter = [0]  # Use list to allow modification in nested function
    
    def assign_ids(node):
        if node:
            node.node_id = f"node{node_counter[0]}"
            node_counter[0] += 1
            assign_ids(node.left)
            assign_ids(node.right)
    
    assign_ids(root)
    
    # Generate node definitions
    def add_nodes(node):
        if node:
            if node.char:  # Leaf node
                label = f"{node.char}\\n{node.freq:.4f}"
                color = "lightblue"
            else:  # Internal node
                label = f"{node.freq:.4f}"
                color = "lightgray"
            
            dot_lines.append(f'    {node.node_id} [label="{label}", fillcolor="{color}"];')
            add_nodes(node.left)
            add_nodes(node.right)
    
    add_nodes(root)
    dot_lines.append('')
    
    # Generate edges
    def add_edges(node):
        if node:
            if node.left:
                dot_lines.append(f'    {node.node_id} -> {node.left.node_id} [label="0", color="red"];')
                add_edges(node.left)
            if node.right:
                dot_lines.append(f'    {node.node_id} -> {node.right.node_id} [label="1", color="blue"];')
                add_edges(node.right)
    
    add_edges(root)
    dot_lines.append('}')
    
    return '\n'.join(dot_lines)

def save_dot_file(dot_content, filename="huffman_tree.dot"):
    """Save DOT content to a file"""
    with open(filename, 'w') as f:
        f.write(dot_content)
    print(f"DOT graph saved to: {filename}")
    print(f"To generate image: dot -Tpng {filename} -o huffman_tree.png")
    print(f"Or view online at: http://magjac.com/graphviz-visual-editor/")

def calculate_average_length(codes, alphabet):
    """Calculate the average code length"""
    freq_dict = dict(alphabet)
    total_length = sum(freq_dict[char] * len(code) for char, code in codes.items())
    return total_length

def main():
    # Given alphabet frequencies
    alphabet = [
        ('A', 0.0858), ('B', 0.0135), ('C', 0.0316), ('D', 0.0356), ('E', 0.1197), 
        ('F', 0.0199), ('G', 0.0219), ('H', 0.0406), ('I', 0.0776), ('J', 0.0012),
        ('K', 0.0064), ('L', 0.0441), ('M', 0.0304), ('N', 0.0715), ('O', 0.0768), 
        ('P', 0.0240), ('Q', 0.0039), ('R', 0.0672), ('S', 0.0634), ('T', 0.0893),
        ('U', 0.0308), ('V', 0.0108), ('W', 0.0135), ('X', 0.0035), ('Y', 0.0156), 
        ('Z', 0.0014)
    ]
    
    print("Building Huffman Tree for English Alphabet")
    print("=" * 50)
    
    # Build Huffman tree
    root = build_huffman_tree(alphabet)
    
    # Generate codes
    codes = generate_codes(root)
    
    # Sort codes by frequency (descending) for better readability
    freq_dict = dict(alphabet)
    sorted_codes = sorted(codes.items(), key=lambda x: freq_dict[x[0]], reverse=True)
    
    # Print results
    print("\nHuffman Codes:")
    print("-" * 30)
    for char, code in sorted_codes:
        freq = freq_dict[char]
        print(f"{char}: {code:10s} (freq: {freq:.4f}, length: {len(code)})")
    
    # Calculate and print statistics
    avg_length = calculate_average_length(codes, alphabet)
    print(f"\nAverage code length: {avg_length:.4f} bits")
    
    # Compare with fixed-length encoding
    fixed_length = 5  # log2(26) ≈ 4.7, so we need 5 bits
    print(f"Fixed-length encoding: {fixed_length} bits per character")
    print(f"Compression ratio: {fixed_length/avg_length:.2f}:1")
    
    # Generate and save DOT graph
    print("\n" + "=" * 50)
    dot_content = generate_dot_graph(root, "Huffman Tree - English Alphabet")
    save_dot_file(dot_content)
    
    # Also print DOT content to console
    print("\nDOT Graph Content:")
    print("-" * 20)
    print(dot_content)

if __name__ == "__main__":
    main()
