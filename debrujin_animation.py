import numpy as np
import matplotlib.pyplot as plt
import networkx as nx
from matplotlib.animation import FuncAnimation
from collections import defaultdict
import matplotlib.patches as patches

class DeBruijnGraphAnimation:
    def __init__(self):
        self.fig, (self.ax1, self.ax2, self.ax3) = plt.subplots(1, 3, figsize=(18, 6))
        self.fig.suptitle('Genome Assembly using De Bruijn Graph', fontsize=16, fontweight='bold')
        
        # Input reads (length 6 each)
        self.reads = ["ATGCTA", "TGCTAG", "GCTAGC", "CTAGCA"]
        self.k = 3
        self.current_step = 0
        self.assembly_sequence = ""
        
        # Setup axes
        self.setup_axes()
        
        # Generate k-mers and build graph
        self.kmers = self.generate_kmers()
        self.graph = self.build_de_bruijn_graph()
        self.eulerian_path = self.find_eulerian_path()
        
    def setup_axes(self):
        # Axes for displaying reads
        self.ax1.set_title('Step 1: Input Reads')
        self.ax1.set_xlim(0, 10)
        self.ax1.set_ylim(0, 5)
        self.ax1.axis('off')
        
        # Axes for k-mer breakdown
        self.ax2.set_title('Step 2: K-mer Breakdown (k=3)')
        self.ax2.set_xlim(0, 10)
        self.ax2.set_ylim(0, 8)
        self.ax2.axis('off')
        
        # Axes for graph visualization
        self.ax3.set_title('Step 3: De Bruijn Graph Assembly')
        self.ax3.set_xlim(-1.5, 1.5)
        self.ax3.set_ylim(-1.5, 1.5)
        self.ax3.axis('off')
        
    def generate_kmers(self):
        kmers = []
        for read in self.reads:
            read_kmers = []
            for i in range(len(read) - self.k + 1):
                kmer = read[i:i + self.k]
                read_kmers.append(kmer)
            kmers.append(read_kmers)
        return kmers
    
    def build_de_bruijn_graph(self):
        graph = nx.DiGraph()
        
        # Add nodes (k-1 mers)
        node_set = set()
        for read_kmers in self.kmers:
            for kmer in read_kmers:
                prefix = kmer[:-1]
                suffix = kmer[1:]
                node_set.add(prefix)
                node_set.add(suffix)
        
        for node in node_set:
            graph.add_node(node)
        
        # Add edges (k-mers)
        edge_count = defaultdict(int)
        for read_kmers in self.kmers:
            for kmer in read_kmers:
                prefix = kmer[:-1]
                suffix = kmer[1:]
                edge_count[(prefix, suffix, kmer)] += 1
        
        for (prefix, suffix, kmer), count in edge_count.items():
            graph.add_edge(prefix, suffix, kmer=kmer, weight=count)
        
        return graph
    
    def find_eulerian_path(self):
        # Find start node (with more outgoing than incoming edges)
        start_node = None
        for node in self.graph.nodes():
            in_degree = self.graph.in_degree(node)
            out_degree = self.graph.out_degree(node)
            if out_degree > in_degree:
                start_node = node
                break
        
        if start_node is None:
            start_node = list(self.graph.nodes())[0]
        
        # Hierholzer's algorithm for Eulerian path
        stack = [start_node]
        path = []
        
        while stack:
            current = stack[-1]
            if self.graph.out_degree(current) > 0:
                next_node = next(self.graph.neighbors(current))
                stack.append(next_node)
                self.graph.remove_edge(current, next_node)
            else:
                path.append(stack.pop())
        
        path.reverse()
        
        # Rebuild the graph since we removed edges
        self.graph = self.build_de_bruijn_graph()
        
        return path
    
    def reconstruct_sequence(self, path):
        if len(path) < 2:
            return ""
        
        sequence = path[0]
        for i in range(1, len(path)):
            # Add the last character of each node
            sequence += path[i][-1]
        
        return sequence
    
    def draw_reads(self):
        self.ax1.clear()
        self.ax1.set_title('Step 1: Input Reads (Length 6)', fontweight='bold')
        self.ax1.set_xlim(0, 10)
        self.ax1.set_ylim(0, 5)
        self.ax1.axis('off')
        
        for i, read in enumerate(self.reads):
            y_pos = 4 - i
            # Draw read as a colored rectangle
            rect = patches.Rectangle((1, y_pos - 0.3), 6, 0.6, 
                                   linewidth=2, edgecolor='blue', 
                                   facecolor='lightblue', alpha=0.7)
            self.ax1.add_patch(rect)
            
            # Add read text
            self.ax1.text(4, y_pos, read, ha='center', va='center', 
                         fontsize=12, fontweight='bold', color='darkblue')
            
            # Add read label
            self.ax1.text(0.5, y_pos, f'Read {i+1}:', ha='right', va='center', 
                         fontsize=10, fontweight='bold')
    
    def draw_kmers(self):
        self.ax2.clear()
        self.ax2.set_title('Step 2: K-mer Breakdown (k=3)', fontweight='bold')
        self.ax2.set_xlim(0, 10)
        self.ax2.set_ylim(0, 8)
        self.ax2.axis('off')
        
        colors = ['lightcoral', 'lightgreen', 'lightyellow', 'lightcyan']
        
        for read_idx, read_kmers in enumerate(self.kmers):
            y_pos = 7 - read_idx * 1.5
            
            # Draw original read
            self.ax2.text(0.5, y_pos, f'Read {read_idx+1}:', ha='right', va='center', 
                         fontsize=10, fontweight='bold')
            self.ax2.text(1.5, y_pos, self.reads[read_idx], ha='left', va='center', 
                         fontsize=11, fontweight='bold', color='blue')
            
            # Draw arrow
            self.ax2.annotate('', xy=(4, y_pos), xytext=(2.5, y_pos),
                            arrowprops=dict(arrowstyle='->', lw=2, color='gray'))
            
            # Draw k-mers
            for kmer_idx, kmer in enumerate(read_kmers):
                x_pos = 4.5 + kmer_idx * 1.2
                rect = patches.Rectangle((x_pos - 0.4, y_pos - 0.3), 0.8, 0.6,
                                       linewidth=2, edgecolor='red',
                                       facecolor=colors[read_idx % len(colors)], alpha=0.8)
                self.ax2.add_patch(rect)
                self.ax2.text(x_pos, y_pos, kmer, ha='center', va='center',
                            fontsize=10, fontweight='bold')
    
    def draw_graph(self, highlight_path=None):
        self.ax3.clear()
        self.ax3.set_title('Step 3: De Bruijn Graph Assembly', fontweight='bold')
        self.ax3.set_xlim(-1.5, 1.5)
        self.ax3.set_ylim(-1.5, 1.5)
        self.ax3.axis('off')
        
        # Create a circular layout for better visualization
        pos = nx.circular_layout(self.graph)
        
        # Draw nodes
        nx.draw_networkx_nodes(self.graph, pos, node_color='lightblue', 
                              node_size=1500, ax=self.ax3, alpha=0.9)
        
        # Draw node labels
        nx.draw_networkx_labels(self.graph, pos, font_size=10, 
                               font_weight='bold', ax=self.ax3)
        
        # Draw edges with arrows and labels
        edge_colors = []
        edge_widths = []
        
        for u, v, data in self.graph.edges(data=True):
            if highlight_path and (u, v) in highlight_path:
                edge_colors.append('red')
                edge_widths.append(3)
            else:
                edge_colors.append('black')
                edge_widths.append(2)
        
        nx.draw_networkx_edges(self.graph, pos, edge_color=edge_colors,
                              width=edge_widths, arrows=True, arrowsize=20,
                              arrowstyle='->', ax=self.ax3)
        
        # Add edge labels (k-mers)
        edge_labels = {(u, v): data['kmer'] for u, v, data in self.graph.edges(data=True)}
        nx.draw_networkx_edge_labels(self.graph, pos, edge_labels=edge_labels,
                                   font_size=8, font_weight='bold', ax=self.ax3)
        
        # Display assembly progress
        if self.assembly_sequence:
            self.ax3.text(0, -1.3, f'Assembled: {self.assembly_sequence}', 
                         ha='center', va='center', fontsize=12, fontweight='bold',
                         bbox=dict(boxstyle="round,pad=0.3", facecolor="lightgreen"))
    
    def update(self, frame):
        self.current_step = frame
        
        if frame == 0:
            # Show initial reads
            self.draw_reads()
            self.ax2.clear()
            self.ax2.axis('off')
            self.ax3.clear()
            self.ax3.axis('off')
            
        elif frame == 1:
            # Show k-mer breakdown
            self.draw_reads()
            self.draw_kmers()
            self.ax3.clear()
            self.ax3.axis('off')
            
        elif frame == 2:
            # Show initial graph
            self.draw_reads()
            self.draw_kmers()
            self.draw_graph()
            
        elif frame >= 3:
            # Show Eulerian path traversal
            self.draw_reads()
            self.draw_kmers()
            
            # Highlight path up to current step
            path_edges = []
            if frame - 3 < len(self.eulerian_path) - 1:
                for i in range(frame - 3):
                    if i < len(self.eulerian_path) - 1:
                        path_edges.append((self.eulerian_path[i], self.eulerian_path[i+1]))
                
                self.assembly_sequence = self.reconstruct_sequence(self.eulerian_path[:frame-2])
                self.draw_graph(path_edges)
            else:
                # Final assembly
                self.assembly_sequence = self.reconstruct_sequence(self.eulerian_path)
                self.draw_graph([(self.eulerian_path[i], self.eulerian_path[i+1]) 
                               for i in range(len(self.eulerian_path)-1)])
                
                # Display final result
                self.ax3.text(0, -1.5, '✓ Assembly Complete!', 
                             ha='center', va='center', fontsize=14, fontweight='bold',
                             color='green')
    
    def animate(self):
        # Total frames: 0=reads, 1=kmers, 2=graph, then path traversal + 1 for final
        total_frames = 3 + len(self.eulerian_path)
        
        anim = FuncAnimation(self.fig, self.update, frames=total_frames,
                           interval=1500, repeat=False)
        
        plt.tight_layout()
        plt.show()
        
        return anim

# Create and run the animation
def main():
    print("Genome Assembly using De Bruijn Graph")
    print("=" * 50)
    print("Input Reads (length 6):")
    reads = ["ATGCTA", "TGCTAG", "GCTAGC", "CTAGCA"]
    for i, read in enumerate(reads, 1):
        print(f"Read {i}: {read}")
    print()
    
    animation = DeBruijnGraphAnimation()
    anim = animation.animate()
    
    # Final result
    final_sequence = animation.reconstruct_sequence(animation.eulerian_path)
    print(f"\nFinal Assembled Sequence: {final_sequence}")
    print(f"Length: {len(final_sequence)} bases")

if __name__ == "__main__":
    main()
