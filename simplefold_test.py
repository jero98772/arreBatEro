"""
Protein structure prediction with SimpleFold - Fixed for memory issues.
"""

import sys
import os
import numpy as np
from math import pow
import py3Dmol
from pathlib import Path
from io import StringIO
from Bio.PDB import PDBIO, MMCIFParser, Superimposer
import torch

# Memory management settings
os.environ['PYTORCH_CUDA_ALLOC_CONF'] = 'expandable_segments:True'

# Force CPU usage if GPU memory is insufficient
USE_CPU = True  # Set to False if you have sufficient GPU memory

# Add SimpleFold to path
sys.path.append(str(Path("./src/simplefold").resolve()))

from src.simplefold.wrapper import ModelWrapper, InferenceWrapper


def calculate_tm_score(coords1, coords2, L_target=None):
    """Compute TM-score for two aligned coordinate sets (numpy arrays)."""
    assert coords1.shape == coords2.shape, "Aligned coords must have same shape"
    N = coords1.shape[0]

    if L_target is None:
        L_target = N

    dists = np.linalg.norm(coords1 - coords2, axis=1)

    d0 = 1.24 * pow(L_target - 15, 1 / 3) - 1.8
    if d0 < 0.5:
        d0 = 0.5

    score = np.sum(1.0 / (1.0 + (dists / d0) ** 2)) / L_target
    return score


def clear_gpu_cache():
    """Clear GPU cache to free up memory."""
    if torch.cuda.is_available():
        torch.cuda.empty_cache()
        print("GPU cache cleared.")


def main():
    # Clear any existing GPU cache
    clear_gpu_cache()
    
    # Example amino acid sequences
    example_sequences = {
        "7ftv_A": "GASKLRAVLEKLKLSRDDISTAAGMVKGVVDHLLLRLKCDSAFRGVGLLNTGSYYEHVKISAPNEFDVMFKLEVPRIQLEEYSNTRAYYFVKFKRNPKENPLSQFLEGEILSASKMLSKFRKIIKEEINDDTDVIMKRKRGGSPAVTLLISEKISVDITLALESKSSWPASTQEGLRIQNWLSAKVRKQLRLKPFYLVPKHAEETWRLSFSHIEKEILNNHGKSKTCCENKEEKCCRKDCLKLMKYLLEQLKERFKDKKHLDKFSSYHVKTAFFHVCTQNPQDSQWDRKDLGLCFDNCVTYFLQCLRTEKLENYFIPEFNLFSSNLIDKRSKEFLTKQIEYERNNEFPVFD",
        "8cny_A": "MGPSLDFALSLLRRNIRQVQTDQGHFTMLGVRDRLAVLPRHSQPGKTIWVEHKLINILDAVELVDEQGVNLELTLVTLDTNEKFRDITKFIPENISAASDATLVINTEHMPSMFVPVGDVVQYGFLNLSGKPTHRTMMYNFPTKAGQCGGVVTSVGKVIGIHIGGNGRQGFCAGLKRSYFAS",
        "8g8r_A": "GTVNWSVEDIVKGINSNNLESQLQATQAARKLLSREKQPPIDNIIRAGLIPKFVSFLGKTDCSPIQFESAWALTNIASGTSEQTKAVVDGGAIPAFISLLASPHAHISEQAVWALGNIAGDGSAFRDLVIKHGAIDPLLALLAVPDLSTLACGYLRNLTWTLSNLCRNKNPAPPLDAVEQILPTLVRLLHHNDPEVLADSCWAISYLTDGPNERIEMVVKKGVVPQLVKLLGATELPIVTPALRAIGNIVTGTDEQTQKVIDAGALAVFPSLLTNPKTNIQKEATWTMSNITAGRQDQIQQVVNHGLVPFLVGVLSKADFKTQKEAAWAITNYTSGGTVEQIVYLVHCGIIEPLMNLLSAKDTKIIQVILDAISNIFQAAEKLGETEKLSIMIEECGGLDKIEALQRHENESVYKASLNLIEKYFS",
    }

    # Use a smaller protein for testing if memory is limited
    seq_id = "8cny_A"  # Smaller protein (161 aa vs 351 aa)
    aa_sequence = example_sequences[seq_id]
    print(f"Predicting structure for {seq_id} with {len(aa_sequence)} amino acids.")

    # Model settings - optimized for lower memory usage
    simplefold_model = "simplefold_3B"  # Use smallest available model
    backend = "torch"  # Explicitly use torch
    ckpt_dir = "artifacts"
    output_dir = "artifacts"
    prediction_dir = f"predictions_{simplefold_model}_{backend}"
    output_name = f"{seq_id}"
    num_steps = 200  # Reduced from 500 for faster execution
    tau = 0.05
    plddt = True
    nsample_per_protein = 1

    try:
        # Initialize model with CPU fallback
        print("Initializing model...")
        model_wrapper = ModelWrapper(
            simplefold_model=simplefold_model,
            ckpt_dir=ckpt_dir,
            plddt=plddt,
            backend=backend,
        )
        
        # Force CPU usage if specified
        if USE_CPU:
            model_wrapper.device = torch.device("cpu")
            print("Using CPU for computation due to GPU memory constraints.")
        
        device = model_wrapper.device
        print(f"Using device: {device}")
        
        folding_model = model_wrapper.from_pretrained_folding_model()
        plddt_model = model_wrapper.from_pretrained_plddt_model()

        # Clear cache after model loading
        clear_gpu_cache()

        # Inference with memory-optimized settings
        print("Starting inference...")
        inference_wrapper = InferenceWrapper(
            output_dir=output_dir,
            prediction_dir=prediction_dir,
            num_steps=num_steps,
            tau=tau,
            nsample_per_protein=nsample_per_protein,
            device=device,
            backend=backend,
        )
        
        # Force CPU for ESM model if needed
        if USE_CPU and hasattr(inference_wrapper, 'esm_model'):
            inference_wrapper.esm_model = inference_wrapper.esm_model.cpu()
        
        batch, structure, record = inference_wrapper.process_input(aa_sequence)
        print(f"Batch info: {batch}")
        
        results = inference_wrapper.run_inference(batch, folding_model, plddt_model, device=device)
        save_paths = inference_wrapper.save_result(structure, record, results, out_name=output_name)

        pdb_path = save_paths[0]
        print(f"Structure saved to: {pdb_path}")

        # Visualization
        print("Creating visualization...")
        view = py3Dmol.view(query=pdb_path)
        if plddt:
            view.setStyle({"cartoon": {"colorscheme": {"prop": "b", "gradient": "roygb", "min": 0, "max": 100}}})
        else:
            view.setStyle({"cartoon": {"color": "spectrum"}})
        view.zoomTo()
        view.show()

        # TM-score and RMSD calculation (if reference structure exists)
        reference_path = f"assets/{seq_id}.cif"
        if Path(reference_path).exists():
            print("Calculating TM-score and RMSD...")
            parser = MMCIFParser(QUIET=True)
            struct1 = parser.get_structure("ref", reference_path)
            struct2 = parser.get_structure("prd", pdb_path)

            atoms1 = [a for a in struct1.get_atoms() if a.get_id() == "CA"]
            atoms2 = [a for a in struct2.get_atoms() if a.get_id() == "CA"]

            if len(atoms1) == len(atoms2):
                sup = Superimposer()
                sup.set_atoms(atoms1, atoms2)
                sup.apply(struct2.get_atoms())

                coords1 = np.array([a.coord for a in atoms1])
                coords2 = np.array([a.coord for a in atoms2])
                tm_score = calculate_tm_score(coords1, coords2)

                print(f"TM-score (0-1, higher is better): {tm_score:.3f}")
                print(f"RMSD (lower is better): {sup.rms:.3f}")

                # Save aligned structures for comparison
                io = PDBIO()
                s1_buf, s2_buf = StringIO(), StringIO()
                io.set_structure(struct1)
                io.save(s1_buf)
                io.set_structure(struct2)
                io.save(s2_buf)

                # Visualize GT vs predicted
                view = py3Dmol.view(width=600, height=400)
                view.addModel(s1_buf.getvalue(), "pdb")
                view.addModel(s2_buf.getvalue(), "pdb")

                view.setStyle({"model": 0}, {"cartoon": {"color": "blue"}})
                view.setStyle({"model": 1}, {"cartoon": {"color": "red"}})

                view.addLabel("Ground Truth", {"position": {"x": 0, "y": 0, "z": 0}, "backgroundColor": "blue", "fontColor": "white", "fontSize": 12})
                view.addLabel("Predicted", {"position": {"x": 0, "y": 4, "z": 0}, "backgroundColor": "red", "fontColor": "white", "fontSize": 12})

                view.zoomTo()
                view.show()
            else:
                print(f"Warning: Different number of CA atoms: ref={len(atoms1)}, pred={len(atoms2)}")
        else:
            print(f"Reference structure not found at {reference_path}")

    except torch.cuda.OutOfMemoryError as e:
        print(f"CUDA out of memory error: {e}")
        print("\nSuggestions to fix this:")
        print("1. Set USE_CPU = True at the top of the script")
        print("2. Use a smaller protein sequence")
        print("3. Close other GPU-using processes")
        print("4. Use a different SimpleFold model if available")
        
    except Exception as e:
        print(f"An error occurred: {e}")
        print("Check that all required files and models are available.")
    
    finally:
        # Clean up GPU memory
        clear_gpu_cache()


if __name__ == "__main__":
    main()
