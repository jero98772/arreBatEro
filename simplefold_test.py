from Bio import Entrez, SeqIO

# Required: set your email (NCBI requires a contact email)

def fetch_fasta_by_accession(accession, out_file="sequence.fasta"):
    handle = Entrez.efetch(db="nucleotide", id=accession, rettype="fasta", retmode="text")
    records = list(SeqIO.parse(handle, "fasta"))
    handle.close()
    if not records:
        raise ValueError("No records returned")
    SeqIO.write(records, out_file, "fasta")
    return records

if __name__ == "__main__":
    accession = "NC_000913.3"   # example: E. coli K-12 MG1655 chromosome
    recs = fetch_fasta_by_accession(accession, "ecoli.fasta")
    print("Fetched", len(recs), "records. First record id:", recs[0].id)
