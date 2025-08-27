#!/usr/bin/env python3
from Bio import SeqIO

def count_patterns(fasta_file, pos1, pos2, pattern1, pattern2):
    """
    Count specific nucleotide patterns at given positions in a FASTA file

    Args:
        fasta_file (str): Path to the FASTA file
        pos1 (int): First position (0-based)
        pos2 (int): Second position (0-based)
        pattern1 (str): First pattern to search at specified positions
        pattern2 (str): Second pattern to search at specified positions

    Returns:
        tuple: (Count of pattern1, Count of pattern2, Total number of sequences, Count of other patterns)
    """
    count_pattern1 = 0  # Count for pattern1 (e.g., "RG")
    count_pattern2 = 0  # Count for pattern2 (e.g., "KR")
    total_sequences = 0  # Total number of sequences
    else_count = 0  # Count of other patterns
    
    # Read the FASTA file
    for record in SeqIO.parse(fasta_file, "fasta"):
        total_sequences += 1
        sequence = str(record.seq)
        
        # Check sequence length
        if len(sequence) > pos2:
            # Check pattern1 (e.g., "RG")
            if sequence[pos1] == pattern1[0] and sequence[pos2] == pattern1[1]:
                count_pattern1 += 1
            
            # Check pattern2 (e.g., "KR")
            if sequence[pos1] == pattern2[0] and sequence[pos2] == pattern2[1]:
                count_pattern2 += 1

            # Count other patterns
            if sequence[pos1] != pattern1[0] and sequence[pos2] != pattern1[1]:
                if sequence[pos1] != pattern2[0] and sequence[pos2] != pattern2[1]:
                    else_count += 1
    
    return count_pattern1, count_pattern2, total_sequences, else_count

def main():
    # Set file path and search patterns
    fasta_file = "translated.fasta"  # Path to the FASTA file
    pos1 = 202  # 203rd position (0-based index)
    pos2 = 203  # 204th position (0-based index)
    pattern1 = "RG"  # Pattern 1: R at 203rd, G at 204th
    pattern2 = "KR"  # Pattern 2: K at 203rd, R at 204th
    
    try:
        # Count patterns
        count_ac, count_bd, total, else_count = count_patterns(fasta_file, pos1, pos2, pattern1, pattern2)
        
        # Display results
        print(f"Total number of sequences analyzed: {total}")
        print(f"Pattern '{pattern1[0]}' at position {pos1+1} and '{pattern1[1]}' at position {pos2+1}: {count_ac} sequences")
        print(f"Pattern '{pattern2[0]}' at position {pos1+1} and '{pattern2[1]}' at position {pos2+1}: {count_bd} sequences")
        print(f"Other patterns: {else_count} sequences")        

        # Calculate and display percentages
        if total > 0:
            percentage_ac = (count_ac / total) * 100
            percentage_bd = (count_bd / total) * 100
            print(f"Percentage of pattern '{pattern1}': {percentage_ac:.2f}%")
            print(f"Percentage of pattern '{pattern2}': {percentage_bd:.2f}%")
        
    except FileNotFoundError:
        print(f"Error: File '{fasta_file}' not found.")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main()
