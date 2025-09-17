#!/usr/bin/env python3
import numpy as np
import pandas as pd
from scipy import stats
import matplotlib.pyplot as plt
import seaborn as sns
import math

def g_test(observed):
    """
    Function to perform G-test (Likelihood Ratio Test)
    
    Args:
        observed (ndarray): 2x2 array of observed values
    
    Returns:
        tuple: (G statistic, p-value, degrees of freedom)
    """
    # Calculate row and column sums
    row_sums = observed.sum(axis=1)
    col_sums = observed.sum(axis=0)
    total = observed.sum()
    
    # Calculate expected values
    expected = np.outer(row_sums, col_sums) / total
    
    # Calculate G value (if 0, treat 0*ln(0)=0)
    G = 0
    for i in range(observed.shape[0]):
        for j in range(observed.shape[1]):
            if observed[i, j] > 0:  # Avoid 0 * log(0) issue
                G += observed[i, j] * math.log(observed[i, j] / expected[i, j])
    
    G = 2 * G
    
    # Calculate degrees of freedom
    dof = (observed.shape[0] - 1) * (observed.shape[1] - 1)
    
    # Calculate p-value
    p_value = 1 - stats.chi2.cdf(G, dof)
    
    return G, p_value, dof, expected

def perform_statistical_analysis(group1_wild, group1_mutant, group2_wild, group2_mutant):
    """
    Function to perform significance tests for viral mutation between two groups
    
    Args:
        group1_wild (int): Wild type count for group 1
        group1_mutant (int): Mutant count for group 1
        group2_wild (int): Wild type count for group 2
        group2_mutant (int): Mutant count for group 2
    
    Returns:
        dict: Dictionary containing test results
    """
    # Create contingency table
    contingency_table = np.array([
        [group1_wild, group1_mutant],
        [group2_wild, group2_mutant]
    ])
    
    # Perform G-test
    G, g_p_value, dof, expected = g_test(contingency_table)
    
    # Chi-square test (for comparison)
    chi2, chi2_p_value, _, _ = stats.chi2_contingency(contingency_table)
    
    # Fisher's exact test (for small samples)
    oddsratio, fisher_p = stats.fisher_exact(contingency_table)
    
    # Return results as a dictionary
    results = {
        'contingency_table': contingency_table,
        'expected': expected,
        'G': G,
        'g_p_value': g_p_value,
        'chi2': chi2,
        'chi2_p_value': chi2_p_value,
        'dof': dof,
        'oddsratio': oddsratio,
        'fisher_p_value': fisher_p
    }
    
    return results

def display_results(results):
    """
    Function to display test results
    
    Args:
        results (dict): Dictionary containing test results
    """
    print("\n=== Test Results ===")
    print("\nContingency Table:")
    df = pd.DataFrame(
        results['contingency_table'],
        index=['Group 1', 'Group 2'],
        columns=['Wild Type', 'Mutant']
    )
    print(df)
    
    print("\nExpected Values:")
    df_expected = pd.DataFrame(
        results['expected'],
        index=['Group 1', 'Group 2'],
        columns=['Wild Type', 'Mutant']
    )
    print(df_expected.round(2))
    
    # Calculate totals and proportions for each group
    row_sums = df.sum(axis=1)
    proportions = df.div(row_sums, axis=0) * 100
    
    print("\nMutant Proportion in Each Group:")
    print(f"Group 1: {proportions.iloc[0, 1]:.2f}% ({df.iloc[0, 1]}/{row_sums[0]})")
    print(f"Group 2: {proportions.iloc[1, 1]:.2f}% ({df.iloc[1, 1]}/{row_sums[1]})")
    
    print(f"\nG value: {results['G']:.4f}")
    print(f"Degrees of freedom: {results['dof']}")
    print(f"G-test p-value: {results['g_p_value']:.6f}")
    
    if results['g_p_value'] < 0.05:
        print("Result: Statistically significant difference (p < 0.05)")
    else:
        print("Result: No statistically significant difference (p >= 0.05)")
    
    print(f"\nComparison: Chi-square test p-value: {results['chi2_p_value']:.6f}")
    print(f"Comparison: Fisher's exact test p-value: {results['fisher_p_value']:.6f}")
    
    print(f"\nOdds ratio: {results['oddsratio']:.4f}")

    """
    Function to visualize test results
    
    Args:
        results (dict): Dictionary containing test results
    """
    # Convert contingency table to DataFrame
    df = pd.DataFrame(
        results['contingency_table'],
        index=['Group 1', 'Group 2'],
        columns=['Wild Type', 'Mutant']
    )
    
    # Calculate totals for each group
    row_sums = df.sum(axis=1)
    
    # Calculate proportions
    proportions = df.div(row_sums, axis=0) * 100
    
    # Create plots
    fig, axs = plt.subplots(1, 3, figsize=(18, 6))
    
    # Subplot 1: Bar plot (counts)
    df.plot(kind='bar', ax=axs[0])
    axs[0].set_title('Distribution of Wild Type and Mutant (Counts)')
    axs[0].set_ylabel('Count')
    axs[0].set_xticks(range(len(df.index)))
    axs[0].set_xticklabels(df.index, rotation=0)
    
    # Subplot 2: Bar plot (proportions)
    proportions.plot(kind='bar', ax=axs[1])
    axs[1].set_title('Distribution of Wild Type and Mutant (Proportion)')
    axs[1].set_ylabel('Proportion (%)')
    axs[1].set_xticks(range(len(proportions.index)))
    axs[1].set_xticklabels(proportions.index, rotation=0)
    
    for container in axs[1].containers:
        axs[1].bar_label(container, fmt='%.1f%%')
    
    # Subplot 3: Heatmap of contingency table
    sns.heatmap(df, annot=True, fmt='d', cmap='YlGnBu', cbar=False, ax=axs[2])
    axs[2].set_title('Heatmap of Contingency Table')
    
    # Display p-values
    fig.suptitle(
        f"Statistical Analysis of Viral Mutation\nG-test p-value: {results['g_p_value']:.6f}, "
        f"Chi-square test p-value: {results['chi2_p_value']:.6f}, "
        f"Fisher's exact test p-value: {results['fisher_p_value']:.6f}"
    )
    
    plt.tight_layout()
    plt.savefig('virus_mutation_g_test.png', dpi=300, bbox_inches='tight')
    plt.show()

def main():
    # Sample data - replace with actual data
    # Sample size per group: 2917
    
    # Data for group 1
    group1_wild = 7  # Wild type count for group 1
    group1_mutant = 2391  # Mutant count for group 1
    
    # Data for group 2
    group2_wild = 7  # Wild type count for group 2
    group2_mutant = 2739  # Mutant count for group 2
    
    # Confirm input values
    print(f"Group 1: Wild type {group1_wild}, Mutant {group1_mutant}, Total {group1_wild + group1_mutant}")
    print(f"Group 2: Wild type {group2_wild}, Mutant {group2_mutant}, Total {group2_wild + group2_mutant}")
    
    # Perform statistical analysis
    results = perform_statistical_analysis(group1_wild, group1_mutant, group2_wild, group2_mutant)
    
    # Display results
    display_results(results)
    

if __name__ == "__main__":
    main()