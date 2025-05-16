#!/usr/bin/env python3
from Bio import SeqIO

def count_patterns(fasta_file, pos1, pos2, pattern1, pattern2):
    """
    FASTAファイルから特定の位置の塩基パターンをカウントする関数
    
    引数:
        fasta_file (str): FASTAファイルのパス
        pos1 (int): 1番目の位置（0ベース）
        pos2 (int): 2番目の位置（0ベース）
        pattern1 (str): 1番目の位置で検索する文字
        pattern2 (str): 2番目の位置で検索する文字
    
    戻り値:
        tuple: (パターン1のカウント, パターン2のカウント, 総配列数)
    """
    count_pattern1 = 0  # パターン1 (RG) のカウント
    count_pattern2 = 0  # パターン2 (KR) のカウント
    total_sequences = 0  # 総配列数
    else_count = 0 # その他のパターンのカウント
    
    # FASTAファイルを読み込む
    for record in SeqIO.parse(fasta_file, "fasta"):
        total_sequences += 1
        sequence = str(record.seq)
        
        # 配列の長さをチェック
        if len(sequence) > pos2:
            # パターン1 (RG) をチェック
            if sequence[pos1] == pattern1[0] and sequence[pos2] == pattern1[1]:
                count_pattern1 += 1
            
            # パターン2 (KR) をチェック
            if sequence[pos1] == pattern2[0] and sequence[pos2] == pattern2[1]:
                count_pattern2 += 1

            # その他のパターンをカウント
            if sequence[pos1] != pattern1[0] and sequence[pos2] != pattern1[1]:
                if sequence[pos1] != pattern2[0] and sequence[pos2] != pattern2[1]:
                    else_count += 1
    
    return count_pattern1, count_pattern2, total_sequences, else_count

def main():
    # ファイルパスと検索パターンの設定
    fasta_file = "SB_WIV04_nuc_amino_nucArea.translated.fasta"  # FASTAファイルのパス
    pos1 = 202  # 203番目の位置（0ベースなので202）
    pos2 = 203  # 204番目の位置（0ベースなので202）
    pattern1 = "RG"  # パターン1: 203番目がR、204番目がG
    pattern2 = "KR"  # パターン2: 203番目がK、204番目がR
    
    try:
        # パターンのカウント
        count_ac, count_bd, total, else_count = count_patterns(fasta_file, pos1, pos2, pattern1, pattern2)
        
        # 結果の表示
        print(f"解析した配列の総数: {total}")
        print(f"パターン '{pattern1[0]}' at position {pos1+1} and '{pattern1[1]}' at position {pos2+1}: {count_ac} 配列")
        print(f"パターン '{pattern2[0]}' at position {pos1+1} and '{pattern2[1]}' at position {pos2+1}: {count_bd} 配列")
        print(f"その他のパターン: {else_count} 配列")        

        # パーセンテージの計算と表示
        if total > 0:
            percentage_ac = (count_ac / total) * 100
            percentage_bd = (count_bd / total) * 100
            print(f"パターン '{pattern1}' の割合: {percentage_ac:.2f}%")
            print(f"パターン '{pattern2}' の割合: {percentage_bd:.2f}%")
        
    except FileNotFoundError:
        print(f"エラー: ファイル '{fasta_file}' が見つかりません。")
    except Exception as e:
        print(f"エラー: {e}")

if __name__ == "__main__":
    main()