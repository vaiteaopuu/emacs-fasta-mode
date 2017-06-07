import argparse
import numpy as np
import seq_tools as st

# Description #################################################################
# This module contains function to perform a pairwise alignment.
# Description #################################################################

global GAP_PENALITY


def create_matrix(seq_a, seq_b, matrix):
    """This function take 2 sequences and return 2 matrix.
    Keyword Arguments:
    """
    len_seq_a, len_seq_b = len(seq_a), len(seq_b)
    score_init = lambda (i, j): i * GAP_PENALITY if j == 0 else (j * GAP_PENALITY if i == 0 else 0)
    path_init = lambda (i, j): "l" if j == 0 else ("u" if i == 0 else None)

    score_matrix = [[score_init((i, j)) for i in range(len_seq_a + 1)]
                    for j in range(len_seq_b + 1)]
    path_matrix = [[path_init((i, j)) for i in range(len_seq_a + 1)]
                   for j in range(len_seq_b + 1)]

    for ind_a, elem_a in enumerate(seq_a, start=1):
        for ind_b, elem_b in enumerate(seq_b, start=1):
            if ind_a == 0:
                score_matrix[ind_b][ind_a] = GAP_PENALITY * ind_b
                path_matrix[ind_b][ind_a] = "u"
            elif ind_b == 0:
                score_matrix[ind_b][ind_a] = GAP_PENALITY * ind_a
                path_matrix[ind_b][ind_a] = "l"
            else:
                fill_matrix(ind_a, ind_b, elem_a, elem_b, score_matrix,
                            path_matrix)

    return score_matrix, path_matrix


def fill_matrix(ind_a, ind_b, elem_a, elem_b, score_matrix, path_matrix):
    """This function take the index of elem a from the first sequence and the
    second sequence. This function will modified the path and score matrix.
    Keyword Arguments:
    """
    score_a_b = get_score(elem_a, elem_b)
    results = [
        ("d", score_a_b + score_matrix[ind_b - 1][ind_a - 1]),
        ("u", score_a_b + score_matrix[ind_b - 1][ind_a] - GAP_PENALITY),
        ("l", score_a_b + score_matrix[ind_b][ind_a - 1] - GAP_PENALITY)
    ]
    path, score = max(results, key=lambda (x, y): y)
    score_matrix[ind_b][ind_a] = score
    path_matrix[ind_b][ind_a] = path


def get_score(elem_a, elem_b, matrix_substitution=None):
    """This function take the elements.
    Keyword Arguments:
    """
    if matrix_substitution is not None:
        return matrix_substitution[elem_a][elem_b]
    else:
        return 1 if elem_a == elem_b else 0


def backtracking(path_matrix, seq_a, seq_b):
    """This function take the path matrix in order to get the alignment of
    sequences.
    """
    alignment_a = ""
    alignment_b = ""
    pos_i, pos_j = len(seq_b), len(seq_a)
    while pos_i >= 0 and pos_j >= 0:
        if pos_i == 0:
            alignment_a += seq_a[:pos_j][::-1]
            alignment_b += "-" * len(seq_b[:pos_j])
            break
        elif pos_j == 0:
            alignment_a += "-" * len(seq_a[:pos_i])
            alignment_b += seq_b[:pos_i][::-1]
            break
        elif path_matrix[pos_i][pos_j] == "d":
            alignment_a += seq_a[pos_j - 1]
            alignment_b += seq_b[pos_i - 1]
            pos_i -= 1
            pos_j -= 1
        elif path_matrix[pos_i][pos_j] == "u":
            alignment_a += "-"
            alignment_b += seq_b[pos_i - 1]
            pos_i -= 1
        elif path_matrix[pos_i][pos_j] == "l":
            alignment_a += seq_a[pos_j - 1]
            alignment_b += "-"
            pos_j -= 1
    return alignment_a[::-1], alignment_b[::-1]


def main():
    """This function take 2 sequences.
    """
    parser = argparse.ArgumentParser(description="""
    This module contains a fasta parser
                                                 """)
    parser.add_argument('-f', '--fasta', dest='fasta_file')
    parser.add_argument(
        '-p', '--penality', dest='penality', type=int, default=1)
    args = parser.parse_args()
    global GAP_PENALITY
    GAP_PENALITY = args.penality
    sequences = st.parse_fasta_file(args.fasta_file)
    score_matrix, path_matrix = create_matrix(sequences[0], sequences[1], None)
    alignment_a, alignment_b = backtracking(path_matrix, sequences[0],
                                            sequences[1])
    print "ALIGNEMENT"
    print "\n".join(
        map(str, [
            sequences[0].sequence, alignment_a, alignment_b, sequences[1]
            .sequence
        ]))


if __name__ == '__main__':
    main()
