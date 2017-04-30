import argparse
import re

# Description #################################################################
# This module contains some classes definition for fasta sequences
# representation and some basic functions.
# Description #################################################################


class Sequence:
    """Fasta sequence
    """

    def __init__(self, seq_name):
        """This is the model of sequence
        Keyword Arguments:
        """
        self.seq_name = seq_name
        self.sequence = ""

    def __iadd__(self, seq_piece):
        self.sequence += seq_piece.strip("\r\n")
        return self

    def __add__(self, seq_piece):
        self.sequence += seq_piece.strip("\r\n")
        return self

    def __len__(self):
        return len(self.sequence)

    def __getitem__(self, pos):
        return self.sequence[pos]

    def __str__(self):
        return ">{}\n{}\n".format(self.seq_name,
                                  fasta_format(str(self.sequence)))

    def __repr__(self):
        return ">{}\n{}\n".format(self.seq_name,
                                  fasta_format(str(self.sequence)))


def parse_fasta_file(fasta_file):
    """This function take a fasta_file and return a list of Sequence objects
    Keyword Arguments:
    fasta_file -- fasta file
    """
    with open(fasta_file) as fasta:
        fasta_sequences = []
        for line in fasta:
            if line.startswith(">"):
                cur_seq = Sequence(parse_header(line))
                fasta_sequences.append(cur_seq)
            elif not line.startswith(";") and len(line) > 0:
                cur_seq += line.strip("\r\n")
    return fasta_sequences


def parse_header(line):
    """Parse the fasta sequence header
    """
    try:
        return re.match("> *(\S+).+", line).group(1)
    except:
        return None


def get_res_from_file(data_file="./data/amino-acids.dat"):
    """Read residue information from data file
    """
    word = re.compile("\S+")
    results = {}
    with open(data_file) as res_file:
        for line in res_file:
            if not line.startswith("#") and len(line) > 0:
                val = word.findall(line)
                results[val[1]] = val[2]
    return results


def fasta_format(sequence, nb_col=80):
    """Write 80 characters per line.
    """
    if len(sequence) < nb_col:
        return sequence
    else:
        temp = ""
        i = 0
        while i < len(sequence):
            temp += sequence[i]
            i += 1
            if i % nb_col is 0:
                temp += "\n"
    return temp


def count_res(infile):
    """Count and make % of residues population
    """
    results = {}
    with open(infile) as fasta_file:
        for line in fasta_file:
            if not line.startswith(">") and not line.startswith(";"):
                for elem in set(line.strip()):
                    try:
                        results[elem] += line.count(elem)
                    except KeyError:
                        results[elem] = line.count(elem)
    return results


def print_stat(statistics):
    """Print statistics
    """
    nb_total = sum(statistics.values())
    results = []
    for elem, count in statistics.items():
        value = float(count) / nb_total
        results.append(
            str(elem) + "\t-" + str(round(100 * value, 2)) + "\t" + int(
                100 * value) * "=" + ">")
    return "\n".join(results)


# GLOBAL VARIABLES ############################################################
global PROPERTIES, CLASSES
PROPERTIES, CLASSES = get_res_from_file(), get_res_from_file()


def main():
    """try this module
    """
    parser = argparse.ArgumentParser(description="""
    This module contains a fasta parser
                                                 """)
    parser.add_argument('-f', '--fasta', dest='fasta_file')
    parser.add_argument('-a', '--action', dest='action')
    args = parser.parse_args()
    global PROPERTIES, CLASSES
    if args.action.upper() == "ALIGN":
        PROPERTIES, CLASSES = get_res_from_file(), get_res_from_file()
        sequences = parse_fasta_file(args.fasta_file)
        print "".join(str(seq) for seq in sequences)
    elif args.action.upper() == "COUNT":
        print print_stat(count_res(args.fasta_file))


if __name__ == '__main__':
    main()
