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
        self.sequence = Sequence_elem(None)

    def __iadd__(self, seq_piece):
        self.sequence += Sequence_elem(seq_piece)
        return self

    def __add__(self, seq_piece):
        self.sequence += Sequence_elem(seq_piece)
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


class Sequence_elem:
    """This class represents the residue or nucleotide.
    """

    def __init__(self, elem_in_string):
        """This class will take a string like ACGTTTG and transform it into a
        list of sequence elements.
        Keyword Arguments:
        self           -- itself
        elem_in_string -- string
        """
        if elem_in_string is None:
            self.elements = []
        else:
            self.elements = [Element(elem) for elem in elem_in_string]

    def __add__(self, elements):
        self.elements += elements
        return self

    def __iadd__(self, elements):
        self.elements += elements
        return self

    def __getitem__(self, pos):
        return self.elements[pos]

    def __contains__(self, elem):
        return elem in self.elements

    def __len__(self):
        return len(self.elements)

    def __str__(self):
        return "".join(str(elem) for elem in self.elements)

    def __repr__(self):
        return "".join(str(elem) for elem in self.elements)


class Element:
    """This class represents amino-acids or nucleotides
    """

    def __init__(self, elem_char):
        """This constructor take a character like A or C and build a Element object
        Keyword Arguments:
        self      -- itself
        elem_char -- Residue or nucleotide as character
        """
        self.elem_name = elem_char
        self.elem_proterty = PROPERTIES[elem_char]
        self.elem_class = CLASSES[elem_char]

    def __str__(self):
        return self.elem_name

    def __repr__(self):
        return self.elem_name

    def __eq__(self, elem):
        return self.elem_name == elem.elem_name


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


global PROPERTIES, CLASSES
PROPERTIES, CLASSES = get_res_from_file(), get_res_from_file()


def main():
    """try this module
    """
    parser = argparse.ArgumentParser(description="""
    This module contains a fasta parser
                                                 """)
    parser.add_argument('-f', '--fasta', dest='fasta_file')
    args = parser.parse_args()
    global PROPERTIES, CLASSES
    PROPERTIES, CLASSES = get_res_from_file(), get_res_from_file()
    sequences = parse_fasta_file(args.fasta_file)
    print "".join(str(seq) for seq in sequences)


if __name__ == '__main__':
    main()
