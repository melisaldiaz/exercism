class Transcriptor {
    toRna(strand: string): string {
        const input: string[] = strand.split('');

        if (any(nonNucleotide, input)) { throw new Error('Invalid input DNA.') }

        else {
            for (let i = 0; i < input.length; i++) {
                input[i] = toRNAnuc(input[i]);
            }
        } return input.join('');
    }
}

// Returns the RNA complement to the given DNA nucleotide.
function toRNAnuc(x: string): string {
    let result: string = '';

    switch (x) {
        case 'G': result = 'C';
            break;

        case 'C': result = 'G';
            break;

        case 'T': result = 'A';
            break;

        case 'A': result = 'U';
            break;

    } return result;
}


// Determines if a character represents a Nucleotide.
function isNucleotide(n: string): boolean {
    const nucleotides: string[] = ['A', 'C', 'G', 'T']

    for (let i = 0; i < nucleotides.length; i++) {
        if (n === nucleotides[i]) { return true; }
    }
    return false;
}

function nonNucleotide(n: string): boolean {
    return !isNucleotide(n)
}

// Determines whether any element of the structure satisfies the predicate.
function any(predicate: (x: any) => boolean, x: any[]): boolean {
    for (let i = 0; i < x.length; i++) {
        if (predicate(x[i])) { return true }
    }
    return false;
}


export default Transcriptor