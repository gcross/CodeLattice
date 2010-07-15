//@+leo-ver=4-thin
//@+node:gcross.20100315191926.1436:@thin Scanning/C.cc
//@@language C++

//@<< Headers >>
//@+node:gcross.20100315191926.1437:<< Headers >>
#include <boost/foreach.hpp>
#include <vector>
#include "codelib.hpp"

using namespace std;

//@+others
//@-others
//@-node:gcross.20100315191926.1437:<< Headers >>
//@nl

//@+others
//@+node:gcross.20100315191926.1594:Type aliases
typedef dynamic_quantum_operator operator_t;
typedef vector<operator_t> operator_vector_t;
typedef qec<operator_t> qec_t;
//@-node:gcross.20100315191926.1594:Type aliases
//@+node:gcross.20100315191926.1439:Functions
//@+node:gcross.20100315191926.2794:solve
extern "C" void solve(
    int number_of_qubits, int number_of_operators,
    int* restrict operator_table, int* restrict values,
    bool noisy,
    int* restrict number_of_stabilizers, int* restrict number_of_gauge_qubits,
    int* restrict number_of_logical_qubits, int ** restrict logical_qubit_distances
) {
    operator_vector_t operators;
    operators.resize(number_of_operators);
    BOOST_FOREACH(operator_t& op, operators) {
        op.resize(number_of_qubits);
        op.set(operator_table[0],values[operator_table[1]]);
        op.set(operator_table[2],values[operator_table[3]]);
        operator_table += 4;
    }
    qec_t code(operators);
    code.optimize_logical_qubits(noisy);
    if (noisy) {
        cout << code << endl;
        cout << endl;
        cout << "Distances: ";
        BOOST_FOREACH(const int& distance, code.logical_qubit_error_distances) {
            cout << distance << " ";
        }
        cout << endl;
    }
    (*number_of_stabilizers) = code.stabilizers.size();
    (*number_of_gauge_qubits) = code.gauge_qubits.size();
    (*number_of_logical_qubits) = code.logical_qubits.size();
    (*logical_qubit_distances) = (int*) calloc((*number_of_logical_qubits),sizeof(int));
    for (int i = 0; i < (*number_of_logical_qubits); ++i)
        (*logical_qubit_distances)[i] = code.logical_qubit_error_distances[i];
}
//@-node:gcross.20100315191926.2794:solve
//@+node:gcross.20100316133702.1646:<<
std::ostream& operator<<(std::ostream& out, const dynamic_quantum_operator& op) {
    for(int i = 0; i < op.length(); i++)
        out.put(pauli_char_from_op(op,i));
	return out;
}
//@-node:gcross.20100316133702.1646:<<
//@-node:gcross.20100315191926.1439:Functions
//@-others
//@-node:gcross.20100315191926.1436:@thin Scanning/C.cc
//@-leo
