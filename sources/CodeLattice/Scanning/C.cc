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
//@-node:gcross.20100315191926.1594:Type aliases
//@+node:gcross.20100728085419.1624:Macros
//@+node:gcross.20100728085419.1625:make_solve_fixed
#define make_solve_fixed(n) extern "C" void solve_##n(int number_of_qubits, int number_of_operators, int* restrict operator_table, int* restrict values, bool noisy, int* restrict number_of_stabilizers, int* restrict number_of_gauge_qubits, int* restrict number_of_logical_qubits, int ** restrict logical_qubit_distances) { solve_fixed<n>( number_of_qubits, number_of_operators, operator_table, values, noisy, number_of_stabilizers, number_of_gauge_qubits, number_of_logical_qubits, logical_qubit_distances); } std::ostream& operator<<(std::ostream& out, const static_quantum_operator<n>& op) { for(int i = 0; i < op.length(); i++) out.put(pauli_char_from_op(op,i)); return out; }
//@-node:gcross.20100728085419.1625:make_solve_fixed
//@-node:gcross.20100728085419.1624:Macros
//@+node:gcross.20100315191926.1439:Functions
//@+node:gcross.20100315191926.2794:solve
template<class operator_t,
         class qubit_vector_t,
         class operator_vector_t,
         class index_vector_t
        > void solve(
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
    qec<operator_t
       ,qubit_vector_t
       ,operator_vector_t
       ,index_vector_t
       > code(operators);
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
    int* distance_ptr = (*logical_qubit_distances) + (*number_of_logical_qubits) - 1;
    BOOST_FOREACH(size_t& distance, code.logical_qubit_error_distances) {
        (*distance_ptr) = distance;
        --distance_ptr;
    }
}
//@-node:gcross.20100315191926.2794:solve
//@+node:gcross.20100728085419.1622:<<
std::ostream& operator<<(std::ostream& out, const dynamic_quantum_operator& op) {
    for(int i = 0; i < op.length(); i++)
        out.put(pauli_char_from_op(op,i));
	return out;
}
//@-node:gcross.20100728085419.1622:<<
//@+node:gcross.20100728085419.1621:solve_any
extern "C" void solve_any(
    int number_of_qubits, int number_of_operators,
    int* restrict operator_table, int* restrict values,
    bool noisy,
    int* restrict number_of_stabilizers, int* restrict number_of_gauge_qubits,
    int* restrict number_of_logical_qubits, int ** restrict logical_qubit_distances
) {
    solve<dynamic_quantum_operator
         ,vector<qubit<dynamic_quantum_operator> >
         ,vector<dynamic_quantum_operator>
         ,vector<size_t>
        >
    (
        number_of_qubits, number_of_operators,
        operator_table, values,
        noisy,
        number_of_stabilizers, number_of_gauge_qubits,
        number_of_logical_qubits, logical_qubit_distances
    );
}
//@-node:gcross.20100728085419.1621:solve_any
//@+node:gcross.20100728085419.1623:solve_fixed
template<int N> void solve_fixed(
    int number_of_qubits, int number_of_operators,
    int* restrict operator_table, int* restrict values,
    bool noisy,
    int* restrict number_of_stabilizers, int* restrict number_of_gauge_qubits,
    int* restrict number_of_logical_qubits, int ** restrict logical_qubit_distances
) {
    solve<static_quantum_operator<N>
         ,static_vector<qubit<static_quantum_operator<N> >,N>
         ,static_vector<static_quantum_operator<N>,N*(N-1)>
         ,static_vector<size_t,N>
         >
    (
        number_of_qubits, number_of_operators,
        operator_table, values,
        noisy,
        number_of_stabilizers, number_of_gauge_qubits,
        number_of_logical_qubits, logical_qubit_distances
    );
}
//@-node:gcross.20100728085419.1623:solve_fixed
//@-node:gcross.20100315191926.1439:Functions
//@+node:gcross.20100728132013.1998:Solvers
make_solve_fixed(4)
make_solve_fixed(6)
make_solve_fixed(8)
make_solve_fixed(12)
make_solve_fixed(16)
make_solve_fixed(18)
make_solve_fixed(24)
make_solve_fixed(32)
make_solve_fixed(36)
make_solve_fixed(48)
make_solve_fixed(54)
make_solve_fixed(64)
make_solve_fixed(72)
make_solve_fixed(96)
make_solve_fixed(100)
make_solve_fixed(108)
make_solve_fixed(128)
make_solve_fixed(144)
make_solve_fixed(150)
make_solve_fixed(162)
make_solve_fixed(192)
make_solve_fixed(200)
make_solve_fixed(216)
make_solve_fixed(256)
make_solve_fixed(288)
make_solve_fixed(300)
make_solve_fixed(384)
make_solve_fixed(400)
make_solve_fixed(432)
make_solve_fixed(450)
make_solve_fixed(576)
make_solve_fixed(600)
make_solve_fixed(648)
make_solve_fixed(864)
//@-node:gcross.20100728132013.1998:Solvers
//@-others
//@-node:gcross.20100315191926.1436:@thin Scanning/C.cc
//@-leo
