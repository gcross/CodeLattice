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
extern "C" int solve(
    int number_of_qubits, int number_of_operators,
    int* restrict operator_table, int* restrict values
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
    code.optimize_logical_qubits(false);
    if(code.logical_qubits.size() == 0)
        return 0;
    else
        return code.logical_qubit_error_distances.back();
}
//@-node:gcross.20100315191926.2794:solve
//@-node:gcross.20100315191926.1439:Functions
//@-others
//@-node:gcross.20100315191926.1436:@thin Scanning/C.cc
//@-leo
