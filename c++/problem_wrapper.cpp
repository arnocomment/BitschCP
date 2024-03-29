#include "headers/utilities.hpp"
#include "headers/problem_wrapper.hpp"
#include "headers/gecode_problem.hpp"

/**
 * Wraps the Problem constructor.
 * @todo modify this to include any parameters your Problem constructor requires
 * @return A pointer to a Problem object casted as a void*
 */
void* create_new_problem(string cantus_firmus, int species){
    vector<int> cf = convert_cf(cantus_firmus);
    return (void*) new Problem(cf, species);
}



/**
 * returns the size of the problem
 * @param sp a void* pointer to a Problem object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp){
    return static_cast<Problem*>(sp)->getSize();
}

/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a Problem object
 * @return an int* pointer representing the values of the variables
 */
int* return_solution(void* sp){
    return static_cast<Problem*>(sp)->return_solution();
}

/**
 * creates a search engine for Problem objects
 * @param sp a void* pointer to a Problem object
 * @return a void* cast of a Base<Problem>* pointer
 */
void* create_solver(void* sp, int type){
    return (void*) make_solver(static_cast<Problem*>(sp), type);
}

/**
 * returns the next solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a Base<Problem>* pointer for the search engine of the problem
 * @return a void* cast of a Problem* pointer
 */
void* return_next_solution_space(void* solver){
    return (void*) get_next_solution_space(static_cast<DFS<Problem>*>(solver));
}