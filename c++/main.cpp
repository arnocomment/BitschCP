#include "headers/gecode_problem.hpp"
#include "headers/utilities.hpp"

using namespace Gecode;
using namespace std;

int main(int argc, char* argv[]) {
    int size = 3;
    // create a new problem
    string cf_str = "600062006500640067006500640062006000";
    vector<int> cf = convert_cf(cf_str);
    for (auto i:cf) {
        std::cout << i << ' ';
    }
    std::cout << endl;
    
    //Problem* p = new Problem(size, 1);
    //printf("here");
    //// create a new search engine
    //Search::Base<Problem>* e = make_solver(p, bab_solver);
    //delete p;
    //int nb_sol = 0;
    //while(Problem * pb = get_next_solution_space(e)){
    //    nb_sol++;
    //    cout << "Solution " << nb_sol << ": " << endl;
    //    pb->print_solution();
    //    delete pb;
    //}
    //cout << "No (more) solutions." << endl;
    return 0;
}

