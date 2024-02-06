#include "headers/gecode_problem.hpp"
#include "headers/utilities.hpp"

using namespace Gecode;
using namespace std;

int main(int argc, char* argv[]) {
    // CANTUS FIRMUS AS SENDED BY OPENMUSIC VIA CFFI
    string cf_str = "600062006500640067006500640062006000";
    int species = 1;            // TODO : implement florid counterpoint

    // CONVERSION INTO VECTOR AND MAKE PROBLEM (as done in function create_new_problem)
    vector<int> cf = convert_cf(cf_str);
    Problem* p = new Problem(cf, species);

    std::cout << "Cantus Firmus MIDI values : ";
    for (auto i:p->getCf()) {
        std::cout << i << ' ';
    }
    std::cout << endl;
    std::cout << "Size of the CSP           : " << p->getSize() << endl;
    //// create a new search engine
    Search::Base<Problem>* e = make_solver(p, dfs_solver);
    delete p;
    int nb_sol = 0;
    while(Problem * pb = get_next_solution_space(e)){
        nb_sol++;
        cout << "Solution " << nb_sol << ": " << endl;
        pb->print_solution();
        delete pb;
        // TMP to find only the 10 first solutions and make the code terminate
        if (nb_sol == 10) {
            break;
        }
    }
    cout << "No (more) solutions." << endl;
    return 0;
}

