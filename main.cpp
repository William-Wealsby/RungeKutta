#include<iostream>
#include<fstream>
#include<cmath>

void initialise(){
    continue
}



int main(int argc, char** argv){

    std::string input = "";
    std::string output = "output/output.json";
    
    // uses values of argv piped in
    if (argc==3){
        input = argv[1];
        output = argv[2]; 
    }

    // define parameters of problem
    

    // import parameters from file if given in argv
    if (input!=""){
        std::ifstream inputfile(input);
        

    }

    // create memory for run

    return 0;
}
