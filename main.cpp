#include <iostream>

extern "C" {
    double sum(double, double);
    double sub(double, double);
}


int main() {
    std::cout << "sum of 3.0 and 4.0: " << sum(3.0, 4.0) << std::endl;
    std::cout << "sub of 3.0 and 4.0: " << sub(3.0, 4.0) << std::endl;
}