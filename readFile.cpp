#include <string>
#include <iostream>
#include <fstream>

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << argv[1] << "\n";
        return 1;
    }

    char c;
    while (file.get(c)) {
        std::cout << c;
    }
    std::cout << (int)c <<std::endl;

    return 0;
}
