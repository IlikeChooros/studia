#include <iostream>
#include <memory>
#include <vector>
#include <sstream>
#include <iomanip>

#include "figures.hpp"

class InvalidParametersError : public std::invalid_argument {
public:
    InvalidParametersError(const FigureType& type)
        : std::invalid_argument("Nieprawdiłowe parametry dla figury: " + std::string(figure_type_to_string(type))) {}
};

// Check if the char can be represented as a figure type
inline bool is_figure_type(char type)
{
    return type == (char)CircleType || type == (char)PentagonType || type == (char)HexagonType || type == (char)FourSidedType;
}

inline int n_params_by_type(char type)
{
    if (type == (char)FourSidedType)
        return 5;
    return 1;
}

// Create a figure base on the type and parameters, it's heap allocated, should be deleted after use.
// Throws: std::invalid_argument if the parameters are invalid for the given type
[[nodiscard]]
Figure* get_figure(char type, std::vector<double>& params)
{
    if (type == (char)CircleType)
    {
        if (Circle::check(params[0]))
            return new Circle(params[0]);
        
        throw InvalidParametersError(CircleType);
    }

    if (type == (char)PentagonType || type == (char)HexagonType)
    {
        if (params[0] > 0)
        {
            if (type == (char)PentagonType)
                return new Pentagon(params[0]);
            return new Hexagon(params[0]);
        }
        
        throw InvalidParametersError((FigureType)type);
    }

    // That's a four sided figure, so check the parameters
    auto figtype = FourSided::check(params[0], params[1], params[2], params[3], params[4]);
    if (figtype == None)
        throw InvalidParametersError(FourSidedType);
    
    switch(figtype)
    {
        case SquareType:
            return new Square(params[0]);
        case RhombusType:
            return new Rhombus(params[0], params[1]);
        default:
            return new Rectangle(params[0], params[1]);
    }
}

void find_type_token(std::stringstream& ss)
{
    std::string token;
    while (ss >> token)
    {
        if (token.size() == 1 && is_figure_type(token[0]))
        {
            ss.seekg(-1, std::ios_base::cur);
            return;
        }
    }
}

// Parse the arguments and return a figure
// Throws: std::invalid_argument if the parameters are invalid for the given type
[[nodiscard]] 
Figure* parse_args(std::stringstream& ss)
{
    std::vector<double> params{5, 0};

    char type;
    if (!(ss >> type))
        throw std::invalid_argument("Błędny typ figury");
    
    if (!is_figure_type(type))
        throw std::invalid_argument("Nieznany typ figury: " + std::string(1, type));


    auto n_params = n_params_by_type(type);
    for (int i = 0; i < n_params; i++)
    {
        double param;
        if (!(ss >> param)) // assert that's a number
        {
            if (ss.eof())
                throw std::invalid_argument("Brakujący parametr dla figury: " + std::string(1, type));
            throw std::invalid_argument("Błędny parametr figury: " + std::string(1, type));
        }
        
        params[i] = param;
    }

    return get_figure(type, params);
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <figure_type> [<params>...]" << '\n';
        return 1;
    }

    std::vector<std::shared_ptr<Figure>> figures;
    std::stringstream ss;
    
    // Create a string stream to read the parameters
    for (int i = 1; i < argc; i++) {
        ss << argv[i];
        if ( i < argc - 1)
            ss << ' ';
    }

    std::cout << std::fixed << std::setprecision(2);

    // Read the parameters from the string stream
    while(!ss.eof())
    {
        try {
            figures.emplace_back(parse_args(ss));
            auto& figure = figures.back();

            if (!figure)
                break;

            std::cout 
                << std::setw(16) << figure->name()
                << " --> obwód: " << figure->perimeter() 
                << ", pole: " << figure->area() << '\n';
        }
        catch (const std::invalid_argument& e) {
            // Failed
            std::cerr << e.what() << '\n';

            // Find another figure type
            find_type_token(ss);
        }
    }

    return 0;
}