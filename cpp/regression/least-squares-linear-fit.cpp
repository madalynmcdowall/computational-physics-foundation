// Purpose: Fit a line to data points using least-squares method
// Note: My input file, "input.dat" contained 25 data points.
//       My output file,"output.dat", contains the original x
//       and y values along with the fitted y values.

#include <iostream>
#include <fstream>
#include <string>

int main() {
    // Variable Dictionary
    std::ifstream infile;                        // Input file stream
    std::ofstream outfile;                       // Output file stream
    std::string input_file_name, output_file_name; // File names
    double x, y;                                 // Data point coordinates
    double *x_array{nullptr}, *y_array{nullptr}; // The x and y dynamic arrays
    int num_pnts{0};                             // Number of data points
    double sum_x{0.0}, sum_y{0.0};               // Sums of x and y values
    double sum_xx{0.0}, sum_xy{0.0};             // Sums of x^2 and x*y
    double m, b, chi_squared{0.0};               // Slope, intercept, chi-sq
    double temp_x, temp_y;                       // Temp. for swapping
    double n;                                    // Num. of points as double
    double diff;                                 // Diff. actual and fit
    int i, j;                                    // Loop counter variables

    // Prompting the user for the input and output file names
    std::cout << "Enter the name of the input file: ";
    std::cin >> input_file_name;
    std::cout << "Enter the name of the output file: ";
    std::cin >> output_file_name;

    // Counting the number of data points in the input file
    infile.open(input_file_name);
    if (infile.fail()) {
        std::cout << "Error opening input file." << std::endl;
        return 4;
    }

    while(infile.good()) {
        infile >> x >> y;
        if(infile.fail()) {
            break;
        }
        num_pnts++;
    }

    if (num_pnts == 0) {
        std::cout << "No data points found in file." << std::endl;
        infile.close();
        return 3;
    }

    infile.clear();
    infile.seekg(0, std::ios::beg);

    // Allocating memory for data points and reading them from the file
    x_array = new double[num_pnts];
    y_array = new double[num_pnts];

    for(i = 0; i < num_pnts; i++) {
        if (!(infile >> x_array[i] >> y_array[i])) {
            std::cout << "Error reading data." << std::endl;
            delete[] x_array;
            delete[] y_array;
            infile.close();
            return 2;
        }
    }
    infile.close();

    // Using Bubble Sort to sort the data points
    for (j = 1; j < num_pnts; j++) {
        for (i = 0; i < num_pnts - 1; i++) {
            if (x_array[i] > x_array[i + 1]) {
                // Swap x
                temp_x = x_array[i];
                x_array[i] = x_array[i + 1];
                x_array[i + 1] = temp_x;
                // Swap y
                temp_y = y_array[i];
                y_array[i] = y_array[i + 1];
                y_array[i + 1] = temp_y;
            }
        }
    }

    // Calculating sums needed for least squares
    for (i = 0; i < num_pnts; i++) {
        sum_x += x_array[i];
        sum_y += y_array[i];
        sum_xx += x_array[i] * x_array[i];
        sum_xy += x_array[i] * y_array[i];
    }

    // The least-squares formulas for the slope and intercept
    n = static_cast<double>(num_pnts);
    m = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
    b = (sum_y - m * sum_x) / n;

    // Calculating chi-squared
    for (i = 0; i < num_pnts; i++) {
        diff = y_array[i] - (m * x_array[i] + b);
        chi_squared += diff * diff;
    }

    // Output results to console
    std::cout << "Slope = " << m << std::endl;
    std::cout << "Intercept = " << b << std::endl;
    std::cout << "Chi-squared = " << chi_squared << std::endl;

    // Writing results to the output file
    outfile.open(output_file_name);
    if (outfile.fail()) {
        std::cout << "Error opening output file." << std::endl;
        delete[] x_array;
        delete[] y_array;
        return 1;
    }

    for (i = 0; i < num_pnts; i++) {
        outfile << x_array[i] << " " << y_array[i] << " " << \
                   m * x_array[i] + b << std::endl;
    }
    outfile.close();

    // Cleaning up the dynamically allocated memory
    delete[] x_array;
    delete[] y_array;

    return 0;
}
