// Purpose: Use user-defined initial guesses to find the roots of 
//          f(x) = x + 3*sin(2x) via Newton-Raphson iteration.
//
// Number of roots found: 4
//
// Approximate values of roots:
//     x = -2.61298
//     x = -1.91750
//     x =  1.91750
//     x =  2.61298
//
// Initial guesses, epsilon, root values, and iterations used:
//     Initial Guess   Epsilon    Root         No. of Iterations
//     -3.0            1.0e-6     -2.61298     5
//     -2.0            1.0e-6     -1.91750     4
//      2.0            1.0e-6      1.91750     4
//      3.0            1.0e-6      2.61298     5
//     -3.0            1.0e-5     -2.61298     5
//     -2.0            1.0e-5     -1.91750     4
//      2.0            1.0e-5      1.91750     4
//      3.0            1.0e-5      2.61298     5
//     -3.0            1.0e-4     -2.61298     4
//     -2.0            1.0e-4     -1.91750     3
//      2.0            1.0e-4      1.91750     3
//      3.0            1.0e-4      2.61298     4
//     -3.0            1.0e-3     -2.61298     4
//     -2.0            1.0e-3     -1.91750     3
//      2.0            1.0e-3      1.91750     3
//      3.0            1.0e-3      2.61298     4
//     -3.0            1.0e-2     -2.61301     3
//     -2.0            1.0e-2     -1.91740     2
//      2.0            1.0e-2      1.91740     2
//      3.0            1.0e-2      2.61301     3
//
// Observations: Looking at the different root values, I can see that 
//               using an epsilon between 1.0e-6 and 1.0e-3 and initial 
//               guesses of -3.0, -2.0, 2.0, and 3.0 all resulted in 
//               accurate root values.
//
// Note: Using WolframAlpha, the roots I found using this program were 
//       accurate to at least 5 decimal places.

#include <iostream>
#include <cmath>
#include <cstdlib>

int main() {
    // Variable dictionary
    double xold{0.0}, xnew{0.0};       // Old and new estimates for the root
    double epsilon{0.0};               // Convergence tolerance
    int niter{1};                      // Iteration counter
    const int NITER_MAX{300};          // Max iterations allowed
    double f_x, fprime_x;              // Function and derivative

    // Asking the user for initial guess and epsilon values
    std::cout << "Enter initial guess: ";
    std::cin >> xold;
    std::cout << "Enter the convergence tolerance (epsilon): ";
    std::cin >> epsilon;

    // Newton-Raphson iteration loop
    while (niter <= NITER_MAX) {
        
        // Calculating f(x) and f'(x)
        f_x = xold + 3.0 * std::sin(2.0 * xold);
        fprime_x = 1.0 + 6.0 * std::cos(2.0 * xold);

        // Checking for near-zero derivative to avoid division by zero
        if (std::abs(fprime_x) < 1.0e-10) {
            std::cout << "Error: derivative too close to zero." << std::endl;
            return 2;
        }

        // Computing the new estimate
        xnew = xold - f_x / fprime_x;

        // Checking for convergence
        double scale = (std::abs(xold) > 0.0 ? std::abs(xold) : 1.0);
        if (std::abs(xnew - xold) < epsilon * scale) {
            std::cout << "Root: x = " << xnew << std::endl;
            std::cout << "Iterations: " << niter << std::endl;
            return 1;
        }

        // Update for next iteration
        xold = xnew;
        niter++;
    }

    // If maximum iterations reached, then the method will fail
    std::cout << "Method failed: too many iterations." << std::endl;

    return 0;
}
