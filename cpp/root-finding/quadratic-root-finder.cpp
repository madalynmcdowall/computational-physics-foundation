// Purpose: Find roots of a quadratic polynomial using the
// quadratic formula with user given A, B, and C coefficients.

#include <iostream>
#include <cmath>

int main() {
  // Variable Dictionary
  double a, b, c;              // Coefficients
  double discriminant;         // The discriminant (b^2 - 4ac)
  double root1, root2;         // The roots
  double real_part, imag_part; // Real and imag. parts of complex roots

  // Prompting the user for their coefficient values
  std::cout << "Please enter the values of A, B, and C: ";
  std::cin >> a >> b >> c;

  if(a == 0.0) {
    // If A=0, the equation is linear (bx + c = 0)
    root1 = -c/b;
    
    // Outputting the root
    std::cout << "Linear equation with one root: x = " << root1 << std::endl;
  }
  else {
    // Calculating the discriminant
    discriminant = b*b - 4.0*a*c;

    if(discriminant > 0.0) {
      // If discriminant > 0 then there are two distinct real roots
      root1 = (-b + sqrt(discriminant))/(2.0*a);
      root2 = (-b - sqrt(discriminant))/(2.0*a);
      
      // Outputting the two real roots
      std::cout << "This results in two real roots:" << std::endl;
      std::cout << " x = " << root1 << " and x = " << root2 << std::endl;
    }
    else if(discriminant == 0.0) {
      // If discriminant = 0 then there is one repeated real root
      root1 = -b/(2.0*a);
     
      // Outputting the repeated root
      std::cout << "This results in one repeated root: x = " << \
	           root1 << std::endl;
    }
    else {
      // If discriminant < 0 then there are two complex conjugate roots
      real_part = -b/(2.0*a);
      imag_part = sqrt(-discriminant)/(2.0*a);
      
      // Outputting the complex conjugate roots
      std::cout << "This results in two complex roots:" << std::endl;
      std::cout << "x = " << real_part << " + " << imag_part << \
	           "i" << std::endl;
      std::cout << "x = " << real_part << " - " << imag_part << \
	           "i" << std::endl;
    }
  }
  return 0;
}
