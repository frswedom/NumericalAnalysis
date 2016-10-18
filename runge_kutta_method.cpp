#include <iostream>
#include <cmath>

using namespace std;

double f1(double x, double y)
{
	return -(3*x*x*y*y + 7) / (2*y);
}

void RungeKuttaMethod( double (*f)(double, double), double minValue, double maxValue, double initialValue)
{
	const int steps = 20;
	
	double y[steps + 1];
	double x[steps + 1];
	
	double h = (maxValue - minValue) / steps;
	x[0] = minValue;
	x[steps] = maxValue;
	y[0] = initialValue;
	for (int i = 0; i < steps; i++)
	{
		x[i] = x[0] + i*h;
		double k1 = h*f(x[i], y[i]);
		double k2 = h*f(x[i] + h / 2, y[i] + k1 / 2);
		double k3 = h*f(x[i] + h / 2, y[i] + k2 / 2);
		double k4 = h*f(x[i] + h, y[i] + k3);

		y[i + 1] = y[i] + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
	}
	cout << "RungeKuttaMethod\n";
	for (int i = 0; i < steps; i++)
	{
		cout << x[i] << ", ";
	}
	cout << x[steps];
	cout << '\n';
	for (int i = 0; i < steps; i++)
	{
		cout << y[i] << ", ";
	}
	cout << y[steps];
	cout << '\n';
	return;
}
int main()
{
	cout.precision(16);
	RungeKuttaMethod(f1, 0.0, 1.0, 1.0);
	return 0;
}
