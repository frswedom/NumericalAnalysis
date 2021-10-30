#include <iostream>
#include <cmath>

using namespace std;

double f2(double x, double y)
{
	return 1 - (x - 1) * sin(y) + 2 * (x + y);
}

void PredictorCorrectorMethod( double (*f)(double, double), double minValue, double maxValue, double initialValue)

{
	const int steps = 20;
	const int init_step = 4;

	double y[steps + 1];
	double x[steps + 1];

	double h = (maxValue - minValue) / steps;
	x[0] = minValue;
	x[steps] = maxValue;
	y[0] = initialValue;

	// first 4 values
	for (int i = 0; i < init_step; i++)
	{
		x[i] = x[0] + i * h;
		double k1 = h * f(x[i], y[i]);
		double k2 = h * f(x[i] + h / 2, y[i] + k1 / 2);
		double k3 = h * f(x[i] + h / 2, y[i] + k2 / 2);
		double k4 = h * f(x[i] + h, y[i] + k3);

		y[i + 1] = y[i] + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
	}



	double f_3 = f(x[init_step - 3], y[init_step - 3]);
	double f_2 = f(x[init_step - 2], y[init_step - 2]);
	double f_1 = f(x[init_step - 1], y[init_step - 1]);
	double f_0 = f(x[init_step], y[init_step]);
	double f_corrector;
	double tmp_y;
	for (int i = init_step; i < steps; i++)
	{
		x[i] = x[0] + i * h;
		tmp_y = y[i] + h * (55 * f_0 - 59 * f_1 + 37 * f_2 - 9 * f_3) / 24;
		f_corrector = f(x[i + 1], tmp_y);
		y[i + 1] = y[i] + h * (9 * f_corrector + 19 * f_0 - 5 * f_1 + f_2) / 24;
		f_3 = f_2;
		f_2 = f_1;
		f_1 = f_0;
		f_0 = f(x[i] + h, y[i + 1]);
	}
	cout << "PredictorCorrectorMethod\n";
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
	PredictorCorrectorMethod(f2, 0.0, 1.0, 1.0);
	return 0;
}
