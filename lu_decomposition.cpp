#include <iostream>
using namespace std;

int main()
{
	
	const int size = 4;
	const double a[size][size]{ 
	{ 8.2, -3.2, 14.2, 14.8 },
	{ 5.6, -12.0, 15.0, -6.4 },
	{ 5.7, 3.6, -12.4, -2.3 },
	{ 6.8, 13.2, -6.3, -8.7} };
	const double b[size] {-8.4, 4.5, 3.3, 14.3};
	
	//LU decomposition
	double l[size][size]{}, u[size][size]{};
	for (int i = 0; i < size; u[i][i++] = 1);
	for (int k = 0; k < size; ++k)
	{
		for (int i = k; i < size; ++i)
		{
			l[i][k] = a[i][k];
			for (int q = 0; q < k; ++q)
			{
				l[i][k] -= l[i][q] * u[q][k];
			}
		}
		for (int j = k + 1; j < size; ++j)
		{
			u[k][j] = a[k][j];
			for (int q = 0; q < k; ++q)
			{
				u[k][j] -= l[k][q] * u[q][j];
			}
			u[k][j] /= l[k][k];
		}
	}
	//determinant
	double det = 1.0;
	for (int i = 0; i < size; ++i)
		det *= l[i][i];
	//solve the system of equations
	double y[size]{}, x[size]{};
	for (int i = 0; i < size; ++i)
	{
		y[i] = b[i];
		for (int j = 0; j < i; ++j)
			y[i] -= l[i][j] * y[j];
		y[i] /= l[i][i];
	}
	for (int i = size - 1; i >= 0; --i)
	{
		x[i] = y[i];
		for (int j = i + 1; j < size; ++j)
			x[i] -= u[i][j] * x[j];
	}
	//inverse matrix
	double inverse[size][size]{};
	for (int k = 0; k < size; ++k)
	{
		double y[size]{};
		for (int i = 0; i < size; ++i)
		{
			y[i] = ((i==k) ? 1 : 0);
			for (int j = 0; j < i; ++j)
				y[i] -= l[i][j] * y[j];
			y[i] /= l[i][i];
		}
		for (int i = size - 1; i >= 0; --i)
		{
			inverse[i][k] = y[i];
			for (int j = i + 1; j < size; ++j)
				inverse[i][k] -= u[i][j] * inverse[j][k];
		}
	}
	cout.flags(ios::fixed);
	cout.precision(10);
	//print L
	cout << "L:" << endl;
	for (int i = 0; i < size; ++i)
	{
		for (int j = 0; j < size; ++j)
			cout << l[i][j] << "\t";
		cout << endl;
	}
	//print U
	cout << "U:" << endl;
	for (int i = 0; i < size; ++i)
	{
		for (int j = 0; j < size; ++j)
			cout << u[i][j] << "\t";
		cout << endl;
	}
	cout << "det: " << det << endl;
	//print Y
	cout << "Y: " << endl;
	for (int i = 0; i < size; ++i)
	{
		cout << y[i] << ' ';
	}
	//print X
	cout << endl << "X: " << endl;
	for (int i = 0; i < size; ++i)
	{
		cout << x[i] << ' ';
	}
	//print inverse matrix
	cout << endl << "Inverse matrix: " << endl;
	for (int i = 0; i < size; ++i)
	{
		for (int j = 0; j < size; ++j)
			cout << inverse[i][j] << '\t';
		cout << endl;
	}
	return 0;
}

