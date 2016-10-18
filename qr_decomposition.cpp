#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

using namespace std;
using array = vector < double >;
using matrix = vector < array >;

matrix operator*(matrix lhs, matrix rhs)
{
	if (lhs[0].size() != rhs.size())
		return matrix();
	matrix ans(lhs.size(), array(rhs[0].size(), 0.0));
	for (int i = 0; i < ans.size(); ++i)
	{
		for (int j = 0; j < ans[0].size(); ++j)
		{
			for (int k = 0; k < rhs.size(); ++k)
			{
				ans[i][j] += lhs[i][k] * rhs[k][j];
			}
		}
	}
	return ans;
}

array operator*(matrix lhs, array rhs)
{
	if (lhs[0].size() != rhs.size())
		return array();
	array ans(lhs.size(), 0.0);
	for (int i = 0; i < ans.size(); ++i)
	{
		for (int j = 0; j < rhs.size(); ++j)
		{
			ans[i] += lhs[i][j] * rhs[j];
		}
	}
	return ans;
}

void print(matrix m)
{
	cout.precision(8);
	for (int i = 0; i < m.size(); i++)
	{
		for (int j = 0; j < m[0].size(); j++)
			cout << m[i][j] << ' ';
		cout << endl;
	}
	cout << endl;
	return;
}

void print(array m)
{
	cout.precision(8);
	for (int i = 0; i < m.size(); i++)
	{
		cout << m[i] << ' ';
	}
	cout << endl;
	return;
}

int main()
{
	int n = 4;
	matrix A = { { 15.7, 6.6, -5.7, 11.5 },
	{ 8.8, -6.7, 5.5, -4.5 },
	{ 6.3, -5.7, -23.4, 6.6 },
	{ 14.3, 8.7, -15.7, -5.8 } };
	array b = {-2.4, 5.6, 7.7, 23.4};
	matrix Q(n, array(n, 0.0)); // Q is actually Q_T
	matrix R = A;
	for (int i = 0; i < n; i++) // set Q as identity matrix
		Q[i][i] = 1.0;

	for (int g = 0; g < (n - 1); g++)
	{
		matrix P(n, array(n, 0.0));
		for (int i = 0; i < n; i++) // set P as identity matrix
			P[i][i] = 1.0;
		array u(n - g);
		for (int i = g; i < n; i++)
			u[i - g] = R[i][g];
		double magn = 0.0;
		for (int i = 0; i < u.size(); i++)
			magn += u[i] * u[i];
		u[0] += sqrt(magn)* ((R[g][g] > 0) ? 1.0 : -1.0);

		magn = 0.0;
		for (int i = 0; i < u.size(); i++)
			magn += u[i] * u[i];

		for (int i = g; i < n; i++)
		{
			for (int j = g; j < n; j++)
			{
				P[i][j] -= u[i - g] * u[j - g] * 2.0 / magn;
			}
		}
		R = P * R;
		Q = P * Q;
	}
	
	cout << "Q_T: \n";
	print(Q);
	cout << "R: \n";
	print(R);
	
	//determinant
	double eps;
	cout << "enter eps: ";
	cin >> eps;
	double det = (n % 2) ? 1.0 : -1.0;
	for (int i = 0; i < n; i++)
		det *= R[i][i];
	cout << "Determinant: ";
	cout << det << "\n";
	if (abs(det) < eps)
	{
		cout << "Error\n";
		return 0;
	}

	//Solve system
	array u = Q * b;
	array x(n);
	for (int i = n - 1; i >= 0; --i)
	{
		x[i] = u[i];
		for (int j = i + 1; j < n; ++j)
			x[i] -= R[i][j] * x[j];
		x[i] /= R[i][i];
	}
	cout << "\nSolve Ax=b: \n";
	print(x);

	//Find inverse matrix
	matrix inv(n, array(n));
	for (int i = n - 1; i >= 0; --i)
	{
		inv[i] = Q[i];
		for (int j = i + 1; j < n; ++j)
			for (int k = 0; k < n; ++k)
				inv[i][k] -= R[i][j] * inv[j][k];

		for (int j = 0; j < n; j++)
			inv[i][j] /= R[i][i];
	}
	cout << "\nInverse matrix: \n";
	print(inv);

	return 0;
}